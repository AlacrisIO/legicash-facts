(* WARNING: We use GLOBAL STATE for our mailboxes,
   so there's only one facilitator running in a given process.
   I blame OCaml for lack of dynamic binding. Yay Common Lisp special variables and Scheme parameters! *)
open Lens.Infix

open Legilogic_lib
open Lib
open Action
open Yojsoning
open Marshaling
open Persisting
open Signing
open Types
open Merkle_trie

open Legilogic_ethereum

open Side_chain

exception Facilitator_not_found of string

module FacilitatorState = struct
  [@warning "-39"]
  type t = { keypair: Keypair.t
           ; committed: State.t signed
           ; current: State.t
           ; fee_schedule: FacilitatorFeeSchedule.t }
  [@@deriving lens { prefix=true}, yojson]

  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling4
        (fun { keypair; committed; current ; fee_schedule } ->
           keypair.address, committed, current, fee_schedule)
        (fun address committed current fee_schedule ->
           { keypair= keypair_of_address address; committed; current ; fee_schedule })
        Address.marshaling (signed_marshaling State.marshaling)
        State.marshaling FacilitatorFeeSchedule.marshaling
    let walk_dependencies _methods context {committed; current} =
      let open Lwt in
      walk_dependency SignedState.dependency_walking context committed
      >>= fun () -> walk_dependency State.dependency_walking context current
    let make_persistent = normal_persistent
    let yojsoning = {to_yojson;of_yojson}
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
  (** TODO: somehow only save the current/committed state and the fee schedule *)
  let facilitator_state_key facilitator_address =
    "LCFS0001" ^ (Address.to_big_endian_bits facilitator_address)
  let save facilitator_state =
    let open Lwt in
    save facilitator_state (* <-- use inherited binding *)
    >>= fun () ->
    let address = facilitator_state.keypair.address in
    let key = facilitator_state_key address in
    Db.put key (Digest.to_big_endian_bits (digest facilitator_state))
  let load facilitator_address =
    facilitator_address |> facilitator_state_key |> Db.get
    |> (function
      | Some x -> x
      | None -> raise (Facilitator_not_found
                         (Printf.sprintf "Facilitator %s not found in the database"
                            (Address.to_0x facilitator_address))))
    |> Digest.unmarshal_string |> db_value_of_digest unmarshal_string
end

(* TODO:
   divide requests in multiple kinds:
 * user query, which looks at some existing account without modification
 * user transaction request, which modifies their account
 * system transaction, which e.g. posts

   The side-chain has three different (kind of) states:
 * current, the facilitator's view of itself
 * pending to the main chain, but not yet passed the challenge period
   (there can be multiple such states at various stages of advancement,
   e.g. posted on the main chain but not yet in a main chain block,
   in a main chain block but not yet considered final,
   in a final-enough main chain block, still unchallenged but not yet past the challenge period
   in a final-enough main chain block and challenged, trial still open.
 * confirmed on the main chain
 * old enough that it doesn't directly matter to the contract anymore.
*)

module FacilitatorAction = Action(FacilitatorState)
module FacilitatorAsyncAction = AsyncAction(FacilitatorState)

type account_lens = (FacilitatorState.t, AccountState.t) Lens.t

type validated_transaction_request =
  [ `Confirm of TransactionRequest.t * (Transaction.t * unit Lwt.t) or_exn Lwt.u ]

type inner_transaction_request =
  [ validated_transaction_request
  | `Flush of int
  | `Committed of (State.t signed * unit Lwt.u) ]

let inner_transaction_request_mailbox : inner_transaction_request Lwt_mvar.t = Lwt_mvar.create_empty ()

type facilitator_service =
  { address : Address.t
  ; state_ref : FacilitatorState.t ref }

let the_facilitator_service_ref : (facilitator_service option ref) = ref None

let get_facilitator_state () : FacilitatorState.t =
  !the_facilitator_service_ref
  |> (function Some x -> x | None -> bork "Facilitator service not started")
  |> fun service -> !(service.state_ref)

let facilitator_account_lens address =
  FacilitatorState.lens_current |-- State.lens_accounts
  |-- defaulting_lens (konstant AccountState.empty) (AccountMap.lens address)

let signed_request_requester rx = rx.payload.UserTransactionRequest.rx_header.requester

exception Malformed_request of string

(** Check that the request is basically well-formed, or else fail
    This function should include all checks that can be made without any non-local side-effect
    beside reading pure or monotonic data, which is allowed for now
    (but may later have to be split to another function).
    Thus, we can later parallelize this check.
    TODO: parallelize the signature checking in a C worker thread that lets us do additional OCaml work.
*)

let validate_user_transaction_request :
  (UserTransactionRequest.t signed * bool, TransactionRequest.t) Lwt_exn.arr =
  fun (signed_request, is_forced) ->
    let {payload=UserTransactionRequest.{ rx_header={ requester; requester_revision }; operation }} =
      signed_request in
    let state = get_facilitator_state () in
    let AccountState.{balance; account_revision} = (facilitator_account_lens requester).get state in
    let FacilitatorState.{fee_schedule} = state in
    let open Lwt_exn in
    let check test exngen =
      fun x ->
        if test then return x
        else fail (Malformed_request (exngen ())) in
    `UserTransaction signed_request
    |> ((* XXX THE REVISION CHECK BELOW IS TEMPORARILY DISABLED FOR DEMO PURPOSES,
           UNTIL WE IMPROVE THE SIDE CHAIN USER SOFTWARE
           TODO - Fix the side_chain_user and endpoints/actions code, then re-enable this check. *)
      let open Revision in
      check (requester_revision = (add account_revision one) || true) (* <-- TODO: REMOVE the || true *)
        (fun () ->
           Printf.sprintf "You made a request with revision %s but the next expected revision is %s"
             (to_string requester_revision) (to_string (add account_revision one)))
      >>> check (is_signed_value_valid UserTransactionRequest.digest requester signed_request)
            (konstant "The signature for the request doesn't match the requester")
      (* TODO: check confirmed main & side chain state + validity window *)
      >>> (* Check that the numbers add up: *)
      let open TokenAmount in
      match operation with
      | Deposit
          { deposit_amount
          ; deposit_fee
          ; main_chain_deposit={tx_header= {value}} as main_chain_deposit
          ; main_chain_deposit_confirmation } ->
        check (is_sum value deposit_amount deposit_fee)
          (fun () ->
             Printf.sprintf "Deposit amount %s and fee %s fail to add up to deposit value %s"
               (to_string deposit_amount) (to_string deposit_fee) (to_string value))
        >>> check (is_forced || compare deposit_fee fee_schedule.deposit_fee >= 0)
              (fun () -> Printf.sprintf "Insufficient deposit fee %s, requiring at least %s"
                           (to_string deposit_fee) (to_string fee_schedule.deposit_fee))
        (* TODO: CHECK FROM THE CONFIRMATION THAT THE CORRECT PERSON DID THE DEPOSIT,
           AND/OR THAT IT   WAS TAGGED WITH THE CORRECT RECIPIENT. *)
        >>> check (Ethereum_chain.is_confirmation_valid
                     main_chain_deposit_confirmation main_chain_deposit)
              (fun () -> "The main chain deposit confirmation is invalid")
      | Payment {payment_invoice; payment_fee; payment_expedited=_payment_expedited} ->
        check (payment_invoice.recipient != requester)
          (fun () -> "Recipient same as requester")
        >>> check (is_add_valid payment_invoice.amount payment_fee)
              (fun () -> "Adding payment amount and fee causes an overflow!")
        >>> check (compare balance (add payment_invoice.amount payment_fee) >= 0)
              (fun () ->
                 Printf.sprintf "Balance %s insufficient to cover payment amount %s plus fee %s"
                   (to_string balance) (to_string payment_invoice.amount) (to_string payment_fee))
        (* TODO: make per_account_limit work on the entire floating thing *)
        >>> check (compare fee_schedule.per_account_limit payment_invoice.amount >= 0)
              (fun () ->
                 Printf.sprintf "Payment amount %s is larger than authorized limit %s"
                   (to_string payment_invoice.amount) (to_string fee_schedule.per_account_limit))
        >>> check (is_forced ||
                   is_mul_valid state.fee_schedule.fee_per_billion payment_invoice.amount)
              (fun () ->
                 Printf.sprintf
                   "Payment fee calculation overflows with amount %s, scheduled fee per billion %s"
                   (to_string payment_invoice.amount) (to_string fee_schedule.fee_per_billion))
        >>> check (is_forced ||
                   compare payment_fee
                     (div (mul state.fee_schedule.fee_per_billion payment_invoice.amount)
                        one_billion_tokens)
                   >= 0)
              (fun () ->
                 Printf.sprintf
                   "Insufficient payment fee %s when at least %s were expected"
                   (to_string payment_fee)
                   (to_string (div (mul state.fee_schedule.fee_per_billion payment_invoice.amount)
                                 one_billion_tokens)))
      | Withdrawal {withdrawal_amount; withdrawal_fee} ->
        check (is_add_valid withdrawal_amount withdrawal_fee)
          (fun () -> "Adding withdrawal amount and fee causes an overflow!")
        >>> check (compare balance (add withdrawal_amount withdrawal_fee) >= 0)
              (fun () ->
                 Printf.sprintf "Balance %s insufficient to cover withdrawal amount %s plus fee %s"
                   (to_string balance) (to_string withdrawal_amount) (to_string withdrawal_fee))
        >>> check (is_forced || compare withdrawal_fee fee_schedule.withdrawal_fee >= 0)
              (fun () -> Printf.sprintf "Insufficient withdrawal fee %s, requiring at least %s"
                           (to_string withdrawal_fee) (to_string fee_schedule.withdrawal_fee)))

let make_user_transaction :
  (UserTransactionRequest.t signed, Transaction.t) FacilitatorAction.arr =
  fun signed_request facilitator_state ->
    let current_state = facilitator_state.current in
    let new_revision = Revision.(add current_state.facilitator_revision one) in
    let transaction =
      Transaction.
        { tx_header=TxHeader.{ tx_revision= new_revision
                             ; updated_limit= facilitator_state.current.spending_limit }
        ; tx_request=`UserTransaction signed_request } in
    let requester = signed_request_requester signed_request in
    let account_lens = facilitator_account_lens requester in
    let new_requester_revision = signed_request.payload.rx_header.requester_revision in
    let new_facilitator_state =
      facilitator_state
      |> (account_lens |-- AccountState.lens_account_revision).set new_requester_revision
      |> (FacilitatorState.lens_current |-- State.lens_facilitator_revision).set new_revision
      |> Lens.modify (FacilitatorState.lens_current |-- State.lens_transactions)
           (TransactionMap.add new_revision transaction) in
    FacilitatorAction.return transaction new_facilitator_state

let modify_guarded_state guard modification lens failure success state =
  if guard (lens.Lens.get state) then
    Ok success, Lens.modify lens modification state
  else
    Error (failure state), state

let decrement_state_tokens amount =
  modify_guarded_state
    (fun x -> TokenAmount.compare x amount >= 0)
    (fun x -> TokenAmount.sub x amount)

(** Facilitator actions to use up some of the limit *)
exception Spending_limit_exceeded

let spend_spending_limit (amount : TokenAmount.t) : ('a, 'a) FacilitatorAction.arr =
  decrement_state_tokens
    amount
    (FacilitatorState.lens_current |-- State.lens_spending_limit)
    (fun _ -> Spending_limit_exceeded)

let maybe_spend_spending_limit
      (is_expedited : bool) (amount: TokenAmount.t) : ('a, 'a) FacilitatorAction.arr =
  if is_expedited then spend_spending_limit amount else FacilitatorAction.return

exception Already_posted

exception Insufficient_balance of string

(* To prevent double-deposit or double-withdrawal of a same main_chain_transaction_signed,
   we put those transactions in a set of already posted transactions.
   (Future: prune that set by expiring deposit requests?
   Have more expensive process to account for old deposits?)
*)
let check_against_double_accounting
      (main_chain_transaction : Ethereum_chain.Transaction.t)
  : ('a, 'a) FacilitatorAction.arr =
  let witness = Ethereum_chain.Transaction.digest main_chain_transaction in
  let lens =
    FacilitatorState.lens_current
    |-- State.lens_main_chain_transactions_posted
    |-- DigestSet.lens witness in
  modify_guarded_state not (konstant true) lens (fun _ -> Already_posted)

let credit_balance (amount : TokenAmount.t) (account_address : Address.t)
  : ('a, 'a) FacilitatorAction.arr =
  FacilitatorAction.map_state
    (Lens.modify (facilitator_account_lens account_address |-- AccountState.lens_balance)
       (TokenAmount.add amount))

let debit_balance amount account_address =
  let lens = facilitator_account_lens account_address |-- AccountState.lens_balance in
  decrement_state_tokens
    amount lens
    (fun state ->
       Insufficient_balance
         (Printf.sprintf "Account %s has insufficient balance %s to debit transaction value %s"
            (Address.to_0x account_address) (TokenAmount.to_string (lens.get state))
            (TokenAmount.to_string amount)))

let accept_fee fee : ('a, 'a) FacilitatorAction.arr =
  fun x state -> credit_balance fee state.FacilitatorState.keypair.address x state

(** compute the effects of a request on the account state *)
let effect_validated_user_transaction_request :
  ( UserTransactionRequest.t signed, UserTransactionRequest.t signed) FacilitatorAction.arr =
  fun rx ->
    let open FacilitatorAction in
    let requester = signed_request_requester rx in
    rx
    |> match rx.payload.operation with
    | Deposit { deposit_amount; deposit_fee; main_chain_deposit } ->
      check_against_double_accounting main_chain_deposit
      >>> credit_balance deposit_amount requester
      >>> accept_fee deposit_fee
    | Payment {payment_invoice; payment_fee; payment_expedited} ->
      maybe_spend_spending_limit payment_expedited payment_invoice.amount
      >>> debit_balance (TokenAmount.add payment_invoice.amount payment_fee) requester
      >>> credit_balance payment_invoice.amount payment_invoice.recipient
      >>> accept_fee payment_fee
    | Withdrawal {withdrawal_amount; withdrawal_fee} ->
      debit_balance (TokenAmount.add withdrawal_amount withdrawal_fee) requester
      >>> accept_fee withdrawal_fee


(** TODO: have a server do all the effect_requests sequentially,
    after they have been validated in parallel (well, except that Lwt is really single-threaded *)
let post_validated_transaction_request :
  ( TransactionRequest.t, Transaction.t * unit Lwt.t) Lwt_exn.arr =
  simple_client
    inner_transaction_request_mailbox
    (fun (request, resolver) -> `Confirm (request, resolver))

let process_validated_transaction_request : (TransactionRequest.t, Transaction.t) FacilitatorAction.arr =
  function
  | `UserTransaction request ->
    request |> FacilitatorAction.(effect_validated_user_transaction_request >>> make_user_transaction)
  | `AdminTransaction _ ->
    bottom () (* TODO: do it *)

let make_transaction_commitment : Transaction.t -> TransactionCommitment.t =
  fun transaction ->
    let FacilitatorState.{committed} = get_facilitator_state () in
    let State.{ facilitator_revision
              ; spending_limit
              ; accounts
              ; transactions
              ; main_chain_transactions_posted } = committed.payload in
    let accounts = dv_digest accounts in
    let signature = committed.signature in
    let main_chain_transactions_posted = dv_digest main_chain_transactions_posted in
    let revision = transaction.tx_header.tx_revision in
    match TransactionMap.Proof.get revision transactions with
    | Some tx_proof ->
      TransactionCommitment.
        { transaction; tx_proof; facilitator_revision; spending_limit;
          accounts; main_chain_transactions_posted; signature }
    | None -> bork "Transaction %s not found, cannot build commitment!" (Revision.to_0x revision)

(* Process a user request, with a flag to specify whether it's a forced request
   (published on the main chain), in which case there are no fee amount minima.

   We use the latest current state of the facilitator exported by the inner loop, but read-only,
   to check that the request is valid.
   If it is, we pass it to the inner loop, that will use it read-write.
   In the future, maybe moving away from Lwt and from single-threaded OCaml,
   and/or using forking and reducing the use of facilitator state so no DB access is needed,
   this could be done in different threads or processes.
*)
let process_user_transaction_request :
  (UserTransactionRequest.t signed * bool, TransactionCommitment.t) Lwt_exn.arr =
  let open Lwt_exn in
  validate_user_transaction_request
  >>> post_validated_transaction_request
  >>> fun (transaction, wait_for_commit) ->
  let open Lwt in
  wait_for_commit
  >>= fun () ->
  make_transaction_commitment transaction |> Lwt_exn.return

(** This is a placeholder until we separate client and server in separate processes *)
let post_user_transaction_request request =
  (*stateless_parallelize*) process_user_transaction_request (request, false)

type main_chain_account_state =
  { address : Address.t
  ; balance : TokenAmount.t
  ; revision : Revision.t
  }
[@@deriving to_yojson]

(* TODO : put in a module of JSON utilities ? *)
let error_json fmt =
  Printf.ksprintf (fun x -> `Assoc [("error",`String x)]) fmt

let get_account_balance address (facilitator_state:FacilitatorState.t) =
  try
    let account_state = AccountMap.find address facilitator_state.current.accounts in
    `Assoc [("address",Address.to_yojson address)
           ;("account_balance",TokenAmount.to_yojson account_state.balance)]
  with Not_found ->
    error_json "Could not find balance for address %s" (Address.to_0x address)


let get_account_balances (facilitator_state:FacilitatorState.t) =
  let pair_to_yojson ((address, state): (Address.t * AccountState.t)) =
    Address.to_0x address, AccountState.to_yojson state in
  `Assoc (AccountMap.bindings facilitator_state.current.accounts
          |> List.filter (fst >> ((<>) Test.trent_address)) (* Exclude Trent *)
          |> List.map pair_to_yojson)

let get_account_state address (facilitator_state:FacilitatorState.t) =
  try
    AccountMap.find address facilitator_state.current.accounts
    |> fun acct_state ->
    `Assoc [("address",Address.to_yojson address)
           ;("account_state",AccountState.to_yojson acct_state)
           ]
  with Not_found ->
    error_json "Could not find account state for account: %s" (Address.to_0x address)

let get_account_status address facilitator_state =
  let open Lwt_exn in
  let exception Failure_to_get_main_chain_balance of exn in
  let exception Failure_to_get_main_chain_transaction_count of exn in
  let side_chain_state = get_account_state address facilitator_state in
  trying Ethereum_json_rpc.eth_get_balance (address, Latest)
  >>= handling (fun e -> fail (Failure_to_get_main_chain_balance e))
  >>= fun balance ->
  trying Ethereum_json_rpc.eth_get_transaction_count (address, Latest)
  >>= handling (fun e -> fail (Failure_to_get_main_chain_transaction_count e))
  >>= fun revision ->
  let main_chain_account = { address; balance; revision } in
  return (`Assoc [("side_chain_account",side_chain_state)
                 ;("main_chain_account",main_chain_account_state_to_yojson main_chain_account)])

(* TODO: maintain per-account index of transactions, otherwise this won't scale!!! *)
let get_recent_transactions address maybe_limit facilitator_state =
  let all_transactions = facilitator_state.FacilitatorState.current.transactions in
  let get_operation_for_address _rev (transaction:Transaction.t) ((count, transactions) as accum) k =
    if (match maybe_limit with Some limit -> count >= limit | _ -> false) then
      transactions
    else
      match transaction.tx_request with
      | `UserTransaction rx ->
        let requester = rx.payload.rx_header.requester in
        let recipient : address option = match rx.payload.operation with
          | Payment details -> Some details.payment_invoice.recipient
          | _ -> None in
        if ((requester = address) || (recipient = Some address)) then
          k (Revision.(add one count), transaction::transactions)
        else
          k accum
      | _ -> k accum
  in
  `List (List.map Transaction.to_yojson
           (TransactionMap.foldrk get_operation_for_address all_transactions (Revision.zero,[]) snd))

let get_proof tx_revision (facilitator_state : FacilitatorState.t) =
  let transactions = facilitator_state.current.transactions in
  match TransactionMap.Proof.get tx_revision transactions with
  | None ->
    error_json "Cannot provide proof for tx-revision: %s" (Revision.to_string tx_revision)
  | Some proof -> TransactionMap.Proof.to_yojson proof

(** Take messages from the user_query_request_mailbox, and process them (TODO: in parallel?) *)
let process_user_query_request request =
  let open Lwt_exn in
  let state = get_facilitator_state () in
  (match (request : UserQueryRequest.t) with
   | Get_account_balance {address} ->
     get_account_balance address state |> return
   | Get_account_balances ->
     get_account_balances state |> return
   | Get_account_state {address} ->
     get_account_state address state |> return
   | Get_account_status {address} ->
     get_account_status address state
   | Get_recent_transactions { address; count } ->
     get_recent_transactions address count state |> return
   | Get_proof {tx_revision} ->
     get_proof tx_revision state |> return)

let post_user_query_request =
  (*stateless_parallelize*) process_user_query_request

(** Take messages from the admin_query_request_mailbox, and process them (TODO: in parallel?) *)
let process_admin_query_request = bottom
let post_admin_query_request =
  (*stateless_parallelize*) process_admin_query_request

(** We assume that the operation will correctly apply:
    balances are sufficient for spending,
    deposits confirmation will check out,
    active revision will only increase, etc.
*)

let increment_capped max x =
  if x < max then x + 1 else max

(* TODO: tweak these numbers later *)
let batch_timeout_trigger_in_seconds = 0.01
let batch_size_trigger_in_requests = 1000

let inner_transaction_request_loop =
  let open Lwter in
  fun facilitator_state_ref ->
    return (!facilitator_state_ref, 0, Lwt.return_unit)
    >>= forever
          (fun (facilitator_state, batch_id, previous) ->
             (* The promise sent back to requesters, that they have to wait on
                for their confirmation's batch to have been committed,
                and our private resolver for this batch. *)
             let (batch_committed, notify_batch_committed) = Lwt.task () in
             (* An internal promise to detect if and when we trigger the batch based on a timeout *)
             let (time_triggered, time_trigger) = Lwt.task () in
             (* An internal promise to detect if and when we trigger the batch based on time *)
             let (size_triggered, size_trigger) = Lwt.task () in
             (* When we are ready and either trigger criterion is met,
                send ourselves a Flush message for this batch_id *)
             Lwt.async (fun () -> Lwt.join [previous;Lwt.pick [time_triggered; size_triggered]]
                         >>= (fun () -> Lwt_mvar.put inner_transaction_request_mailbox (`Flush batch_id)));
             let rec request_batch facilitator_state size =
               Lwt_mvar.take inner_transaction_request_mailbox
               >>= function
               | `Confirm (request_signed, continuation) ->
                 process_validated_transaction_request request_signed facilitator_state
                 |> fun (confirmation_or_exn, new_facilitator_state) ->
                 facilitator_state_ref := new_facilitator_state;
                 (match confirmation_or_exn with
                  | Error e ->
                    Lwt.wakeup_later continuation (Error e);
                    request_batch new_facilitator_state size
                  | Ok confirmation ->
                    Lwt.wakeup_later continuation (Ok (confirmation, batch_committed));
                    let new_size = increment_capped max_int size in
                    if new_size = batch_size_trigger_in_requests then
                      (* Flush the data after enough entries are written *)
                      Lwt.wakeup_later size_trigger ()
                    else if new_size = 1 then
                      (* Start a timeout to trigger flushing, but only after some entry is written *)
                      Lwt.async (fun () -> Lwt_unix.sleep batch_timeout_trigger_in_seconds
                                  >>= fun () -> Lwt.wakeup_later time_trigger ();
                                  Lwt.return_unit);
                    request_batch new_facilitator_state new_size)
               | `Flush id ->
                 assert (id = batch_id);
                 if size > 0 then
                   (let (ready, notify_ready) = Lwt.task () in
                    let signed_state =
                      SignedState.make facilitator_state.keypair facilitator_state.current in
                    let facilitator_state_to_save =
                      FacilitatorState.lens_committed.set signed_state facilitator_state in
                    Lwt.async (fun () ->
                      ready
                      >>= fun () ->
                      Lwt_mvar.put inner_transaction_request_mailbox
                        (`Committed (signed_state, notify_batch_committed)));
                    FacilitatorState.save facilitator_state_to_save
                    >>= fun () -> Db.async_commit notify_ready
                    >>= fun () -> Lwt.return (facilitator_state, (batch_id + 1), batch_committed))
                 else
                   (Lwt.wakeup_later notify_batch_committed ();
                    Lwt.return (facilitator_state, (batch_id + 1), batch_committed))
               | `Committed (signed_state, previous_notify_batch_committed) ->
                 let new_facilitator_state =
                   FacilitatorState.lens_committed.set signed_state facilitator_state in
                 facilitator_state_ref := new_facilitator_state;
                 Lwt.wakeup_later previous_notify_batch_committed ();
                 request_batch new_facilitator_state size in
             request_batch facilitator_state 0)

let initial_side_chain_state =
  State.
    { facilitator_revision= Revision.of_int 0
    ; spending_limit= TokenAmount.of_int 0 (* TODO: have a way to ramp it up! *)
    ; accounts= AccountMap.empty
    ; transactions= TransactionMap.empty
    ; main_chain_transactions_posted= DigestSet.empty }

let initial_facilitator_state address =
  let keypair = keypair_of_address address in
  FacilitatorState.
    { keypair
    ; committed= SignedState.make keypair initial_side_chain_state
    ; current= initial_side_chain_state
    ; fee_schedule= initial_fee_schedule }

(* TODO: make it a PersistentActivity ? *)
let start_facilitator address =
  let open Lwt_exn in
  match !the_facilitator_service_ref with
  | Some x ->
    if Address.equal x.address address then
      (Logging.log "Facilitator service already running for address %s, not starting another one"
         (Address.to_0x address);
       return ())
    else
      bork "Cannot start a facilitator service for address %s because there's already one for %s"
        (Address.to_0x address) (Address.to_0x x.address)
  | None ->
    let facilitator_state =
      try
        FacilitatorState.load address
      with Not_found ->
        (* TODO: don't create a new facilitator unless explicitly requested? *)
        initial_facilitator_state address
    in
    let state_ref = ref facilitator_state in
    the_facilitator_service_ref := Some { address; state_ref };
    Lwt.async (const state_ref >>> inner_transaction_request_loop);
    Lwt_exn.return ()

module Test = struct
  open Signing.Test

  let get_facilitator_state = get_facilitator_state

  (* a sample facilitator state *)

  let%test "db-save-retrieve" =
    (* test whether retrieving a saved facilitator state yields the same state
       here, the account and confirmation maps are empty, so it doesn't really
       exercise the node-by-node persistence machinery
       in Side_chain_action.Test, the "deposit_and_payment_valid" test does
       a save and retrieval with nonempty such maps
    *)
    register_test_keypairs ();
    let open Lwt in
    Db.run ~db_name:"unit_test_db"
      (fun () ->
         let trent_state = initial_facilitator_state trent_address in
         FacilitatorState.save trent_state
         >>= Db.commit
         >>= fun () ->
         let retrieved_state = FacilitatorState.load trent_address in
         Lwt.return (FacilitatorState.to_yojson_string retrieved_state
                     = FacilitatorState.to_yojson_string trent_state))
end

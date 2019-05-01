(* WARNING: We use GLOBAL STATE for our mailboxes,
   so there's only one operator running in a given process.
   I blame OCaml for lack of dynamic binding.
   Yay Common Lisp special variables and Scheme parameters! *)
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
open State_update

open Legilogic_ethereum
open Side_chain_server_config
open Operator_contract
open Ethereum_json_rpc

open Side_chain

exception Operator_not_found of string
exception Malformed_request  of string

module OperatorState = struct
  [@warning "-39"]
  type t = { keypair: Keypair.t
           ; committed: State.t signed
           ; current: State.t
           ; fee_schedule: OperatorFeeSchedule.t }
  [@@deriving lens { prefix=true}, yojson, rlp]

  module PrePersistable = struct
    type nonrec t = t
    let marshaling = marshaling_of_rlping rlping
    let walk_dependencies _methods context {committed; current} =
      let open Lwt in
      walk_dependency SignedState.dependency_walking context committed
      >>= fun () -> walk_dependency State.dependency_walking context current
    let make_persistent = normal_persistent
    let yojsoning = {to_yojson;of_yojson}
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
  (** TODO: somehow only save the current/committed state and the fee schedule *)
  let operator_state_key operator_address =
    "LCFS0001" ^ (Address.to_big_endian_bits operator_address)
  let save operator_state =
    let open Lwt in
    save operator_state (* <-- use inherited binding *)
    >>= fun () ->
    let address = operator_state.keypair.address in
    let key = operator_state_key address in
    Db.put key (Digest.to_big_endian_bits (digest operator_state))
  let load operator_address =
    operator_address |> operator_state_key |> Db.get
    |> (function
      | Some x -> x
      | None -> raise (Operator_not_found
                         (Printf.sprintf "Operator %s not found in the database"
                            (Address.to_0x operator_address))))
    |> Digest.unmarshal_string |> db_value_of_digest unmarshal_string
end

(* TODO:
   divide requests in multiple kinds:
 * user query, which looks at some existing account without modification
 * user transaction request, which modifies their account
 * system transaction, which e.g. posts a state update to the main chain
   (Which all the time update the main chain.
    We have a batch of operations to put to the main chain.)



   The side-chain has three different (kind of) states:
 * current, the operator's view of itself
 * pending to the main chain, but not yet passed the challenge period
   (there can be multiple such states at various stages of advancement,
   e.g. posted on the main chain but not yet in a main chain block,
   in a main chain block but not yet considered final,
   in a final-enough main chain block, still unchallenged but not yet past the challenge period
   in a final-enough main chain block and challenged, trial still open.
 * confirmed on the main chain
 * old enough that it doesn't directly matter to the contract anymore.
*)

module OperatorAction = Action(OperatorState)
module OperatorAsyncAction = AsyncAction(OperatorState)

type account_lens = (OperatorState.t, AccountState.t) Lens.t

(* type transport_data = Digest.t *)
type transport_data = (TransactionReceipt.t * Digest.t) option

type validated_transaction_request =
  [ `Confirm of (TransactionRequest.t * transport_data) * ((Transaction.t * transport_data) * unit Lwt.t) or_exn Lwt.u ]

type inner_transaction_request =
  [ validated_transaction_request
  | `Flush of int
  | `Committed of (State.t signed * unit Lwt.u)
  | `GetCurrentDigest of (Digest.t Lwt.u) ]

let inner_transaction_request_mailbox : inner_transaction_request Lwt_mvar.t = Lwt_mvar.create_empty ()

type operator_service =
  { address : Address.t
  ; state_ref : OperatorState.t ref }

let the_operator_service_ref : (operator_service option ref) = ref None

let get_operator_state : unit -> OperatorState.t =
  fun () ->
  !the_operator_service_ref
  |> (function Some x -> x | None -> bork "Operator service not started")
  |> fun service -> !(service.state_ref)

let operator_account_lens : Address.t -> account_lens =
  fun address ->
  OperatorState.lens_current |-- State.lens_accounts
  |-- defaulting_lens (konstant AccountState.empty) (AccountMap.lens address)

let signed_request_requester : UserTransactionRequest.t signed -> Address.t =
  fun rx ->
  rx.payload.UserTransactionRequest.rx_header.requester

(** Check that the request is basically well-formed, or else fail
    This function should include all checks that can be made without any non-local side-effect
    beside reading pure or monotonic data, which is allowed for now
    (but may later have to be split to another function).
    Thus, we can later parallelize this check.
    NB: the "is_forced" flagged denotes whether the transaction is being forced upon the operator
    by having been published on the main chain (or, in the future, in sister chains).
    TODO: parallelize the signature checking in a C worker thread that lets us do additional OCaml work.
 *)
let validate_user_transaction_request :
  (UserTransactionRequest.t signed * bool, TransactionRequest.t) Lwt_exn.arr =
  fun ((signed_request, is_forced) : (UserTransactionRequest.t signed * bool)) ->
    let {payload=UserTransactionRequest.{ rx_header={ requester; requester_revision }; operation }} =
      signed_request in
    let state = get_operator_state () in
    let AccountState.{balance; account_revision} = (operator_account_lens requester).get state in
    let OperatorState.{fee_schedule} = state in
    let open Lwt_exn in
    let check (test: bool) (exngen: unit -> string) =
      fun x ->
        if test then return x
        else fail (Malformed_request (exngen ())) in
    `UserTransaction signed_request
    |> ((* XXX THE REVISION CHECK BELOW IS TEMPORARILY DISABLED FOR DEMO PURPOSES,
           UNTIL WE IMPROVE THE SIDE CHAIN USER SOFTWARE
           TODO - Fix the side_chain_user and endpoints/actions code, then re-enable this check. *)
      let open Revision in
      Logging.log "requester_revision=%s account_revision=%s" (Revision.to_string requester_revision) (Revision.to_string account_revision);
      check (requester_revision = (add account_revision one) || true) (* <-- TODO: REMOVE the || true *)
        (fun () ->
           Printf.sprintf "You made a request with revision %s but the next expected revision is %s"
             (to_string requester_revision) (to_string (add account_revision one)))
      >>> check (is_signed_value_valid UserTransactionRequest.digest requester signed_request)
            (konstant "The signature for the request doesn't match the requester")
      (* TODO: check confirmed main & side chain state + validity window *)
      >>> let open TokenAmount in
      match operation with
      | UserOperation.Deposit
          { deposit_amount
          ; deposit_fee
          ; main_chain_deposit=Ethereum_chain.SignedTransactionData.{value} as main_chain_deposit
          ; main_chain_deposit_confirmation } ->
        check (is_sum value deposit_amount deposit_fee)
          (fun () ->
             Printf.sprintf "Deposit amount %s and fee %s fail to add up to deposit value %s"
               (to_string deposit_amount) (to_string deposit_fee) (to_string value))
        >>> check (is_forced || compare deposit_fee fee_schedule.deposit_fee >= 0)
              (fun () -> Printf.sprintf "Insufficient deposit fee %s, requiring at least %s"
                           (to_string deposit_fee) (to_string fee_schedule.deposit_fee))
        >>> Ethereum_transaction.check_transaction_confirmation
              ~sender:requester ~recipient:(get_contract_address ())
              main_chain_deposit main_chain_deposit_confirmation
      | UserOperation.Payment {payment_invoice; payment_fee; payment_expedited=_payment_expedited} ->
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
        (* Check for overflow *)
        >>> check (is_forced ||
                   is_mul_valid state.fee_schedule.fee_per_billion payment_invoice.amount)
              (fun () ->
                 Printf.sprintf
                   "Payment fee calculation overflows with amount %s, scheduled fee per billion %s"
                   (to_string payment_invoice.amount) (to_string fee_schedule.fee_per_billion))
        (* Check that the payment fee is sufficient *)
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
      | UserOperation.Withdrawal {withdrawal_amount; withdrawal_fee} ->
        check (is_add_valid withdrawal_amount withdrawal_fee)
          (fun () ->
            "Adding withdrawal amount and fee causes an overflow!")
        >>> check (compare balance (add withdrawal_amount withdrawal_fee) >= 0)
              (fun () ->
                Printf.sprintf "Balance %s insufficient to cover withdrawal amount %s plus fee %s"
                  (to_string balance) (to_string withdrawal_amount) (to_string withdrawal_fee))
        >>> check (is_forced || compare withdrawal_fee fee_schedule.withdrawal_fee >= 0)
              (fun () ->
                Printf.sprintf "Insufficient withdrawal fee %s, requiring at least %s"
                  (to_string withdrawal_fee) (to_string fee_schedule.withdrawal_fee)))

(** Add a transaction to the side_chain, given the [updated_limit] and the [tx_request].
    Don't do that until you've properly processed the transaction! *)
let add_transaction : TokenAmount.t -> (TransactionRequest.t, Transaction.t) OperatorAction.arr =
  fun updated_limit tx_request operator_state ->
    let tx_revision = Revision.(add operator_state.current.operator_revision one) in
    let transaction = Transaction.{tx_header=TxHeader.{tx_revision; updated_limit};tx_request} in
    OperatorAction.return transaction
      (operator_state
       |> (OperatorState.lens_current |-- State.lens_operator_revision).set tx_revision
       |> Lens.modify (OperatorState.lens_current |-- State.lens_transactions)
            (TransactionMap.add tx_revision transaction))

let make_user_transaction : (UserTransactionRequest.t signed, Transaction.t) OperatorAction.arr =
  fun signed_request operator_state ->
    let tx_request=`UserTransaction signed_request in
    let updated_limit= operator_state.current.spending_limit in
    let requester = signed_request_requester signed_request in
    let account_lens = operator_account_lens requester in
    let new_requester_revision = signed_request.payload.rx_header.requester_revision in
    add_transaction updated_limit tx_request
      (operator_state
       |> (account_lens |-- AccountState.lens_account_revision).set new_requester_revision)

(** Get balance for given account at given revision in given side chain state *)
let _balance_at_revision : Address.t -> Revision.t -> State.t -> TokenAmount.t =
  fun address revision state ->
    ignore (address, revision, state);
    bottom ()

(* TODO: Clearly this function returning zero is not what we want *)
let compute_updated_limit :
  operator:Address.t -> previous_confirmed:State.t -> new_confirmed:Revision.t -> current:State.t
  -> TokenAmount.t =
  fun ~operator ~previous_confirmed ~new_confirmed ~current ->
    ignore (operator, previous_confirmed, new_confirmed, current);
    TokenAmount.zero
(*
   XXXXX
   We may need to keep track of more quantities that initially imagined. Figure out which.

   How is the total limit defined?
 * consider the last *committed* bond B (balance for operator address? Or something ad hoc?)
 * multiply it by a fraction F (say 1/5) for the limit.

   What diminishes the limit?
 * taking money out of the bond / operator address
 * taking money out of (other) addresses in an expedited way.

   When you post an update, what happens?
 * Some newly *committed* state was confirmed on the main chain.
 * The *committed* state is in the future to the *previous committed* state.
 * The *committed* state is in the past of the *current* state.
 * The bond has changed: it is now based on the account balance at the new committed state,
 * The set of expedited transactions that encroach on the limit has changed, too:
   it is now smaller, and we should be able to computed how that has changed by
   a difference between the spent since previous_committed and committed.
   [So, maybe separately both a "bond-based limit" and a "tokens spent expeditedly" amount,
   rather than just the synthetic difference? Or can we do with a single synthetic number?]

   Restrictions:
 * Transfers to/from the bond account are special, because of the fraction F.
 * Hypothetical rule: any payment from the operator balance / bond MUST be expedited,
   so it is properly accounted??
   (THEN, to get all your money: do it in N steps until there's not enough money left to care;
   or close the entire chain).
   OR, it must be done as the last thing before the state update, or as part of the state update?
 * you cannot withdraw directly from the operator balance, or we must specially make all such
   withdrawals expedited somehow.

   let operator_address = operator_state.keypair.address in
   let previous_committed_state = operator_state.committed.payload in
   let previous_committed_revision = previous_committed_state.operator_revision in
   let current_state = operator_state.current in
   let current_revision = current_state.operator_revision in
   let previous_limit = previous_committed_state.spending_limit in
   let previous_balance =
   balance_at_revision operator_address previous_committed_revision operator_state.current in
   let current_balance =
   balance_at_revision operator_address current_revision operator_state.current in
   let delta_in_operator_balance = TokenAmount.sub current_balance previous_balance in
   (* TODO: have a variant of add that also checks the upper limit and has monadic errors *)
   TokenAmount.add previous_limit delta_in_operator_balance in
   TokenAmount.zero
*)

let process_admin_transaction_request :
  (AdminTransactionRequest.t, Transaction.t) OperatorAction.arr =
  fun request operator_state ->
    let tx_request=`AdminTransaction request in
    let updated_limit = match request with
      | StateUpdate (side_chain_revision, _digest) ->
        compute_updated_limit
          ~operator:operator_state.keypair.address
          ~previous_confirmed:operator_state.committed.payload (* TODO: rename committed to confirmed *)
          ~new_confirmed:side_chain_revision
          ~current:operator_state.current in
    add_transaction updated_limit tx_request operator_state

let modify_guarded_state guard modification lens failure success state =
  if guard (lens.Lens.get state) then
    Ok success, Lens.modify lens modification state
  else
    Error (failure state), state

let decrement_state_tokens amount =
  modify_guarded_state
    (fun x -> TokenAmount.compare x amount >= 0)
    (fun x -> TokenAmount.sub x amount)

(** Operator actions to use up some of the limit *)
exception Spending_limit_exceeded

let spend_spending_limit (amount : TokenAmount.t) : ('a, 'a) OperatorAction.arr =
  decrement_state_tokens
    amount
    (OperatorState.lens_current |-- State.lens_spending_limit)
    (fun _ -> Spending_limit_exceeded)

let maybe_spend_spending_limit
      (is_expedited : bool) (amount: TokenAmount.t) : ('a, 'a) OperatorAction.arr =
  if is_expedited then spend_spending_limit amount else OperatorAction.return

exception Already_posted

exception Insufficient_balance of string

(* To prevent double-deposit or double-withdrawal of a same main_chain_transaction_signed,
   we put those transactions in a set of already posted transactions.
   (Future: prune that set by expiring deposit requests?
   Have more expensive process to account for old deposits?)
*)
let check_against_double_accounting
      (main_chain_transaction : Ethereum_chain.SignedTransactionData.t)
  : ('a, 'a) OperatorAction.arr =
  let witness = Ethereum_chain.SignedTransactionData.digest main_chain_transaction in
  let lens =
    OperatorState.lens_current
    |-- State.lens_main_chain_transactions_posted
    |-- DigestSet.lens witness in
  modify_guarded_state not (konstant true) lens (fun _ -> Already_posted)

let credit_balance (amount : TokenAmount.t) (account_address : Address.t)
  : ('a, 'a) OperatorAction.arr =
  OperatorAction.map_state
    (Lens.modify (operator_account_lens account_address |-- AccountState.lens_balance)
       (TokenAmount.add amount))


let debit_balance (amount : TokenAmount.t) (account_address : Address.t)
  : ('a, 'a) OperatorAction.arr =
  let lens = operator_account_lens account_address |-- AccountState.lens_balance in
  decrement_state_tokens
    amount lens
    (fun state ->
       Insufficient_balance
         (Printf.sprintf "Account %s has insufficient balance %s to debit transaction value %s"
            (Address.to_0x account_address) (TokenAmount.to_string (lens.get state))
            (TokenAmount.to_string amount)))

let accept_fee (fee : TokenAmount.t) : ('a, 'a) OperatorAction.arr =
  fun x state -> credit_balance fee state.OperatorState.keypair.address x state

(** compute the effects of a request on the account state *)
let effect_validated_user_transaction_request :
  ( UserTransactionRequest.t signed, UserTransactionRequest.t signed) OperatorAction.arr =
  fun rx ->
    let open OperatorAction in
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

let post_state_update_needed_tr (transreq : TransactionRequest.t) : bool =
  match transreq with
  | `AdminTransaction _ -> false
  | `UserTransaction _ -> true

(** TODO: have a server do all the effect_requests sequentially,
    after they have been validated in parallel (well, except that Lwt is really single-threaded *)
(* let post_validated_transaction_request : TransactionRequest.t -> (Transaction.t * unit Lwt.t) Lwt_exn.t*)
let post_validated_transaction_request :
      ( (TransactionRequest.t * transport_data), (Transaction.t * transport_data) * unit Lwt.t) Lwt_exn.arr =
  simple_client inner_transaction_request_mailbox
    (fun ((request, trans_data) : ((TransactionRequest.t * transport_data) * ((Transaction.t * transport_data) * unit Lwt.t) or_exn Lwt.u)) ->
      `Confirm (request, trans_data))


let post_state_update_request (transreq : TransactionRequest.t) : (TransactionRequest.t * transport_data) Lwt_exn.t =
  Logging.log "post_state_update_request, beginning of function";
  let (lneedupdate : bool) = post_state_update_needed_tr transreq in
  (*  Logging.log "post_state_update_request lneedupdate=%B" lneedupdate; *)
  if lneedupdate then
    let get_transport_data : Digest.t -> transport_data Lwt_exn.t =
      (fun digest ->
        (*        Lwt.bind (post_state_update digest)*)
        Lwt.bind (post_to_mailbox_state_update digest)
          (fun receipt_exn ->
            match receipt_exn with
            | Ok receipt ->
               let ret_val : transport_data = Some (receipt, digest) in
               Lwt_exn.return ret_val
            | Error _error -> bork "Cannot handle error in the post_state_update")) in
    let get_state_digest : TransactionRequest.t -> Digest.t Lwt.t =
      fun transreq ->
      simple_client inner_transaction_request_mailbox
                  (fun ((_request, digest_resolver) : (TransactionRequest.t * Digest.t Lwt.u)) ->
                    `GetCurrentDigest digest_resolver) transreq in
    (*    Logging.log "post_state_update_request, before simple_client and push function"; *)
    Lwt_exn.bind (Lwt.bind (get_state_digest transreq) get_transport_data)
      (fun (trans_data : transport_data) ->
        let ret_valb : (TransactionRequest.t * transport_data) = (transreq, trans_data) in
        Lwt_exn.return ret_valb)
  else
    Lwt_exn.return (transreq, None)

let process_validated_transaction_request : (TransactionRequest.t, Transaction.t) OperatorAction.arr =
  function
  | `UserTransaction request ->
    request |> OperatorAction.(effect_validated_user_transaction_request >>> make_user_transaction)
  | `AdminTransaction request ->
    process_admin_transaction_request request

let fct_transaction_hash : transport_data -> (Digest.t * Digest.t) =
  fun trans_data ->
  match trans_data with
  | None -> (Digest.zero, Digest.zero)
  | Some (receipt, digest) -> (receipt.transaction_hash, digest)

let make_transaction_commitment : (Transaction.t * transport_data) -> TransactionCommitment.t =
  fun (transaction, trans_data) ->
    let OperatorState.{committed} = get_operator_state () in
    let State.{ operator_revision
              ; spending_limit
              ; accounts
              ; transactions
              ; main_chain_transactions_posted } = committed.payload in
    let accounts = dv_digest accounts in
    let signature = committed.signature in
    let main_chain_transactions_posted = dv_digest main_chain_transactions_posted in
    let (state_update_transaction_hash, state_digest) = fct_transaction_hash trans_data in
    let tx_revision = transaction.tx_header.tx_revision in
    match TransactionMap.Proof.get tx_revision transactions with
    | Some tx_proof ->
      TransactionCommitment.
        { transaction; tx_proof; operator_revision; spending_limit;
          accounts; main_chain_transactions_posted; signature;
          state_update_transaction_hash; state_digest }
    | None -> bork "Transaction %s not found, cannot build commitment!" (Revision.to_0x tx_revision)

(* Process a user request, with a flag to specify whether it's a forced request
   (published on the main chain), in which case there are no fee amount minima.

   We use the latest current state of the operator exported by the inner loop, but read-only,
   to check that the request is valid.
   If it is, we pass it to the inner loop, that will use it read-write.
   In the future, maybe moving away from Lwt and from single-threaded OCaml,
   and/or using forking and reducing the use of operator state so no DB access is needed,
   this could be done in different threads or processes.
*)
let process_user_transaction_request :
      (UserTransactionRequest.t signed * bool, TransactionCommitment.t) Lwt_exn.arr =
  let open Lwt_exn in
  validate_user_transaction_request
  >>> post_state_update_request
  >>> post_validated_transaction_request
  >>> fun ((transaction_pair, wait_for_commit) : ((Transaction.t * transport_data) * unit Lwt.t)) : TransactionCommitment.t Lwt_exn.t ->
  let open Lwt in
  wait_for_commit
  >>= fun () ->
  make_transaction_commitment transaction_pair |> Lwt_exn.return


let oper_post_user_transaction_request : UserTransactionRequest.t signed -> TransactionCommitment.t Lwt_exn.t =
  fun request ->
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

let get_account_balance address (operator_state:OperatorState.t) =
  try
    let account_state = AccountMap.find address operator_state.current.accounts in
    `Assoc [("address",Address.to_yojson address)
           ;("account_balance",TokenAmount.to_yojson account_state.balance)]
  with Not_found ->
    error_json "Could not find balance for address %s" (Address.to_0x address)

let get_account_state address (operator_state:OperatorState.t) =
  try
    AccountMap.find address operator_state.current.accounts
    |> fun acct_state ->
    `Assoc [("address",Address.to_yojson address)
           ;("account_state",AccountState.to_yojson acct_state)
           ]
  with Not_found ->
    error_json "Could not find account state for account: %s" (Address.to_0x address)

(* TODO: only provide side-chain status.
   Clients must separately query their own ethereum node for their main chain status,
   then reconcile the results. Otherwise, easy DoS attack. *)
let get_account_status address operator_state =
  let open Lwt_exn in
  let exception Failure_to_get_main_chain_balance of exn in
  let exception Failure_to_get_main_chain_transaction_count of exn in
  let side_chain_state = get_account_state address operator_state in
  trying Ethereum_json_rpc.eth_get_balance (address, Latest)
  >>= handling (fun e -> fail (Failure_to_get_main_chain_balance e))
  >>= fun balance ->
  trying Ethereum_json_rpc.eth_get_transaction_count (address, Pending)
  >>= handling (fun e -> fail (Failure_to_get_main_chain_transaction_count e))
  >>= fun revision ->
  let main_chain_account = { address; balance; revision } in
  return (`Assoc [("side_chain_account",side_chain_state)
                 ;("main_chain_account",main_chain_account_state_to_yojson main_chain_account)])

let get_account_balances (operator_state:OperatorState.t) =
  (* TODO Just as in `get_account_status` above, clients should separately query
   * their own ethereum node
   *
   * TODO Replace ad-hoc serialization with concrete type
   *
   * TODO Right now, even after pre-funding, you get no data back from this
   * query unless a user has triggered a deposit/withdrawal from the UI; we
   * should be sending full+accurate user account states regardless
   *
   * TODO Remove/replace the Trent exclusion line; if we continue to exclude
   * Trent then there should be a comment explaining why and the account's
   * address should be taken from the operator state rather than a magic
   * constant
   *)
  let open Lwt_exn in
  AccountMap.bindings operator_state.current.accounts
  |> List.filter (fst >> ((<>) Test.trent_address)) (* Exclude Trent *)
  |> list_map_p (fun (addr, _) ->
         get_account_status addr operator_state
        >>= fun d -> return (Address.to_0x addr, d))
    >>= fun bs -> return @@ `Assoc bs

(* TODO: maintain per-account index of transactions, otherwise this won't scale!!! *)
let get_recent_transactions address maybe_limit operator_state =
  let all_transactions = operator_state.OperatorState.current.transactions in
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

let get_2proof tx_revision (operator_state : OperatorState.t) =
  let transactions = operator_state.current.transactions in
  match TransactionMap.Proof.get tx_revision transactions with
  | None ->
    error_json "Cannot provide proof for tx-revision: %s" (Revision.to_string tx_revision)
  | Some proof -> TransactionMap.Proof.to_yojson proof


let get_contract_address_yojson () =
  let open Lwt_exn in
  let contr_addr = get_contract_address() in
  return (`Assoc [("contract_address",Address.to_yojson contr_addr)])



(** Take messages from the user_query_request_mailbox, and process them (TODO: in parallel?) *)
(*let process_user_query_request : (request : UserQueryRequest.t) : yojson Lwt_exn.t = *)
let process_user_query_request request =
  let open Lwt_exn in
  let state = get_operator_state () in
  (match (request : UserQueryRequest.t) with
   | Get_account_balance {address} ->
     get_account_balance address state |> return
   | Get_account_balances ->
     get_account_balances state
   | Get_contract_address ->
     get_contract_address_yojson ()
   | Get_account_state {address} ->
     get_account_state address state |> return
   | Get_account_status {address} ->
     get_account_status address state
   | Get_recent_transactions { address; count } ->
     get_recent_transactions address count state |> return
   | Get_proof {tx_revision} ->
     get_2proof tx_revision state |> return)

let oper_post_user_query_request =
  (*stateless_parallelize*) process_user_query_request

(** Take messages from the admin_query_request_mailbox, and process them (TODO: in parallel?) *)
let process_admin_query_request = bottom

let oper_post_admin_query_request =
  (*stateless_parallelize*) process_admin_query_request


(** We assume that the operation will correctly apply:
    balances are sufficient for spending,
    deposits confirmation will check out,
    active revision will only increase, etc.
*)
let increment_capped max x =
  if x < max then x + 1 else max

let inner_transaction_request_loop =
  let open Lwter in
  fun (operator_state_ref : OperatorAsyncAction.state ref) ->
    return (!operator_state_ref, 0, Lwt.return_unit)
    >>= forever
          (fun ((operator_state, batch_id, previous) : (OperatorAsyncAction.state * int * unit Lwt.t)) ->
            Logging.log "inner_transaction_request_loop, beginning of lambda";
             (* The promise sent back to requesters, that they have to wait on
                for their confirmation's batch to have been committed,
                and our private resolver for this batch. *)
             let ((batch_committed_t, notify_batch_committed_u) : (unit Lwt.t * unit Lwt.u)) = Lwt.task () in
             (* An internal promise to detect if and when we trigger the batch based on
                a timeout having been passed since the earliest unprocessed commit request. *)
             let ((time_trigger_t, time_trigger_u) : (unit Lwt.t * unit Lwt.u)) = Lwt.task () in
             (* An internal promise to detect if and when we trigger the batch based on
                the size of the batch becoming too long. *)
             let ((size_trigger_t, size_trigger_u) : (unit Lwt.t * unit Lwt.u)) = Lwt.task () in
             (* When we are ready and either trigger criterion is met,
                send ourselves a Flush message for this batch_id *)
             Lwt.async (fun () -> Lwt.join [previous;Lwt.pick [time_trigger_t; size_trigger_t]]
                                  >>= (fun () ->
                          Lwt_mvar.put inner_transaction_request_mailbox (`Flush batch_id)));
             let rec request_batch (operator_state : OperatorAsyncAction.state) (size : int) : (OperatorAsyncAction.state * int * unit Lwt.t) Lwt.t =
               (** The below mailbox is filled by post_validated_request, except for
                   the async line just preceding, whereby a `Flush message is sent. *)
               Lwt_mvar.take inner_transaction_request_mailbox
               >>= function
               | `Confirm ((request_signed_dig, continuation) : ((TransactionRequest.t * transport_data) * ((Transaction.t * transport_data) * unit Lwt.t) or_exn Lwt.u)) ->
                 Logging.log "inner_transaction_request_loop, CASE : Confirm";
                 let (request_signed, trans_data) = request_signed_dig in
                 process_validated_transaction_request request_signed operator_state
                 |> fun ((confirmation_or_exn, new_operator_state) : (Transaction.t OrExn.t * OperatorAsyncAction.state)) ->
                 operator_state_ref := new_operator_state;
                 (match confirmation_or_exn with
                  | Error e ->
                    Logging.log "inner_transaction_request_loop, error case";
                    Lwt.wakeup_later continuation (Error e);
                    request_batch new_operator_state size
                  | Ok confirmation ->
                    Logging.log "inner_transaction_request_loop, Ok case";
                    Lwt.wakeup_later continuation (Ok ((confirmation, trans_data), batch_committed_t));
                    let new_size = increment_capped max_int size in
                    if new_size = Side_chain_server_config.batch_size_trigger_in_requests then
                      (* Flush the data after enough entries are written *)
                      Lwt.wakeup_later size_trigger_u ()
                    else if new_size = 1 then
                      (* Start a timeout to trigger flushing, but only after some entry is written *)
                      Lwt.async (fun () -> Lwt_unix.sleep Side_chain_server_config.batch_timeout_trigger_in_seconds
                                  >>= fun () -> Lwt.wakeup_later time_trigger_u ();
                                                Lwt.return_unit);
                    request_batch new_operator_state new_size)
               | `GetCurrentDigest (digest_resolver : Digest.t Lwt.u) ->
                  Logging.log "inner_transaction_request, CASE : GetCurrentDigest";
                  (* Lwt.wakeup_later notify_batch_committed_u (); *)
                  Lwt.wakeup_later digest_resolver (State.digest !operator_state_ref.current);
                  request_batch operator_state size
               (* Lwt.return (operator_state, batch_id, batch_committed_t) *)
               | `Flush (id : int) ->
                 Logging.log "inner_transaction_request_loop, CASE : Flush";
                 assert (id = batch_id);
                 if size > 0 then
                   (let ((ready, notify_ready) : (unit Lwt.t * unit Lwt.u)) = Lwt.task () in
                    let signed_state =
                      SignedState.make operator_state.keypair operator_state.current in
                    let operator_state_to_save =
                      OperatorState.lens_committed.set signed_state operator_state in
                    Lwt.async (fun () ->
                      ready
                      >>= fun () ->
                      Lwt_mvar.put inner_transaction_request_mailbox
                        (`Committed (signed_state, notify_batch_committed_u)));
                    OperatorState.save operator_state_to_save
                    >>= fun () -> Db.async_commit notify_ready
                    >>= fun () -> Lwt.return (operator_state, (batch_id + 1), batch_committed_t))
                 else
                   (Lwt.wakeup_later notify_batch_committed_u ();
                    Lwt.return (operator_state, (batch_id + 1), batch_committed_t))
               | `Committed ((signed_state, previous_notify_batch_committed_u) : (State.t signed * unit Lwt.u)) ->
                 Logging.log "inner_transaction_request_loop, CASE : Committed";
                 let new_operator_state =
                   OperatorState.lens_committed.set signed_state operator_state in
                 operator_state_ref := new_operator_state;
                 Lwt.wakeup_later previous_notify_batch_committed_u ();
                 request_batch new_operator_state size
             in request_batch operator_state 0)


let initial_side_chain_state =
  State.
    { operator_revision= Revision.of_int 0
    ; spending_limit= TokenAmount.of_int 0 (* TODO: have a way to ramp it up! *)
    ; accounts= AccountMap.empty
    ; transactions= TransactionMap.empty
    ; main_chain_transactions_posted= DigestSet.empty }

let initial_operator_state address =
  let keypair = keypair_of_address address in
  OperatorState.
    { keypair
    ; committed= SignedState.make keypair initial_side_chain_state
    ; current= initial_side_chain_state
    ; fee_schedule= initial_fee_schedule }

(* TODO: make it a PersistentActivity. *)
(* TODO: don't create a new operator unless explicitly requested? *)
let start_operator address =
  let open Lwt_exn in
  match !the_operator_service_ref with
  | Some x ->
    if Address.equal x.address address then
      (Logging.log "Operator service already running for address %s, not starting another one"
         (Address.to_0x address);
       return ())
    else
      bork "Cannot start a operator service for address %s because there's already one for %s"
        (Address.to_0x address) (Address.to_0x x.address)
  | None ->
    let operator_state =
      (* TODO: don't create a new operator unless explicitly requested? *)
      try
        OperatorState.load address
      with Not_found -> initial_operator_state address
    in
    let state_ref = ref operator_state in
    the_operator_service_ref := Some { address; state_ref };
    Lwt.async (const state_ref >>> inner_transaction_request_loop);
    Lwt_exn.return ()


(* Need to create a thread, persistent activity
   for the merging operation to the main chain.
   ---Polled by the users for their transaction.
   ---push transaction to the ethereum.
   Advanced TODO: update as the auction plays out.
 *)
module Test = struct
  open Signing.Test

  let get_operator_state = get_operator_state

  (* a sample operator state *)

  let%test "db-save-retrieve" =
    (* test whether retrieving a saved operator state yields the same state
       here, the account and confirmation maps are empty, so it doesn't really
       exercise the node-by-node persistence machinery
       in Side_chain_action.Test, the "deposit_and_payment_valid" test does
       a save and retrieval with nonempty such maps
    *)
    register_test_keypairs ();
    let open Lwt in
    Db.run ~db_name:"unit_test_db"
      (fun () ->
         let trent_state = initial_operator_state trent_address in
         OperatorState.save trent_state
         >>= Db.commit
         >>= fun () ->
         let retrieved_state = OperatorState.load trent_address in
         Lwt.return (OperatorState.to_yojson_string retrieved_state
                     = OperatorState.to_yojson_string trent_state))
end

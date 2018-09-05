(* WARNING: We use GLOBAL STATE for our mailboxes,
   so there's only one facilitator running in a given process.
   I blame OCaml for lack of dynamic binding. Yay Common Lisp special variables and Scheme parameters! *)
open Lens.Infix

open Legilogic_lib
open Lib
open Action
open Yojsoning
open Signing
open Types
open Merkle_trie

open Legilogic_ethereum

open Side_chain

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
  [ `Confirm of TransactionRequest.t * (Transaction.t * unit Lwt.t) or_exn Lwt.u
  | `Flush of int ]

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
          ; main_chain_deposit_confirmation
          ; deposit_expedited } ->
        check (is_sum value deposit_amount deposit_fee)
          (fun () ->
             Printf.sprintf "Deposit amount %s and fee %s fail to add up to deposit value %s"
               (to_string deposit_amount) (to_string deposit_fee) (to_string value))
        >>> check (not (is_forced && deposit_expedited))
              (konstant "You cannot force an expedited deposit")
        >>> check (is_forced || compare deposit_fee fee_schedule.deposit_fee >= 0)
              (fun () -> Printf.sprintf "Insufficient deposit fee %s, requiring at least %s"
                           (to_string deposit_fee) (to_string fee_schedule.deposit_fee))
        (* TODO: CHECK FROM THE CONFIRMATION THAT THE CORRECT PERSON DID THE DEPOSIT,
           AND/OR THAT IT   WAS TAGGED WITH THE CORRECT RECIPIENT. *)
        >>> check (Main_chain.is_confirmation_valid
                     main_chain_deposit_confirmation main_chain_deposit)
              (fun () -> "The main chain deposit confirmation is invalid")
      | Payment {payment_invoice; payment_fee; payment_expedited=_payment_expedited} ->
        check (is_add_valid payment_invoice.amount payment_fee)
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
      (main_chain_transaction : Main_chain.Transaction.t)
  : ('a, 'a) FacilitatorAction.arr =
  let witness = Main_chain.Transaction.digest main_chain_transaction in
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
            (Address.to_0x_string account_address) (TokenAmount.to_string (lens.get state))
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
    | Deposit { deposit_amount; deposit_fee; main_chain_deposit; deposit_expedited } ->
      maybe_spend_spending_limit deposit_expedited deposit_amount
      >>> check_against_double_accounting main_chain_deposit
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


let simple_client :
  ('request * 'result Lwt.u -> 'message) -> 'message Lwt_mvar.t -> ('request, 'result) Lwt_monad.arr =
  fun make_message mailbox request ->
    let open Lwt in
    let (promise, resolver) = task () in
    make_message (request, resolver)
    |> (Lwt_mvar.put mailbox)
    >>= fun () -> promise

(** TODO: have a server do all the effect_requests sequentially,
    after they have been validated in parallel (well, except that Lwt is really single-threaded *)
let post_validated_transaction_request :
  ( TransactionRequest.t, Transaction.t * unit Lwt.t) Lwt_exn.arr =
  simple_client
    (fun (request, resolver) -> `Confirm (request, resolver))
    inner_transaction_request_mailbox

let process_validated_transaction_request : (TransactionRequest.t, Transaction.t) FacilitatorAction.arr =
  function
  | `UserTransaction request ->
    request |> FacilitatorAction.(effect_validated_user_transaction_request >>> make_user_transaction)
  | `AdminTransaction _ ->
    bottom () (* TODO: do it *)

(* Simple actor loop *)
let simple_request_loop mailbox processor =
  let open Lwt_monad in
  forever
    (fun () ->
       Lwt_mvar.take mailbox
       >>= fun (request, continuation) ->
       processor request
       >>= fun confirmation_or_exn ->
       Lwt.wakeup_later continuation confirmation_or_exn;
       Lwt.return_unit)

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
  (UserTransactionRequest.t signed * bool, Transaction.t) Lwt_exn.arr =
  let open Lwt_exn in
  validate_user_transaction_request
  >>> post_validated_transaction_request
  >>> fun (confirmation, wait_for_commit) ->
  (wait_for_commit |> Lwt.(>>=)) (const confirmation)

let user_transaction_request_mailbox :
  ((UserTransactionRequest.t signed * bool) * Transaction.t or_exn Lwt.u) Lwt_mvar.t =
  Lwt_mvar.create_empty ()
let user_transaction_request_loop =
  simple_request_loop user_transaction_request_mailbox process_user_transaction_request
let post_user_transaction_request :
  (UserTransactionRequest.t signed * bool, Transaction.t) Lwt_exn.arr =
  simple_client identity user_transaction_request_mailbox

(** Take messages from the user_query_request_mailbox, and process them (TODO: in parallel?) *)
let process_user_query_request = bottom
let user_query_request_mailbox :
  (UserQueryRequest.t * yojson or_exn Lwt.u) Lwt_mvar.t =
  Lwt_mvar.create_empty ()
let user_query_request_loop =
  simple_request_loop user_query_request_mailbox process_user_query_request
let post_user_query_request =
  simple_client identity user_query_request_mailbox

(** Take messages from the admin_query_request_mailbox, and process them (TODO: in parallel?) *)
let process_admin_query_request = bottom
let admin_query_request_mailbox :
  (AdminQueryRequest.t * yojson or_exn Lwt.u) Lwt_mvar.t =
  Lwt_mvar.create_empty ()
let admin_query_request_loop =
  simple_request_loop admin_query_request_mailbox process_admin_query_request
let post_admin_query_request =
  simple_client identity admin_query_request_mailbox


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
  let open Lwt in
  let open Lwt_monad in
  fun facilitator_state_ref ->
    return (!facilitator_state_ref, 0, return_unit)
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
                                  return_unit);
                    request_batch new_facilitator_state new_size)
               | `Flush id ->
                 assert (id = batch_id);
                 (if size > 0 then
                    (Side_chain.FacilitatorState.save facilitator_state
                     >>= fun () -> Db.async_commit notify_batch_committed)
                  else
                    Lwt.return_unit)
                 >>= fun () -> Lwt.return (facilitator_state, (batch_id + 1), batch_committed) in
             request_batch facilitator_state 0)

let start_facilitator address =
  let open Lwt_exn in
  match !the_facilitator_service_ref with
  | Some x ->
    if Address.equal x.address address then
      (Logging.log "Facilitator service already running for address %s, not starting another one"
         (Address.to_0x_string address);
       return ())
    else
      bork "Cannot start a facilitator service for address %s because there's already one for %s"
        (Address.to_0x_string address) (Address.to_0x_string x.address)
  | None ->
    let facilitator_state =
      try
        FacilitatorState.load address
      with Not_found ->
        (* TODO: move that somewhere else, and fail instead *)
        let open Side_chain in
        let trent_fee_schedule : FacilitatorFeeSchedule.t =
          { deposit_fee= TokenAmount.of_int 5
          ; withdrawal_fee= TokenAmount.of_int 5
          ; per_account_limit= TokenAmount.of_int 20000
          ; fee_per_billion= TokenAmount.of_int 42 }
        in
        let (confirmed_trent_state : Side_chain.State.t) =
          { facilitator_revision= Revision.of_int 0
          ; spending_limit= TokenAmount.of_int 1000000
          ; accounts= AccountMap.empty
          ; transactions= TransactionMap.empty
          ; main_chain_transactions_posted= Merkle_trie.DigestSet.empty }
        in
        let trent_keys =
          Signing.make_keypair_from_hex
            "b6:fb:0b:7e:61:36:3e:e2:f7:48:16:13:38:f5:69:53:e8:aa:42:64:2e:99:90:ef:f1:7e:7d:e9:aa:89:57:86"
            "04:26:bd:98:85:f2:c9:e2:3d:18:c3:02:5d:a7:0e:71:a4:f7:ce:23:71:24:35:28:82:ea:fb:d1:cb:b1:e9:74:2c:4f:e3:84:7c:e1:a5:6a:0d:19:df:7a:7d:38:5a:21:34:be:05:20:8b:5d:1c:cc:5d:01:5f:5e:9a:3b:a0:d7:df"
        in
        let trent_genesis_state : FacilitatorState.t =
          { keypair= trent_keys
          ; current= confirmed_trent_state
          ; fee_schedule= trent_fee_schedule }
        in
        trent_genesis_state
    in
    let state_ref = ref facilitator_state in
    the_facilitator_service_ref := Some { address; state_ref };
    Lwt.async (const state_ref >>> inner_transaction_request_loop);
    Lwt.async user_transaction_request_loop;
    Lwt.async user_query_request_loop;
    Lwt.async admin_query_request_loop;
    Lwt_exn.return ()

module Test = struct
  let get_facilitator_state = get_facilitator_state
end

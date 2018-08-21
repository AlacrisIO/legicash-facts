open Lib
open Action
open Crypto
open Db
open Merkle_trie
open Main_chain
open Side_chain
open Lens.Infix

module FacilitatorAction = Action(FacilitatorState)
module FacilitatorAsyncAction = AsyncAction(FacilitatorState)

type account_lens = (FacilitatorState.t, AccountState.t) Lens.t

(* WARNING: GLOBAL STATE, so there's only one facilitator running *)
let user_request_mailbox : (Request.t signed * bool * Confirmation.t or_exn Lwt.u) Lwt_mvar.t =
  Lwt_mvar.create_empty ()

type validated_request =
  | Confirm of Request.t signed * (Confirmation.t * unit Lwt.t) or_exn Lwt.u
  | Flush of int

let validated_request_mailbox : validated_request Lwt_mvar.t = Lwt_mvar.create_empty ()

type facilitator_service =
  { address : Address.t
  ; state_ref : FacilitatorState.t ref }

let the_facilitator_service_ref : (facilitator_service option ref) = ref None

let get_facilitator_state () =
  !the_facilitator_service_ref |> Option.get |> fun service -> !(service.state_ref) |> Lwt.return

let facilitator_account_lens address =
  FacilitatorState.lens_current |-- State.lens_accounts
  |-- defaulting_lens (konstant AccountState.empty) (AccountMap.lens address)

let signed_request_requester rx = rx.payload.Request.rx_header.requester

exception Malformed_request of string

(** Check that the request is basically well-formed, or else fail
    This function should include all checks that can be made without any non-local side-effect
    beside reading pure or monotonic data, which is allowed for now
    (but may later have to be split to another function).
    Thus, we can later parallelize this check.
    TODO: parallelize the signature checking in a C worker thread that lets us do additional OCaml work.
*)
let check_side_chain_request_well_formed :
  (Request.t signed * bool, Request.t signed * bool) FacilitatorAction.arr =
  let open FacilitatorAction in
  fun x state ->
    let ({ payload ; signature }, is_forced) = x in
    let Request.{ rx_header={ requester; requester_revision }; operation } = payload in
    let AccountState.{balance; account_revision} = (facilitator_account_lens requester).get state in
    let FacilitatorState.{fee_schedule} = state in
    let check test exngen =
      fun x state ->
        if test then return x state
        else fail (Malformed_request (exngen ())) state in
    (fun arr -> arr x state)
      ((let open Revision in
        check (requester_revision = (add account_revision one))
          (fun () ->
             Printf.sprintf "You made a request with revision %s but the next expected revision is %s"
               (to_string requester_revision) (to_string (add account_revision one))))
       >>> check (is_signature_valid Request.digest requester signature payload)
             (fun () -> "The signature for the request doesn't match the requester")
       (* TODO: check confirmed main & side chain state + validity window *)
       >>> (* Check that the numbers add up: *)
       let open TokenAmount in
       match operation with
       | Deposit
           { deposit_amount
           ; deposit_fee
           ; main_chain_deposit_signed=
               { signature
               ; payload= {tx_header= {value}} as payload
               } as main_chain_deposit_signed
           ; main_chain_deposit_confirmation
           ; deposit_expedited } ->
         check (is_sum value deposit_amount deposit_fee)
           (fun () ->
              Printf.sprintf "Deposit amount %s and fee %s fail to add up to deposit value %s"
                (to_string deposit_amount) (to_string deposit_fee) (to_string value))
         >>> check (not (is_forced && deposit_expedited))
               (fun () -> "You cannot force an expedited deposit")
         >>> check (is_forced || compare deposit_fee fee_schedule.deposit_fee >= 0)
               (fun () -> Printf.sprintf "Insufficient deposit fee %s, requiring at least %s"
                            (to_string deposit_fee) (to_string fee_schedule.deposit_fee))
         (* TODO: use the same signature checking protocol to the main chain *)
         >>> check (is_signature_valid Transaction.digest requester signature payload)
               (fun () -> "The signature for the main chain deposit doesn't match the requester")
         >>> check (Main_chain.is_confirmation_valid
                      main_chain_deposit_confirmation main_chain_deposit_signed)
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

let make_request_confirmation : (Request.t signed, Confirmation.t) FacilitatorAction.arr =
  fun signed_request facilitator_state ->
    let current_state = facilitator_state.current in
    let new_revision = Revision.(add current_state.facilitator_revision one) in
    let confirmation =
      { tx_header=TxHeader.{ tx_revision= new_revision
                           ; updated_limit= facilitator_state.current.spending_limit }
      ; Confirmation.signed_request } in
    let requester = signed_request_requester signed_request in
    let account_lens = facilitator_account_lens requester in
    let new_requester_revision = signed_request.payload.rx_header.requester_revision in
    let new_facilitator_state =
      facilitator_state
      |> (account_lens |-- AccountState.lens_account_revision).set new_requester_revision
      |> (FacilitatorState.lens_current |-- State.lens_facilitator_revision).set new_revision
      |> Lens.modify (FacilitatorState.lens_current |-- State.lens_operations)
           (ConfirmationMap.add new_revision confirmation) in
    FacilitatorAction.return confirmation new_facilitator_state

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
      (main_chain_transaction_signed : Main_chain.TransactionSigned.t)
  : ('a, 'a) FacilitatorAction.arr =
  let witness = Main_chain.TransactionSigned.digest main_chain_transaction_signed in
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
let effect_request : ( Request.t signed, Request.t signed) FacilitatorAction.arr =
  fun rx ->
    let open FacilitatorAction in
    let requester = signed_request_requester rx in
    rx
    |> match rx.payload.operation with
    | Deposit { deposit_amount; deposit_fee; main_chain_deposit_signed; deposit_expedited } ->
      maybe_spend_spending_limit deposit_expedited deposit_amount
      >>> check_against_double_accounting main_chain_deposit_signed
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
let post_validated_request :
  ( Request.t signed * bool, (Confirmation.t * unit Lwt.t)) Lwt_exn.arr =
  fun (request, _is_forced) ->
    let open Lwt_monad in
    let (promise, resolver) = Lwt.task () in
    Lwt_mvar.put validated_request_mailbox (Confirm (request, resolver))
    >>= fun () -> promise

let process_validated_request : (Request.t signed, Confirmation.t) FacilitatorAction.arr =
  FacilitatorAction.(effect_request >>> make_request_confirmation)

(* Process a user request, with a flag to specify whether it's a forced request
   (published on the main chain), in which case there are no fee amount minima.

   We use the latest current state of the facilitator exported by the inner loop, but read-only,
   to check that the request is valid.
   If it is, we pass it to the inner loop, that will use it read-write.
   In the future, maybe moving away from Lwt and from single-threaded OCaml,
   and/or using forking and reducing the use of facilitator state so no DB access is needed,
   this could be done in different threads or processes.
*)
let process_request : (Request.t signed * bool, Confirmation.t) Lwt_exn.arr =
  fun (request, is_forced) ->
    ()
    |> let open Lwt_exn in
    (let open Lwt_monad in
     get_facilitator_state
     >>> FacilitatorAction.to_async check_side_chain_request_well_formed (request, is_forced)
     >>> (fun (result, _state) -> return result))
    >>> post_validated_request
    >>> fun (confirmation, wait_for_commit) ->
    let open Lwt in
    wait_for_commit >>= Lwt_exn.const confirmation

let commit_facilitator_state = bottom

let check_main_chain_for_exits = bottom

(** Take messages from the user_request_mailbox, and process them in parallel *)
let user_request_loop =
  let open Lwt_monad in
  forever
    (fun () ->
       Lwt_mvar.take user_request_mailbox
       >>= fun (request, is_forced, continuation) ->
       process_request (request, is_forced)
       >>= fun confirmation_or_exn ->
       Lwt.wakeup_later continuation confirmation_or_exn;
       Lwt.return_unit)

(** We assume that the operation will correctly apply:
    balances are sufficient for spending,
    deposits confirmation will check out,
    active revision will only increase, etc.
*)

(* TODO: tweak these numbers later *)
let batch_timeout_trigger_in_seconds = 0.01
let batch_size_trigger_in_requests = 1000

let validated_request_loop =
  let open Lwt in
  let open Lwt_monad in
  fun facilitator_state_ref ->
    return (!facilitator_state_ref, 0, return_unit)
    >>= forever
          (fun (facilitator_state, batch_id, previous) ->
             (* The promise sent back to requesters, that they have to wait on
                for their confirmation's batch to have been committed,
                and our private resolver for this batch. *)
             let (wait_on_batch_commit, notify_batch_commit) = Lwt.task () in
             (* An internal promise to detect if and when we trigger the batch based on size *)
             let (size_trigger, trigger_size) = Lwt.task () in
             (* An internal promise to detect if and when we trigger the batch based on time *)
             let timeout = Lwt_unix.sleep batch_timeout_trigger_in_seconds in
             (* When either trigger criterion is met, send ourselves a Flush message for this batch_id *)
             Lwt.async
               (fun () -> Lwt.join [previous; Lwt.pick [timeout; size_trigger]]
                 >>= (fun () -> Lwt_mvar.put validated_request_mailbox (Flush batch_id)));
             let rec request_batch facilitator_state triggered size =
               Lwt_mvar.take validated_request_mailbox
               >>= function
               | Confirm (request_signed, continuation) ->
                 process_validated_request request_signed facilitator_state
                 |> fun (confirmation_or_exn, new_facilitator_state) ->
                 facilitator_state_ref := new_facilitator_state;
                 Lwt.wakeup_later continuation
                   (match confirmation_or_exn with
                    | Ok confirmation -> Ok (confirmation, wait_on_batch_commit)
                    | Error e -> Error e);
                 let size = size + 1 in
                 if (not triggered) && (size >= batch_size_trigger_in_requests) then
                   begin
                     Lwt.wakeup_later trigger_size ();
                     request_batch new_facilitator_state true size
                   end
                 else
                   request_batch new_facilitator_state triggered size
               | Flush id ->
                 assert (id = batch_id);
                 Side_chain.FacilitatorState.save facilitator_state
                 >>= fun () -> Db.async_commit notify_batch_commit
                 >>= fun () -> Lwt.return (facilitator_state, (batch_id + 1), wait_on_batch_commit) in
             request_batch facilitator_state false 0)

let start_facilitator address =
  let open Lwt_monad in
  let open Lwt in
  match !the_facilitator_service_ref with
  | Some x ->
    if Address.equal x.address address then
      Lwt_io.printf
        "Facilitator service already running for address %s, not starting another one\n%!"
        (Address.to_0x_string address)
    else
      bork (Printf.sprintf
              "Cannot start a facilitator service for address %s because there's already one for %s"
              (Address.to_0x_string address) (Address.to_0x_string x.address))
  | None ->
    let facilitator_state = FacilitatorState.load address in
    let state_ref = ref facilitator_state in
    the_facilitator_service_ref := Some { address; state_ref };
    async (const state_ref >>> validated_request_loop);
    async user_request_loop;
    return_unit

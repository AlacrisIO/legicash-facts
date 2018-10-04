open Lens.Infix

open Legilogic_lib
open Lib
open Action
open Yojsoning
open Marshaling
open Signing
open Persisting
open Types
open Merkle_trie
open Json_rpc
open Trie

open Legilogic_ethereum

open Side_chain

(** TODO: query the network, whatever, and find the fee schedule *)
let get_facilitator_fee_schedule _facilitator_address =
  Lwt_exn.return initial_fee_schedule

(** TODO: find and justify a good default validity window in number of blocks *)
let default_validity_window = Duration.of_int 256

let stub_confirmed_main_chain_state = ref Ethereum_chain.genesis_state

let stub_confirmed_main_chain_state_digest = ref (Ethereum_chain.State.digest Ethereum_chain.genesis_state)

let stub_confirmed_side_chain_state = ref Side_chain.State.empty

let stub_confirmed_side_chain_state_digest = ref (State.digest Side_chain.State.empty)

[@@@warning "-32-27"]
let get_keypair_of_address user =
  Lwt_exn.catching_arr keypair_of_address user

(* TODO: ACTUALLY IMPLEMENT IT, MAYBE MOVE IT to side_chain.ml ? *)
let wait_for_facilitator_state_update revision =
  (*bork "wait_for_facilitator_state_update not implemented yet"; bottom ()*)
  (* THIS IS FAKE NEWS! *)
  Lwt_exn.return
    Ethereum_chain.Confirmation.
      { transaction_hash= Digest.zero
      ; transaction_index= Revision.zero
      ; block_number= Revision.zero
      ; block_hash= Digest.zero }


let make_rx_header user facilitator revision =
  Lwt.return RxHeader.
               { facilitator
               ; requester= user
               ; requester_revision= revision
               ; confirmed_main_chain_state_digest= !stub_confirmed_main_chain_state_digest
               ; confirmed_main_chain_state_revision= !stub_confirmed_main_chain_state.revision
               ; confirmed_side_chain_state_digest= !stub_confirmed_side_chain_state_digest
               ; confirmed_side_chain_state_revision=
                   !stub_confirmed_side_chain_state.facilitator_revision
               ; validity_within= default_validity_window }

let make_user_transaction_request user facilitator revision operation =
  let open Lwt_exn in
  of_lwt (make_rx_header user facilitator) revision
  >>= fun rx_header ->
  let request = UserTransactionRequest.{rx_header; operation} in
  get_keypair_of_address user
  >>= fun keypair ->
  SignedUserTransactionRequest.make keypair request |> return

module DepositWanted = struct
  [@@@warning "-39"]
  type t =
    { facilitator: Address.t
    ; deposit_amount: TokenAmount.t }
  [@@deriving yojson]
end

module PaymentWanted = struct
  [@@@warning "-39"]
  type t =
    { facilitator: Address.t
    ; recipient: Address.t
    ; amount: TokenAmount.t
    ; memo: string
    ; payment_expedited: bool }
  [@@deriving yojson]
end

module WithdrawalWanted = struct
  [@@@warning "-39"]
  type t =
    { facilitator: Address.t
    ; withdrawal_amount: TokenAmount.t }
  [@@deriving yojson]
end

module OngoingTransactionStatus = struct
  (* TODO: include a strong reference to the TransactionTracker, so it won't get garbage collected
     at just the wrong moment; make sure it can properly be persisted. Sigh. *)
  [@@@warning "-39"]
  type t =
    | DepositWanted of DepositWanted.t * TokenAmount.t
    | DepositPosted of DepositWanted.t * TokenAmount.t * Ethereum_user.TransactionTracker.Key.t
    | DepositConfirmed of DepositWanted.t * TokenAmount.t * Ethereum_chain.Transaction.t * Ethereum_chain.Confirmation.t
    | Requested of UserTransactionRequest.t signed
    | SignedByFacilitator of TransactionCommitment.t
    | PostedToRegistry of TransactionCommitment.t
    | PostedToMainChain of TransactionCommitment.t * Ethereum_chain.Confirmation.t
    | ConfirmedOnMainChain of TransactionCommitment.t * Ethereum_chain.Confirmation.t
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
  let signed_request_opt : t -> UserTransactionRequest.t signed option = function
    | DepositWanted _ | DepositPosted _ | DepositConfirmed _ -> None
    | Requested signed_request -> Some signed_request
    | SignedByFacilitator tc | PostedToRegistry tc
    | PostedToMainChain (tc, _) | ConfirmedOnMainChain (tc, _) ->
      tc.transaction.tx_request |> TransactionRequest.signed_request |> Option.return
  let signed_request = signed_request_opt >> Option.get
  let request_opt : t -> UserTransactionRequest.t option =
    signed_request_opt >> Option.map (fun x -> x.payload)
  let request = request_opt >> Option.get
  let status_facilitator = function
    | DepositWanted (w, _) | DepositPosted (w, _, _) | DepositConfirmed (w, _, _, _) ->
      w.DepositWanted.facilitator
    | x -> (request x).rx_header.facilitator
end

module FinalTransactionStatus = struct
  type t =
    | SettledOnMainChain of TransactionCommitment.t * Ethereum_chain.Confirmation.t
    | Failed of OngoingTransactionStatus.t * exn
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
  let signed_request_opt : t -> UserTransactionRequest.t signed option = function
    | SettledOnMainChain (tc, _) -> tc.transaction.tx_request |> TransactionRequest.signed_request |> Option.return
    | Failed (ts, _) -> OngoingTransactionStatus.signed_request_opt ts
end

module TransactionStatus = struct
  type t =
    | Ongoing of OngoingTransactionStatus.t
    | Final of FinalTransactionStatus.t
  [@@deriving yojson]
  module P = struct
    type nonrec t = t
    let yojsoning = {to_yojson;of_yojson}
  end
  include (YojsonPersistable (P) : PersistableS with type t := t)

  let signed_request_opt : t -> UserTransactionRequest.t signed option = function
    | Ongoing x -> OngoingTransactionStatus.signed_request_opt x
    | Final x -> FinalTransactionStatus.signed_request_opt x
  let signed_request = signed_request_opt >> Option.get
  let request_opt : t -> UserTransactionRequest.t option =
    signed_request_opt >> Option.map (fun x -> x.payload)
  let request = request_opt >> Option.get
end

exception TransactionFailed of OngoingTransactionStatus.t * exn

type revision_generator = (unit, Revision.t) Lwter.arr

module TransactionTracker = struct
  module Base = struct
    module Key = struct
      [@@@warning "-39"]
      type t= { user : Address.t; facilitator : Address.t; revision : Revision.t } [@@deriving yojson]
      include (YojsonMarshalable(struct
                 type nonrec t = t
                 let yojsoning = {to_yojson;of_yojson}
                 let marshaling = marshaling3
                                    (fun {user;facilitator;revision} -> user,facilitator,revision)
                                    (fun user facilitator revision -> {user;facilitator;revision})
                                    Address.marshaling Address.marshaling Revision.marshaling
               end): YojsonMarshalableS with type t := t)
    end
    type key = Key.t
    let key_prefix = "ALTT"
    type context = revision_generator
    module State = TransactionStatus
    type state = State.t
    let make_default_state = persistent_actor_no_default_state key_prefix Key.to_yojson_string

    (* TODO: inspection? cancellation? split private and public activities? *)
    type t = Key.t * FinalTransactionStatus.t Lwt.t

    open Lwter
    let make_activity revision_generator key saving state =
      let Key.{user; facilitator; revision} = key in
      let rec update (status : TransactionStatus.t) =
        saving status >>= Db.committing >>= loop
      and continue (status : OngoingTransactionStatus.t) =
        TransactionStatus.Ongoing status |> update
      and finalize (status : FinalTransactionStatus.t) =
        (* TODO: remove the request from the ongoing_transactions set!
           -- this requires some function added to the context! *)
        TransactionStatus.Final status |> update
      and invalidate transaction_status error =
        finalize (Failed (transaction_status, error))
      and loop (status: TransactionStatus.t) : FinalTransactionStatus.t Lwt.t =
        match status with
        | Ongoing ongoing ->
          let open OngoingTransactionStatus in
          (match ongoing with
           | DepositWanted (({facilitator; deposit_amount} as deposit_wanted), deposit_fee) ->
             let pre_transaction =
               TokenAmount.(add deposit_amount deposit_fee)
               |> Facilitator_contract.pre_deposit ~facilitator in
             (* TODO: have a single transaction for queueing the Wanted and the DepositPosted *)
             (Ethereum_user.(user_action user add_ongoing_transaction (Wanted pre_transaction))
              >>= function
              | Error error -> invalidate ongoing error
              | Ok (tracker_key, _) ->
                DepositPosted (deposit_wanted, deposit_fee, tracker_key) |> continue)
           | DepositPosted (deposit_wanted, deposit_fee, tracker_key) ->
             let (_, promise) = Ethereum_user.TransactionTracker.get () tracker_key in
             (promise >>= function
              | Failed (_, error) -> invalidate ongoing error (* TODO: keep the ethereum ongoing transaction status? *)
              | Confirmed (transaction, confirmation) ->
                DepositConfirmed (deposit_wanted, deposit_fee, transaction, confirmation) |> continue)
           | DepositConfirmed ({deposit_amount}, deposit_fee,
                               main_chain_deposit, main_chain_deposit_confirmation) ->
             revision_generator () >>= fun revision ->
             (make_user_transaction_request user facilitator revision
                (Deposit
                   { deposit_amount
                   ; deposit_fee
                   ; main_chain_deposit
                   ; main_chain_deposit_confirmation })
              >>= function
              | Ok request -> Requested request |> continue
              | Error error -> invalidate ongoing error)
           | Requested request ->
             (* TODO: handle retries *)
             (request
              |> Side_chain_client.post_user_transaction_request
              >>= function
              | Ok tc -> SignedByFacilitator tc |> continue
              | Error error -> invalidate ongoing error)
           | SignedByFacilitator tc ->
             (* TODO: add support for Shared Knowledge Network / "Smart Court Registry" *)
             PostedToRegistry tc |> continue
           | PostedToRegistry tc ->
             (* TODO: add support for Shared Knowledge Network / "Smart Court Registry" *)
             (* TODO: add support for waiting for a state update from the facilitator *)
             (wait_for_facilitator_state_update tc.facilitator_revision
              >>= function
              | Ok c ->
                (match (tc.transaction.tx_request |> TransactionRequest.request).operation with
                 | Deposit _ | Payment _ -> FinalTransactionStatus.SettledOnMainChain (tc, c) |> finalize
                 | Withdrawal _ -> PostedToMainChain (tc, c) |> continue)
              | Error error -> invalidate ongoing error)
           | PostedToMainChain (tc, confirmation) ->
             (* Withdrawal that we're going to have to claim *)
             (*bottom ()*)
             ConfirmedOnMainChain (tc, confirmation) |> continue
           | ConfirmedOnMainChain (tc, confirmation) ->
             (* Confirmed Withdrawal that we're going to have to execute *)
             (*bottom ()*)
             FinalTransactionStatus.SettledOnMainChain (tc, confirmation) |> finalize)
        | Final x -> return x in
      key, loop state
  end
  include PersistentActivity(Base)
  module Key = Base.Key
  module State = Base.State
  let wait promise =
    let open Lwter in
    promise >>= function
    | FinalTransactionStatus.SettledOnMainChain (t, c) -> Lwt_exn.return (t, c)
    | FinalTransactionStatus.Failed (o, e) -> Lwt_exn.fail (TransactionFailed (o, e))
end

module UserAccountState = struct
  [@warning "-39"]
  type t =
    { is_facilitator_valid: bool
    ; confirmed_state: AccountState.t
    ; side_chain_revision: Revision.t
    ; transaction_counter: Revision.t
    ; ongoing_transactions: RevisionSet.t }
  [@@deriving lens { prefix=true }, yojson ]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling5
        (fun { is_facilitator_valid
             ; confirmed_state
             ; side_chain_revision
             ; transaction_counter
             ; ongoing_transactions } ->
          is_facilitator_valid, confirmed_state, side_chain_revision,
          transaction_counter, ongoing_transactions)
        (fun is_facilitator_valid confirmed_state side_chain_revision
          transaction_counter ongoing_transactions ->
          { is_facilitator_valid
          ; confirmed_state
          ; side_chain_revision
          ; transaction_counter
          ; ongoing_transactions })
        bool_marshaling AccountState.marshaling Revision.marshaling
        Revision.marshaling RevisionSet.marshaling
    let walk_dependencies = no_dependencies
    let make_persistent = normal_persistent
    let yojsoning = {to_yojson;of_yojson}
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
  let empty =
    { is_facilitator_valid= true
    ; confirmed_state= AccountState.empty
    ; side_chain_revision= Revision.zero
    ; transaction_counter = Revision.zero
    ; ongoing_transactions= RevisionSet.empty }
end

module UserAccountStateMap = MerkleTrie (Address) (UserAccountState)

module UserState = struct
  [@warning "-39"]
  type t =
    { address: Address.t
    ; facilitators: UserAccountStateMap.t
    ; notification_counter: Revision.t
    ; notifications: (Revision.t * yojson) list }
  [@@deriving lens { prefix=true }, yojson]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling4
        (fun { address; facilitators; notification_counter; notifications } ->
           address, facilitators, notification_counter, notifications)
        (fun address facilitators notification_counter notifications ->
           { address; facilitators; notification_counter; notifications })
        Address.marshaling UserAccountStateMap.marshaling Revision.marshaling
        (list_marshaling (marshaling2 identity pair Revision.marshaling yojson_marshaling))
    let walk_dependencies = no_dependencies
    let make_persistent = normal_persistent
    let yojsoning = {to_yojson;of_yojson}
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
end

module UserAsyncAction = AsyncAction(UserState)

let facilitator_lens : Address.t -> (UserState.t, UserAccountState.t) Lens.t =
  fun facilitator ->
    UserState.lens_facilitators |--
    defaulting_lens (konstant UserAccountState.empty)
      (UserAccountStateMap.lens facilitator)

let get_next_account_revision : Address.t -> unit -> UserState.t -> (Revision.t * UserState.t) Lwt.t =
  fun facilitator () state ->
    let revision_lens = facilitator_lens facilitator |-- UserAccountState.lens_side_chain_revision in
    let revision = revision_lens.get state in
    Lwt.return (revision, (state |> revision_lens.set Revision.(add one revision)))

module User = struct
  module Base = struct
    module Key = Address
    let key_prefix = "ALUS"
    module State = UserState
    type t = State.t SimpleActor.t
    type context = Key.t -> t (* The get function its own context to pass around! *)
    let make_default_state _context user =
      UserState.
        { address= user
        ; facilitators= UserAccountStateMap.empty
        ; notification_counter= Revision.zero
        ; notifications= [] }
    let next_side_chain_revision user_actor user facilitator =
      SimpleActor.action (user_actor user) (get_next_account_revision facilitator)
    let resume_transactions user_actor user (state : State.t) =
      flip UserAccountStateMap.iter state.facilitators
        (fun facilitator account ->
           let revision_generator = next_side_chain_revision user_actor user facilitator in
           flip RevisionSet.iter account.ongoing_transactions
             (fun revision ->
                TransactionTracker.get revision_generator {user; facilitator; revision} |> ignore))
    let make_activity user_actor user saving state =
      let with_transaction transform = Lwter.(transform >>> saving) in
      let actor = SimpleActor.make ~with_transaction state in
      (* TODO: maybe just use Lwt_mvar.create state and leave it to users to transact on it ? *)
      resume_transactions user_actor user state; (* TODO: pass the actor as context to that? *)
      actor
  end
  include PersistentActivity(Base)
  module Key = Base.Key
  module State = Base.State
  (* Tie the knot for the fix point to pass as context *)
  let rec user_actor user = get user_actor user
  let make_tracker_context = Base.next_side_chain_revision user_actor
  let action user = SimpleActor.action (user_actor user)
  let transaction user transaction parameters =
    Lwt_exn.(action user transaction parameters >>= fun (_, tracker_promise) ->
             TransactionTracker.wait tracker_promise)
end

let add_ongoing_side_chain_transaction :
  (OngoingTransactionStatus.t, TransactionTracker.t) UserAsyncAction.arr =
  fun transaction_status user_state ->
    let user = user_state.address in
    let facilitator = transaction_status |> OngoingTransactionStatus.status_facilitator in
    let revision_lens = (facilitator_lens facilitator |-- UserAccountState.lens_transaction_counter) in
    let revision = revision_lens.get user_state in
    let open Lwter in
    TransactionTracker.(make (User.make_tracker_context user facilitator)
                          Key.{user; facilitator; revision}
                          ((|>) (TransactionStatus.Ongoing transaction_status)))
    >>= fun tracker ->
    UserAsyncAction.return tracker
      (user_state
       |> revision_lens.set Revision.(add one revision)
       |> (facilitator_lens facilitator |-- UserAccountState.lens_ongoing_transactions
           |-- RevisionSet.lens revision).set true)

let deposit_fee_for FacilitatorFeeSchedule.{deposit_fee} _deposit_amount =
  deposit_fee

let payment_fee_for FacilitatorFeeSchedule.{fee_per_billion} payment_amount =
  TokenAmount.(div (mul fee_per_billion payment_amount) one_billion_tokens)

let withdrawal_fee_for FacilitatorFeeSchedule.{withdrawal_fee} _withdrawal_amount =
  withdrawal_fee

let deposit DepositWanted.{facilitator; deposit_amount} =
  let open UserAsyncAction in
  of_lwt_exn get_facilitator_fee_schedule facilitator
  >>= fun fee_schedule ->
  let deposit_fee = deposit_fee_for fee_schedule deposit_amount in
  let status = OngoingTransactionStatus.DepositWanted ({facilitator; deposit_amount}, deposit_fee) in
  add_ongoing_side_chain_transaction status

let get_user_address : (unit, Address.t) UserAsyncAction.arr =
  fun () user_state -> UserAsyncAction.return user_state.UserState.address user_state

let direct_operation :
  Address.t -> (FacilitatorFeeSchedule.t -> UserOperation.t, TransactionTracker.t) UserAsyncAction.arr =
  fun facilitator make_operation ->
    let open UserAsyncAction in
    of_lwt_exn get_facilitator_fee_schedule facilitator >>= fun fee_schedule ->
    let operation = make_operation fee_schedule in
    of_lwt_state (get_next_account_revision facilitator) () >>= fun revision ->
    get_user_address () >>= fun user ->
    of_lwt_exn (make_user_transaction_request user facilitator revision) operation
    >>= fun signed_request ->
    let status = OngoingTransactionStatus.Requested signed_request in
    add_ongoing_side_chain_transaction status

let payment PaymentWanted.{facilitator; recipient; amount; memo; payment_expedited} =
  direct_operation facilitator
    (fun fee_schedule ->
       let payment_invoice = Invoice.{recipient; amount; memo} in
       let payment_fee = payment_fee_for fee_schedule amount in
       UserOperation.Payment {payment_invoice; payment_fee; payment_expedited})

let withdrawal WithdrawalWanted.{facilitator; withdrawal_amount} =
  direct_operation facilitator
    (fun fee_schedule ->
       let withdrawal_fee = withdrawal_fee_for fee_schedule withdrawal_amount in
       UserOperation.Withdrawal {withdrawal_amount; withdrawal_fee})

(*
   let remove_ongoing_transaction facilitator revision user_state =
   (facilitator_lens facilitator
   |-- UserAccountState.lens_ongoing_transactions
   |-- RevisionSet.lens revision).set false user_state

   let get_first_facilitator_state_option :
   (unit, (Address.t * UserAccountState.t) option) UserAsyncAction.readonly =
   fun () user_state ->
   UserAccountStateMap.find_first_opt (konstant true) user_state.facilitators

   let get_first_facilitator =
   UserAsyncAction.(of_readonly get_first_facilitator_state_option
   >>> function
   | None -> fail No_facilitator_yet
   | Some (address, _) -> return address)

   (* TODO: is this used? should balances and revisions be updated in effect_request?
   looks like balances already are *)

   let update_account_state_with_trusted_operation
   trusted_operation ({balance} as account_state : AccountState.t) =
   let f =
   {account_state with account_revision= Revision.add account_state.account_revision Revision.one} in
   match trusted_operation with
   | Operation.Deposit {deposit_amount; deposit_fee=_deposit_fee} ->
   if true (\* check that everything is correct *\) then
   {f with balance= TokenAmount.add balance deposit_amount}
   else bork "I mistrusted your deposit operation"
   | Operation.Payment {payment_invoice; payment_fee} ->
   let decrement = TokenAmount.add payment_invoice.amount payment_fee in
   if TokenAmount.compare balance decrement >= 0 then
   {f with balance= TokenAmount.sub balance decrement}
   else bork "I mistrusted your payment operation"
   | Operation.Withdrawal {withdrawal_amount; withdrawal_fee} ->
   if true (\* check that everything is correct *\) then
   {f with balance= TokenAmount.sub balance (TokenAmount.add withdrawal_amount withdrawal_fee)}
   else bork "I mistrusted your withdrawal operation"

   (** We assume most recent operation is to the left of the changes list, *)
   let update_account_state_with_trusted_operations trusted_operations account_state =
   List.fold_right update_account_state_with_trusted_operation trusted_operations account_state

   let [@warning "-32"] optimistic_facilitator_account_state facilitator user_state =
   match UserAccountStateMap.find_opt facilitator user_state.UserState.facilitators with
   | None -> AccountState.empty
   | Some {is_facilitator_valid; confirmed_state; pending_operations} ->
   match is_facilitator_valid with
   | Rejected -> confirmed_state
   | _ ->
   update_account_state_with_trusted_operations
   (List.map (fun x -> x.TransactionStatus.request.payload.operation) pending_operations)
   confirmed_state

   let ethereum_action : ('i, 'o) Ethereum_user.UserAsyncAction.arr -> ('i, 'o) UserAsyncAction.arr =
   fun action input user_state ->
   UserAsyncAction.of_lwt_exn
   (Ethereum_user.user_action user_state.UserState.address action) input user_state

   (* TODO: find the actual gas limit *)
   let withdrawal_gas_limit = TokenAmount.of_int 1000000

   (* in Lwt monad, because we'll push the request to the main chain *)
   let withdrawal (facilitator, withdrawal_amount) =
   let open UserAsyncAction in
   of_lwt_exn get_facilitator_fee_schedule facilitator
   >>= fun {withdrawal_fee} ->
   issue_user_transaction_request
   (Withdrawal { withdrawal_amount ; withdrawal_fee })


   (** Given a withdrawal on the side chain, reflect that on the main chain
   We should be signing the RLP, not the marshaling! *)
   let make_main_chain_withdrawal_transaction :
   address -> (UserOperation.withdrawal_details, Ethereum_chain.Transaction.t * Ethereum_json_rpc.SignedTransaction.t) Ethereum_user.UserAsyncAction.arr =
   fun facilitator UserOperation.{withdrawal_amount;withdrawal_fee} state ->
   (* TODO: should the withdrawal fee agree with the facilitator state fee schedule? where to enforce? *)
   let ticket = Revision.zero in (* TODO: implement ticketing *)
   let confirmed_state = Digest.zero in (* TODO: is this just a digest of the facilitator state here? *)
   let bond = TokenAmount.zero in (* TODO: where does this come from? *)
   let operation = Facilitator_contract.make_withdraw_call
   facilitator ticket bond confirmed_state in
   let value = TokenAmount.sub withdrawal_amount withdrawal_fee in
   Ethereum_user.(UserAsyncAction.of_lwt_exn
   (make_signed_transaction state.UserState.address operation value) withdrawal_gas_limit)
   state

   let push_side_chain_withdrawal_to_main_chain
   (facilitator : Address.t)
   (transaction : Transaction.t) =
   let request = transaction.tx_request |> TransactionRequest.request in
   (* We assume it's a transaction of the current user, that the facilitator committed to *)
   match request.operation with
   | Withdrawal details ->
   details
   |> ethereum_action
   Ethereum_user.UserAsyncAction.
   (make_main_chain_withdrawal_transaction facilitator
   >>> Ethereum_user.confirm_transaction)
   | Payment _
   | Deposit _ ->
   bork "Side chain transaction does not need subsequent interaction with main chain"

   let add_notification notification_type to_yojson x state =
   let counter = state.UserState.notification_counter in
   UserAsyncAction.return counter
   (state
   |> UserState.lens_notification_counter.set Revision.(add one counter)
   |> Lens.modify UserState.lens_notifications
   (List.cons (counter, `List [`String notification_type; to_yojson x])))

   let add_error_notification = add_notification "error_notification" ErrorNotification.to_yojson

   let _notify_error status error = add_error_notification {status;error}
*)

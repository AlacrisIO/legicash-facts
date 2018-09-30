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

open Legilogic_ethereum

open Side_chain

module DepositWanted = struct
  [@@@warning "-39"]
  type t =
    { facilitator: Address.t
    ; deposit_amount: TokenAmount.t
    ; deposit_fee: TokenAmount.t }
  [@@deriving yojson]
end

module FinalTransactionStatus = struct
  type t =
    [ `SettledOnMainChain of TransactionCommitment.t * Ethereum_chain.Confirmation.t
    | `Failed of yojson ]
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end

module TransactionStatus = struct
  type t =
    [ `DepositWanted of DepositWanted.t
    | `DepositPosted of DepositWanted.t * Ethereum_user.TransactionTracker.Key.t
    | `DepositConfirmed of DepositWanted.t * Ethereum_chain.Transaction.t * Ethereum_chain.Confirmation.t
    | `Requested of UserTransactionRequest.t signed
    | `SignedByFacilitator of TransactionCommitment.t
    | `PostedToRegistry of TransactionCommitment.t
    | `PostedToMainChain of TransactionCommitment.t * Ethereum_chain.Confirmation.t
    | `ConfirmedOnMainChain of TransactionCommitment.t * Ethereum_chain.Confirmation.t
    | FinalTransactionStatus.t ]
  [@@deriving yojson]

  let signed_request_opt : t -> UserTransactionRequest.t signed option = function
    | `DepositWanted _ | `DepositPosted _ | `DepositConfirmed _ | `Failed _ -> None
    | `Requested signed_request -> Some signed_request
    | `SignedByFacilitator tc | `PostedToRegistry tc
    | `PostedToMainChain (tc, _) | `ConfirmedOnMainChain (tc, _) | `SettledOnMainChain (tc, _) ->
      tc.transaction.tx_request |> TransactionRequest.signed_request |> Option.return
  let signed_request = signed_request_opt >> Option.get

  let request_opt : t -> UserTransactionRequest.t option =
    signed_request_opt >> Option.map (fun x -> x.payload)
  let request = request_opt >> Option.get

  module P = struct
    type nonrec t = t
    let yojsoning = {to_yojson;of_yojson}
  end
  include (YojsonPersistable (P) : PersistableS with type t := t)
end

module AccountRevisionTracker = struct
  type t =
    { user : Address.t
    ; facilitator : Address.t
    ; actor : Revision.t SimpleActor.t }
(*
       let make ~user ~facilitator revision =
       let actor = SimpleActor.make ~save:(
       {user;facilitator;actor}
    *)
end

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
    type context = unit (* TODO: Revision tracking actor *)
    module State = TransactionStatus
    type state = State.t
    (* TODO: inspection? cancellation? split private and public activities? *)
    type t = FinalTransactionStatus.t Lwt.t
    let behavior =
      Synchronous
        (fun _context _key _actor ->
           let (promise, _notify) = Lwt.task () in
           (* TODO XXXXXX *)
           promise, Lwter.const_unit)
    let make_default_state = persistent_actor_no_default_state key_prefix Key.to_yojson_string
  end
  include PersistentActivity(Base)
(*
       let trackers : (Address.t * Address.t * Revision.t, t) Hashtbl.t = Hashtbl.create 64
       [@@@warning "-32-27"]
       let make : user:Address.t -> facilitator:Address.t -> Revision.t ->
       nonce_tracker:(Revision.t SimpleActor.t) ->
       TransactionStatus.t -> t =
       fun ~user ~facilitator revision ~nonce_tracker status ->
       let db_key = db_key ~user ~facilitator revision in
       let open Lwt in
       let open TransactionStatus in
       let status_ref = ref status in
       let get () = !status_ref in
       let rec continue (status : TransactionStatus.t) : FinalTransactionStatus.t Lwt.t =
       match status with
       | `DepositWanted ({facilitator; deposit_amount; deposit_fee} as deposit_wanted) ->
       Db.with_transaction
       (fun () ->
       TokenAmount.(add deposit_amount deposit_fee)
       |> Facilitator_contract.pre_deposit ~facilitator
       |> fun pre_transaction ->
       (* TODO: have a single transaction for queueing the `Wanted and the `DepositPosted *)
       Ethereum_user.(user_action user add_ongoing_transaction (`Wanted pre_transaction))
       >>= ((function
       | Error error -> invalidate ~deposit_wanted ~error
       | Ok tracker -> `DepositPosted (deposit_wanted, tracker)) >> update))
       | `DepositPosted (deposit_wanted, (tracker: Ethereum_user.TransactionTracker.t)) ->
       tracker.promise
       >>= ((function
       | `Invalidated (tx, yo) -> Lib.bork "TODO: invalidate ~deposit_wanted ~error"
       | `Confirmed (transaction, confirmation) ->
       `DepositConfirmed (deposit_wanted, transaction, confirmation)) >> update)
       | `DepositConfirmed (deposit_wanted, transaction, confirmation) ->
       bottom ()
       (*           with_transaction
       (fun () ->
       make_user_transaction_request user facilitator operation
       >>= 
       >>= fun rx_header -> return UserTransactionRequest.{rx_header; operation}
       >>= sign_request

       xxx)
       make_rx_header facilitator
       >>= 
       Requested of UserTransactionRequest.t signed
       ) >> update)
     *)
       | _ -> bottom ()
       and invalidate ?deposit_wanted ~error =
       ignore deposit_wanted;
       `Failed (`Assoc [("error", Ethereum_user.exn_to_yojson error)])
       and update (status : TransactionStatus.t) : FinalTransactionStatus.t Lwt.t =
       Db.put db_key (marshal_string status)
       >>= Db.commit
       >>= fun () ->
       status_ref := status;
       continue status in
       {user; facilitator; revision; promise= continue status; get}
       |> fun tracker -> Hashtbl.replace trackers (user, facilitator, revision) tracker; tracker
       let load ~user ~facilitator revision =
       db_key ~user ~facilitator revision
       |> Db.get
       |> Option.get
       |> TransactionStatus.unmarshal_string
       |> bottom ()
       (*|> make ~user ~facilitator revision XXXXXXX*)
       let get user facilitator revision =
       memoize ~table:trackers
       (fun (user, facilitator, revision) -> load ~user ~facilitator revision)
       (user, facilitator, revision)
       module P = struct
       type nonrec t = t
       let marshaling =
       marshaling3
       (fun {user; facilitator; revision} -> user, facilitator, revision) get
       Address.marshaling Address.marshaling Revision.marshaling
       let yojsoning = yojsoning_of_marshaling marshaling
       end
       include (TrivialPersistable (P) : PersistableS with type t := t)
    *)
end

module UserAccountState = struct
  [@warning "-39"]
  type t =
    { is_facilitator_valid: bool
    ; confirmed_state: AccountState.t
    ; pending_operations: TransactionStatus.t list }
  [@@deriving lens { prefix=true }, yojson ]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling3
        (fun { is_facilitator_valid
             ; confirmed_state
             ; pending_operations } ->
          is_facilitator_valid, confirmed_state, pending_operations)
        (fun is_facilitator_valid confirmed_state pending_operations ->
           { is_facilitator_valid
           ; confirmed_state
           ; pending_operations })
        bool_marshaling AccountState.marshaling
        (list_marshaling TransactionStatus.marshaling)
    let walk_dependencies = no_dependencies
    let make_persistent = normal_persistent
    let yojsoning = {to_yojson;of_yojson}
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
  let empty =
    { is_facilitator_valid= true
    ; confirmed_state=AccountState.empty
    ; pending_operations= [] }
end

module UserAccountStateMap = MerkleTrie (Address) (UserAccountState)

exception User_not_found of string

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
  let user_state_key user_address =
    "LCUS0001" ^ (Address.to_big_endian_bits user_address)
  let save user_state =
    Lwt.(save user_state (* <-- use inherited binding *)
         >>= (fun () ->
           Db.put (user_state_key user_state.address)
             (Digest.to_big_endian_bits (digest user_state))))
  let load user_address =
    user_address |> user_state_key |> Db.get
    |> (function
      | Some x -> x
      | None -> raise (User_not_found
                         (Printf.sprintf "User %s not found in the database"
                            (Address.to_0x_string user_address))))
    |> Digest.unmarshal_string |> db_value_of_digest unmarshal_string
end

module UserAction = Action(UserState)
module UserAsyncAction = AsyncAction(UserState)

let get_first_facilitator_state_option :
  (unit, (Address.t * UserAccountState.t) option) UserAction.readonly =
  fun () user_state ->
    UserAccountStateMap.find_first_opt (konstant true) user_state.facilitators

let get_first_facilitator =
  UserAction.(of_readonly get_first_facilitator_state_option
              >>> function
              | None -> fail No_facilitator_yet
              | Some (address, _) -> return address)

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

module UserRevisionTracker = struct
  type t = Revision.t
end

(*
   let make_user_transaction_request user facilitator operation =
   let open Lwt_exn in
   let revision
   >>= fun rx_header ->
   return UserTransactionRequest.
   { rx_header=
   RxHeader.
   { facilitator
   ; requester= user_state.address
   ; requester_revision=
   Revision.add account_state.confirmed_state.account_revision
   (Revision.of_int (1 + List.length account_state.pending_operations))
   ; confirmed_main_chain_state_digest= !stub_confirmed_main_chain_state_digest
   ; confirmed_main_chain_state_revision= !stub_confirmed_main_chain_state.revision
   ; confirmed_side_chain_state_digest= !stub_confirmed_side_chain_state_digest
   ; confirmed_side_chain_state_revision=
   !stub_confirmed_side_chain_state.facilitator_revision
   ; validity_within= default_validity_window }
   user_state
   ; operation }
   >>= fun request ->
   get_keypair_of_address user
   >>= fun keypair ->
   SignedUserTransactionRequest.make keypair request
*)

let make_rx_header facilitator user_state =
  let open UserAction in
  match UserAccountStateMap.find_opt facilitator user_state.UserState.facilitators with
  | None -> fail Not_found user_state
  | Some account_state ->
    return
      RxHeader.{ facilitator
               ; requester= user_state.address
               ; requester_revision=
                   Revision.add account_state.confirmed_state.account_revision
                     (Revision.of_int (1 + List.length account_state.pending_operations))
               ; confirmed_main_chain_state_digest= !stub_confirmed_main_chain_state_digest
               ; confirmed_main_chain_state_revision= !stub_confirmed_main_chain_state.revision
               ; confirmed_side_chain_state_digest= !stub_confirmed_side_chain_state_digest
               ; confirmed_side_chain_state_revision=
                   !stub_confirmed_side_chain_state.facilitator_revision
               ; validity_within= default_validity_window }
      user_state

let transaction_status_of_request rx = `Requested rx

let facilitator_lens : Address.t -> (UserState.t, UserAccountState.t) Lens.t =
  fun facilitator ->
    UserState.lens_facilitators |--
    defaulting_lens (konstant UserAccountState.empty)
      (UserAccountStateMap.lens facilitator)

(** TODO: Handle cases of updates to previous transaction_statuss, rather than just new ones *)
let add_user_transaction_status transaction_status user_state =
  let facilitator = (transaction_status |> TransactionStatus.request).rx_header.facilitator in
  Lens.modify
    (facilitator_lens facilitator |-- UserAccountState.lens_pending_operations)
    (fun ops -> transaction_status::ops) (* TODO: replays, retries...*)
    user_state

let remove_user_request signed_request user_state =
  let facilitator = signed_request.payload.UserTransactionRequest.rx_header.facilitator in
  Lens.modify
    (facilitator_lens facilitator |-- UserAccountState.lens_pending_operations)
    (List.filter (fun x -> x |> TransactionStatus.signed_request_opt != Some signed_request))
    user_state

let sign_request request user_state =
  UserAction.return
    (SignedUserTransactionRequest.make
       (keypair_of_address user_state.UserState.address)
       request)
    user_state

let add_pending_request request state =
  let transaction_status = transaction_status_of_request request in
  UserAction.return request (add_user_transaction_status transaction_status state)

let mark_request_rejected request state =
  UserAction.return () (remove_user_request request state)

let issue_user_transaction_request operation =
  let open UserAction in
  get_first_facilitator ()
  >>= make_rx_header
  >>= fun rx_header -> return UserTransactionRequest.{rx_header; operation}
  >>= sign_request
  >>= add_pending_request

(* TODO: is this used? should balances and revisions be updated in effect_request?
   looks like balances already are
*)
(* let update_account_state_with_trusted_operation
 *       trusted_operation ({balance} as account_state : AccountState.t) =
 *   let f =
 *     {account_state with account_revision= Revision.add account_state.account_revision Revision.one} in
 *   match trusted_operation with
 *   | Operation.Deposit {deposit_amount; deposit_fee=_deposit_fee} ->
 *     if true (\* check that everything is correct *\) then
 *       {f with balance= TokenAmount.add balance deposit_amount}
 *     else bork "I mistrusted your deposit operation"
 *   | Operation.Payment {payment_invoice; payment_fee} ->
 *     let decrement = TokenAmount.add payment_invoice.amount payment_fee in
 *     if TokenAmount.compare balance decrement >= 0 then
 *       {f with balance= TokenAmount.sub balance decrement}
 *     else bork "I mistrusted your payment operation"
 *   | Operation.Withdrawal {withdrawal_amount; withdrawal_fee} ->
 *     if true (\* check that everything is correct *\) then
 *       {f with balance= TokenAmount.sub balance (TokenAmount.add withdrawal_amount withdrawal_fee)}
 *     else bork "I mistrusted your withdrawal operation"
 *
 * (\** We assume most recent operation is to the left of the changes list,
 * *\)
 * let update_account_state_with_trusted_operations trusted_operations account_state =
 *   List.fold_right update_account_state_with_trusted_operation trusted_operations account_state
 *
 * let [@warning "-32"] optimistic_facilitator_account_state facilitator user_state =
 *   match UserAccountStateMap.find_opt facilitator user_state.UserState.facilitators with
 *   | None -> AccountState.empty
 *   | Some {is_facilitator_valid; confirmed_state; pending_operations} ->
 *     match is_facilitator_valid with
 *     | Rejected -> confirmed_state
 *     | _ ->
 *       update_account_state_with_trusted_operations
 *         (List.map (fun x -> x.TransactionStatus.request.payload.operation) pending_operations)
 *         confirmed_state *)

let ethereum_action : ('i, 'o) Ethereum_user.UserAsyncAction.arr -> ('i, 'o) UserAsyncAction.arr =
  fun action input user_state ->
    UserAsyncAction.of_lwt_exn
      (Ethereum_user.user_action user_state.UserState.address action) input user_state

(* TODO: make this asynchronous rather than synchronous by
   saving the signed transaction from make_deposit before we send it,
   so we never send two different deposits due to a persistence race with a crash. *)
let deposit (facilitator, deposit_amount) =
  let open UserAsyncAction in
  of_lwt_exn get_facilitator_fee_schedule facilitator
  >>= fun {deposit_fee} ->
  ethereum_action Facilitator_contract.deposit (facilitator, (TokenAmount.add deposit_amount deposit_fee))
  >>= fun (main_chain_deposit, main_chain_deposit_confirmation) ->
  of_action issue_user_transaction_request
    (Deposit { deposit_amount; deposit_fee; main_chain_deposit; main_chain_deposit_confirmation })

(* TODO: find the actual gas limit *)
let withdrawal_gas_limit = TokenAmount.of_int 1000000

(* in Lwt monad, because we'll push the request to the main chain *)
let withdrawal (facilitator, withdrawal_amount) =
  let open UserAsyncAction in
  of_lwt_exn get_facilitator_fee_schedule facilitator
  >>= fun {withdrawal_fee} ->
  of_action issue_user_transaction_request
    (Withdrawal { withdrawal_amount ; withdrawal_fee })

let payment_fee_for FacilitatorFeeSchedule.{fee_per_billion} payment_amount =
  TokenAmount.(div (mul fee_per_billion payment_amount) one_billion_tokens)

let payment (facilitator, recipient_address, payment_amount, memo) =
  let open UserAsyncAction in
  of_lwt_exn get_facilitator_fee_schedule facilitator
  >>= fun fee_schedule ->
  let payment_invoice = Invoice.{recipient= recipient_address; amount= payment_amount; memo} in
  let payment_fee = payment_fee_for fee_schedule payment_amount in
  of_action issue_user_transaction_request
    (Payment {payment_invoice; payment_fee; payment_expedited= false})

(** We should be signing the RLP, not the marshaling! *)
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

module ErrorNotification = struct
  [@warning "-39"]
  type t =
    { status: TransactionStatus.t
    ; error: Exception.t }
  [@@deriving yojson]
end

(** Events that affect user state *)
(* TODO: Rewrite UserEvent + corresponding return type with GADT ? *)
module UserEvent = struct
  [@warning "-39"]
  type t =
    | GetState of {from: Revision.t option}
    | Payment of {facilitator: Address.t; recipient: Address.t; amount: TokenAmount.t; memo: string}
    | Deposit of {facilitator: Address.t; amount: TokenAmount.t}
    | Withdrawal of {facilitator: Address.t; amount: TokenAmount.t}
    | Fail of ErrorNotification.t
    | Update of TransactionStatus.t
  [@@deriving yojson]
end

let add_notification notification_type to_yojson x state =
  let counter = state.UserState.notification_counter in
  UserAction.return counter
    (state
     |> UserState.lens_notification_counter.set Revision.(add one counter)
     |> Lens.modify UserState.lens_notifications
          (List.cons (counter, `List [`String notification_type; to_yojson x])))

let add_error_notification = add_notification "error_notification" ErrorNotification.to_yojson

let _notify_error status error = add_error_notification {status;error}


(** The transaction_loop is the actor that handles the TransactionStatus.t,
    responds to queries about this state, spawns threads for each state transition,
    saves this state to DB when it changes and updates the user_loop in-memory.

    TODO: when restarting, somehow check if the next step wasn't made already,
    so we don't make it again? At least, for the crucial steps where we'd lose money if we fail?
    e.g. record the time, and on reload, scan for events in a window after that time.
*)
let _transaction_loop : 'a Lwt_mvar.t -> string -> TransactionStatus.t -> _ Lwt.t =
  let open Lwt in
  fun mailbox request_key status ->
    let update status =
      (* First, persist the new state! *)
      Db.put request_key (TransactionStatus.marshal_string status)
      >>= fun () -> (* Then, post the new status to the mailbox *)
      Lwt_mvar.put mailbox status in
    match status with
    | `Failed _ -> return_unit
    | `Requested request ->
      request
      |> Side_chain_client.post_user_transaction_request_to_side_chain
      >>= Lwter.arr
            (function
              | Ok tc -> `SignedByFacilitator tc
              | Error error -> `Failed (Exception.to_yojson error)) (* TODO: include request context *)
      >>= update
    | _ ->
      bottom ()


(** The user_loop is the server that handles the UserState.t, responds to queries about this state,
    spawns threads for new transactions, and gets in-memory state updates from them. On the other hand,
    said threads update the database directly with their own key. *)
(*
   let user_loop mailbox address =
   let open UserEvent in
   let open Lwt in
   let keypair = keypair_of_address address in
   let user_state = UserState.load address in
   let spawn_transaction_watcher (status : TransactionStatus.t) =
   match status with
   | Requested request ->
   | SignedByFacilitator tc ->
   bottom ()
   push_side_chain_withdrawal_to_main_chain
   xxx in
   let spawn_transaction_watchers pending_operations =
   xxx in
   let spawn_request_watcher request =
   spawn_transaction_watcher (transaction_status_of_request request)
   >>= const request in
   let user_step = function
   (* TODO: either move get_facilitator_fee_schedule out of the loop by adding an argument to Payment,
   or make it asynchronous by extending the UserState with the notion of requests that await
   knowledge of fee_schedule *)
   | Payment {facilitator: Address.t; recipient: Address.t; amount: TokenAmount.t; memo: string} ->
   payment (facilitator, recipient, amount, memo)
   >>= spawn_request_watcher
   >>= arr TransactionRequest.to_yojson
   |
   >>= fun transaction ->
   (* set timestamp, now that all processing on Trent is done *)
   payment_timestamp ();
   (* remaining code is preparing response *)
   let tx_revision = transaction.tx_header.tx_revision in
   let sender_name = get_user_name sender in
   let recipient_name = get_user_name recipient in
   let sender_account = get_user_account sender in
   let recipient_account = get_user_account recipient in
   let make_account_state address name (account : AccountState.t) =
   { address
   ; user_name = name
   ; balance = account.balance
   ; revision = account.account_revision
   }
   in
   let sender_account = make_account_state sender sender_name sender_account in
   let recipient_account = make_account_state recipient recipient_name recipient_account in
   let side_chain_tx_revision = tx_revision in
   let payment_result =
   { sender_account
   ; recipient_account
   ; amount_transferred = amount
   ; side_chain_tx_revision
   }
   in
   return (payment_result_to_yojson payment_result)

   | UserTransactionRequest req ->
   add_pending_request req
   >>= bork "Gotta return something to the user"
   in
   simple_server mailbox user_step user_state
*)

let user_table = Hashtbl.create 4

(*
   let start_user_loop address =
   let mailbox = Lwt_mvar.create_empty () in
   Lwt.async (fun () -> user_loop mailbox address);
   Hashtbl.replace user_table address mailbox
*)

let post_user_event address = simple_client (Hashtbl.find user_table address) identity

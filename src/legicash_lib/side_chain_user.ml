open Lens.Infix

open Legilogic_lib
open Lib
open Action
open Yojsoning
open Marshaling
open Crypto
open Persisting
open Db_types
open Merkle_trie

open Legilogic_ethereum

open Side_chain

(* TODO:
   Devise and implement a good persistent asynchronous programming model.
   i.e. each actor (in this case, a side_chain user) has persistent set of partial ongoing
   transactions, and it will keep making progress on by chatting with other actors,
   even if the computer crashes.

   The transactions may be themselves organized as a set of desiderata each associated
   with some "aspect" of the state. Each aspect can synchronize with the outside world;
   if there is a conflict, then log a notification and reset expectations.
   If there is no conflict, but incompleteness, then do the next thing to go forward,
   or set a timeout if waiting for a response.

   Thus, to ensure that a key is usable on ethereum, start with the assumption that geth
   doesn't know about keys, so the next step is to send it the key.
   Confirmation can later be achieved by trying a simple use of the key.
   If later geth complains that the key is not found, issue a notification,
   and probably persist in the desire for a while and retry a few times,
   before giving up on the desire and issuing a bigger notification.
 *)

module KnowledgeStage = struct
  type t = Unknown | Pending | Confirmed | Rejected
  let to_char = function Unknown -> 'U' | Pending -> 'P' | Confirmed -> 'C' | Rejected -> 'R'
  let of_char = function | 'U' -> Unknown | 'P' -> Pending | 'C' -> Confirmed | 'R' -> Rejected
                         | _ -> raise (Internal_error "Invalid KnowledgeStage character")
  module PrePersistable = struct
    type nonrec t = t
    let marshaling = marshaling_map to_char of_char char_marshaling
    let make_persistent = already_persistent
    let walk_dependencies = no_dependencies
    let yojsoning = yojsoning_map to_char of_char char_yojsoning
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
end

module Episteme = struct
  [@warning "-39"]
  type t =
    { request: Request.t signed
    ; confirmation_option: Confirmation.t option
    ; main_chain_confirmation_option: Main_chain.Confirmation.t option }
  [@@deriving lens { prefix=true }, yojson]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling3
        (fun { request; confirmation_option; main_chain_confirmation_option } ->
           request, confirmation_option, main_chain_confirmation_option)
        (fun request confirmation_option main_chain_confirmation_option ->
           { request; confirmation_option; main_chain_confirmation_option })
        (marshaling_signed Request.marshaling)
        (option_marshaling Confirmation.marshaling)
        (option_marshaling Main_chain.Confirmation.marshaling)
    let walk_dependencies = no_dependencies
    let make_persistent = normal_persistent
    let yojsoning = {to_yojson;of_yojson}
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
end

module UserAccountState = struct
  [@warning "-39"]
  type t =
    { facilitator_validity: KnowledgeStage.t
    ; confirmed_state: AccountState.t
    ; pending_operations: Episteme.t list }
  [@@deriving lens { prefix=true }, yojson ]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling3
        (fun { facilitator_validity
             ; confirmed_state
             ; pending_operations } ->
          facilitator_validity, confirmed_state, pending_operations)
        (fun facilitator_validity confirmed_state pending_operations ->
           { facilitator_validity
           ; confirmed_state
           ; pending_operations })
        KnowledgeStage.marshaling AccountState.marshaling
        (list_marshaling Episteme.marshaling)
    let walk_dependencies = no_dependencies
    let make_persistent = normal_persistent
    let yojsoning = {to_yojson;of_yojson}
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
  let empty =
    { facilitator_validity= Confirmed
    ; confirmed_state=AccountState.empty
    ; pending_operations= [] }
end

module UserAccountStateMap = MerkleTrie (Address) (UserAccountState)

module UserState = struct
  type t =
    { main_chain_user_state: Main_chain.UserState.t
    ; facilitators: UserAccountStateMap.t
    ; notifications: (Revision.t * yojson) list }
  [@@deriving lens { prefix=true }]
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
  UserAsyncAction.return Side_chain.Test.trent_fee_schedule

(** TODO: find and justify a good default validity window in number of blocks *)
let default_validity_window = Duration.of_int 256

let stub_confirmed_main_chain_state = ref Main_chain.genesis_state

let stub_confirmed_main_chain_state_digest = ref (Main_chain.State.digest Main_chain.genesis_state)

let stub_confirmed_side_chain_state = ref Side_chain.State.empty

let stub_confirmed_side_chain_state_digest = ref (State.digest Side_chain.State.empty)

let make_rx_header facilitator_address user_state =
  let open UserAction in
  match UserAccountStateMap.find_opt facilitator_address user_state.UserState.facilitators with
  | None -> fail Not_found user_state
  | Some facilitator ->
    return
      { RxHeader.facilitator= facilitator_address
      ; RxHeader.requester= user_state.main_chain_user_state.keypair.address
      ; RxHeader.requester_revision=
          Revision.add facilitator.confirmed_state.account_revision
            (Revision.of_int (1 + List.length facilitator.pending_operations))
      ; RxHeader.confirmed_main_chain_state_digest= !stub_confirmed_main_chain_state_digest
      ; RxHeader.confirmed_main_chain_state_revision= !stub_confirmed_main_chain_state.revision
      ; RxHeader.confirmed_side_chain_state_digest= !stub_confirmed_side_chain_state_digest
      ; RxHeader.confirmed_side_chain_state_revision=
          !stub_confirmed_side_chain_state.facilitator_revision
      ; RxHeader.validity_within= default_validity_window }
      user_state

let mk_rx_episteme rx =
  Episteme.{request= rx; confirmation_option= None; main_chain_confirmation_option= None}

let [@warning "-32"] mk_tx_episteme tx =
  Episteme.
    { request= tx.payload.Confirmation.signed_request
    ; confirmation_option= Some tx.payload
    ; main_chain_confirmation_option= None }

let facilitator_lens : Address.t -> (UserState.t, UserAccountState.t) Lens.t =
  fun facilitator_address ->
  UserState.lens_facilitators |--
    defaulting_lens (konstant UserAccountState.empty)
      (UserAccountStateMap.lens facilitator_address)

(** TODO: Handle cases of updates to previous epistemes, rather than just new ones *)
let add_user_episteme episteme user_state =
  let facilitator = episteme.Episteme.request.payload.rx_header.facilitator in
  Lens.modify
    (facilitator_lens facilitator |-- UserAccountState.lens_pending_operations)
    (fun ops -> episteme::ops) (* TODO: replays, retries...*)
    user_state

let remove_user_request request user_state =
  let facilitator = request.payload.Request.rx_header.facilitator in
  Lens.modify
    (facilitator_lens facilitator |-- UserAccountState.lens_pending_operations)
    (List.filter (fun x -> x.Episteme.request != request))
    user_state

let sign_request request user_state =
  UserAction.return
    (Request.signed user_state.UserState.main_chain_user_state.keypair request)
    user_state

let add_pending_request request state =
  UserAction.return request (add_user_episteme (mk_rx_episteme request) state)

let mark_request_rejected request state =
  UserAction.return () (remove_user_request request state)

let issue_user_request operation =
  let open UserAction in
  get_first_facilitator ()
  >>= make_rx_header
  >>= fun rx_header -> return Request.{rx_header; operation}
  >>= sign_request
  >>= add_pending_request

(* TODO: is this used? should balances and revisions be updated in effect_request?
   looks like balances already are
*)
let update_account_state_with_trusted_operation
      trusted_operation ({balance} as account_state : AccountState.t) =
  let f =
    {account_state with account_revision= Revision.add account_state.account_revision Revision.one} in
  match trusted_operation with
  | Operation.Deposit {deposit_amount; deposit_fee=_deposit_fee} ->
    if true (* check that everything is correct *) then
      {f with balance= TokenAmount.add balance deposit_amount}
    else raise (Internal_error "I mistrusted your deposit operation")
  | Operation.Payment {payment_invoice; payment_fee} ->
    let decrement = TokenAmount.add payment_invoice.amount payment_fee in
    if TokenAmount.compare balance decrement >= 0 then
      {f with balance= TokenAmount.sub balance decrement}
    else raise (Internal_error "I mistrusted your payment operation")
  | Operation.Withdrawal {withdrawal_amount; withdrawal_fee} ->
    if true (* check that everything is correct *) then
      {f with balance= TokenAmount.sub balance (TokenAmount.add withdrawal_amount withdrawal_fee)}
    else raise (Internal_error "I mistrusted your withdrawal operation")

(** We assume most recent operation is to the left of the changes list,
*)
let update_account_state_with_trusted_operations trusted_operations account_state =
  List.fold_right update_account_state_with_trusted_operation trusted_operations account_state

let [@warning "-32"] optimistic_facilitator_account_state facilitator_address user_state =
  match UserAccountStateMap.find_opt facilitator_address user_state.UserState.facilitators with
  | None -> AccountState.empty
  | Some {facilitator_validity; confirmed_state; pending_operations} ->
    match facilitator_validity with
    | Rejected -> confirmed_state
    | _ ->
      update_account_state_with_trusted_operations
        (List.map (fun x -> x.Episteme.request.payload.operation) pending_operations)
        confirmed_state

let lift_main_chain_user_async_action_to_side_chain main_chain_user_async_action input user_state =
  Lwt.bind
    (main_chain_user_async_action input user_state.UserState.main_chain_user_state)
    (fun (result, new_state) ->
       Lwt.return (result, {user_state with main_chain_user_state= new_state}))

let deposit (facilitator_address, deposit_amount) =
  let open UserAsyncAction in
  get_facilitator_fee_schedule facilitator_address
  >>= fun {deposit_fee} ->
  lift_main_chain_user_async_action_to_side_chain Main_chain_action.deposit
    (facilitator_address, (TokenAmount.add deposit_amount deposit_fee))
  >>= fun main_chain_deposit_signed ->
  lift_main_chain_user_async_action_to_side_chain Ethereum_action.wait_for_confirmation
    main_chain_deposit_signed
  >>= fun main_chain_deposit_confirmation ->
  of_action issue_user_request
    (Deposit
       { deposit_amount
       ; deposit_fee
       ; main_chain_deposit_signed
       ; main_chain_deposit_confirmation
       ; deposit_expedited= false })

(* in Lwt monad, because we'll push the request to the main chain *)
let withdrawal (facilitator_address, withdrawal_amount) =
  let open UserAsyncAction in
  get_facilitator_fee_schedule facilitator_address
  >>= fun {withdrawal_fee} ->
  of_action issue_user_request
    (Withdrawal { withdrawal_amount ; withdrawal_fee })

let payment_fee_for FacilitatorFeeSchedule.{fee_per_billion} payment_amount =
  TokenAmount.(div (mul fee_per_billion payment_amount) one_billion_tokens)

let payment (facilitator_address, recipient_address, payment_amount) =
  let open UserAsyncAction in
  get_facilitator_fee_schedule facilitator_address
  >>= fun fee_schedule ->
  let payment_invoice = Invoice.{recipient= recipient_address; amount= payment_amount; memo= ""} in
  let payment_fee = payment_fee_for fee_schedule payment_amount in
  of_action issue_user_request
    (Payment {payment_invoice; payment_fee; payment_expedited= false})

(** We should be signing the RLP, not the marshaling! *)
let make_main_chain_withdrawal_transaction { Operation.withdrawal_amount; Operation.withdrawal_fee} user_address facilitator_keys =
  (* TODO: should the withdrawal fee agree with the facilitator state fee schedule? where to enforce? *)
  let ticket = 0L in (* TODO: implement ticketing *)
  let confirmed_state = Digest.zero in (* TODO: is this just a digest of the facilitator state here? *)
  let bond = 0 in (* TODO: where does this come from? *)
  let operation = Facilitator_contract.make_withdraw_call facilitator_keys.address ticket bond confirmed_state in
  let tx_header =
    Main_chain.TxHeader.
      { sender= user_address
      ; nonce= Main_chain.Nonce.zero (* TODO: get_nonce facilitator_address *)
      ; gas_price= TokenAmount.of_int 2 (* TODO: what are the right gas policies? *)
      ; gas_limit= TokenAmount.of_int 1000000
      ; value= TokenAmount.sub withdrawal_amount withdrawal_fee
      }
  in
  let transaction = Main_chain.{ Transaction.tx_header; Transaction.operation } in
  Main_chain.Transaction.signed facilitator_keys transaction

(* an action made on the side chain may need a corresponding action on the main chain *)
let push_side_chain_action_to_main_chain
      (facilitator_state : FacilitatorState.t)
      (confirmation : Confirmation.t)
      (user_state : UserState.t) =
  (*
     let facilitator_address = facilitator_state.keypair.address in
     if not (is_signature_valid Confirmation.digest facilitator_address signed_confirmation.signature confirmation) then
     raise (Internal_error "Invalid facilitator signature on signed confirmation");*)
  let signed_request = confirmation.signed_request in
  let request = signed_request.payload in
  let user_keys = user_state.main_chain_user_state.keypair in
  let user_address = user_keys.address in
  if not (is_signature_valid Request.digest user_address signed_request.signature request) then
    raise (Internal_error "Invalid user signature on signed request");
  match request.operation with
  | Withdrawal details ->
    let open Lwt in
    let signed_transaction = make_main_chain_withdrawal_transaction details user_address facilitator_state.keypair in
    Ethereum_action.wait_for_confirmation signed_transaction user_state.main_chain_user_state
    >>= fun (main_chain_confirmation, main_chain_user_state) ->
    return (main_chain_confirmation, { user_state with main_chain_user_state })
  | Payment _
  | Deposit _ ->
    raise (Internal_error "Side chain confirmation does not need subsequent interaction with main chain")

let collect_account_liquidation_funds = bottom

let request_account_liquidation = bottom

let initiate_individual_exit = bottom

let request_deposit = bottom


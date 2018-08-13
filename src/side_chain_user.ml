open Lib
open Action
open Yojsoning
open Marshaling
open Crypto
open Db
open Db_types
open Merkle_trie
open Side_chain
open Lens.Infix

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
    ; confirmation_option: Confirmation.t signed option
    ; main_chain_confirmation_option: Main_chain.Confirmation.t option }
  [@@deriving lens, yojson]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling3
        (fun { request; confirmation_option; main_chain_confirmation_option } ->
           request, confirmation_option, main_chain_confirmation_option)
        (fun request confirmation_option main_chain_confirmation_option ->
           { request; confirmation_option; main_chain_confirmation_option })
        (marshaling_signed Request.marshaling)
        (option_marshaling (marshaling_signed Confirmation.marshaling))
        (option_marshaling Main_chain.Confirmation.marshaling)
    let walk_dependencies = no_dependencies
    let make_persistent = normal_persistent
    let yojsoning = {to_yojson;of_yojson}
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
end

module UserAccountStatePerFacilitator = struct
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

module UserAccountStateMap = MerkleTrie (Address) (UserAccountStatePerFacilitator)

type user_state =
  { main_chain_user_state: Main_chain.user_state
  ; facilitators: UserAccountStateMap.t }
[@@deriving lens]

type ('input, 'action) user_action = ('input, 'action, user_state) action

type ('input, 'action) user_async_action = ('input, 'action, user_state) async_action

let get_first_facilitator_state_option (user_state, _) :
  (Address.t * UserAccountStatePerFacilitator.t) option =
  UserAccountStateMap.find_first_opt (konstant true) user_state.facilitators

let get_first_facilitator =
  action_of_pure_action get_first_facilitator_state_option
  ^>> function
    | state, None -> (state, Error No_facilitator_yet)
    | state, Some (address, _) -> (state, Ok address)

(** TODO: find and justify a good default validity window in number of blocks *)
let default_validity_window = Duration.of_int 256

let stub_confirmed_main_chain_state = ref Main_chain.genesis_state

let stub_confirmed_main_chain_state_digest = ref (Main_chain.State.digest Main_chain.genesis_state)

let stub_confirmed_side_chain_state = ref Side_chain.State.empty

let stub_confirmed_side_chain_state_digest = ref (State.digest Side_chain.State.empty)

let make_rx_header (user_state, facilitator_address) =
  match UserAccountStateMap.find_opt facilitator_address user_state.facilitators with
  | None -> (user_state, Error Not_found)
  | Some facilitator ->
    ( user_state
    , Ok
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
        ; RxHeader.validity_within= default_validity_window } )

let mk_rx_episteme rx =
  Episteme.{request= rx; confirmation_option= None; main_chain_confirmation_option= None}

let [@warning "-32"] mk_tx_episteme tx =
  Episteme.
    { request= tx.payload.Confirmation.signed_request
    ; confirmation_option= Some tx
    ; main_chain_confirmation_option= None }

(** TODO: Handle cases of updates to previous epistemes, rather than just new ones *)
let add_user_episteme user_state episteme =
  let facilitator = episteme.Episteme.request.payload.rx_header.facilitator in
  let account_state =
    UserAccountStateMap.find_defaulting
      (konstant UserAccountStatePerFacilitator.empty)
      facilitator user_state.facilitators
  in
  ( user_state_facilitators |-- UserAccountStateMap.lens facilitator
    |-- UserAccountStatePerFacilitator.lens_pending_operations )
  .set (episteme :: account_state.pending_operations) user_state

let issue_user_request =
  (fun (user_state, operation) ->
     (user_state, ()) ^|> get_first_facilitator ^>> make_rx_header
     ^>> action_of_pure_action (fun (user_state, rx_header) ->
       sign Request.digest user_state.main_chain_user_state.keypair.private_key
         {Request.rx_header; Request.operation} ) )
  ^>> fun (user_state, request) ->
    (add_user_episteme user_state (mk_rx_episteme request), Ok request)

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

let [@warning "-32"] optimistic_facilitator_account_state (user_state, facilitator_address) =
  match UserAccountStateMap.find_opt facilitator_address user_state.facilitators with
  | None -> AccountState.empty
  | Some {facilitator_validity; confirmed_state; pending_operations} ->
    match facilitator_validity with
    | Rejected -> confirmed_state
    | _ ->
      update_account_state_with_trusted_operations
        (List.map (fun x -> x.Episteme.request.payload.operation) pending_operations)
        confirmed_state

let lift_main_chain_user_async_action_to_side_chain async_action (user_state, input) =
  let open Lwt in
  async_action (user_state.main_chain_user_state, input)
  >>= fun (new_state, result) ->
  return ({user_state with main_chain_user_state= new_state}, result)

(* TODO: use facilitator fee schedule *)
let deposit_fee = TokenAmount.of_int 5

let deposit ((_user_state, (_facilitator_address, deposit_amount)) as input) =
  let open Lwt in
  input
  |> lift_main_chain_user_async_action_to_side_chain Main_chain_action.deposit
  ^>>+ fun ((_user_state1, main_chain_deposit_signed) as transaction) ->
  transaction
  |> lift_main_chain_user_async_action_to_side_chain Main_chain_action.wait_for_confirmation
  ^>>+ fun (user_state2, main_chain_deposit_confirmation) ->
  return (issue_user_request
            ( user_state2
            , Deposit
                { deposit_amount= TokenAmount.sub deposit_amount deposit_fee
                ; deposit_fee
                ; main_chain_deposit_signed
                ; main_chain_deposit_confirmation
                ; deposit_expedited= false } ))

(* TODO: take into account not just the facilitator name, but the fee schedule, too. *)
let withdrawal_fee = TokenAmount.of_int 5

(* in Lwt monad, because we'll push the request to the main chain *)
let withdrawal (user_state, (_facilitator_address, withdrawal_amount)) =
  Lwt.return (issue_user_request
                ( user_state
                , Withdrawal
                    { withdrawal_amount= TokenAmount.sub withdrawal_amount withdrawal_fee
                    ; withdrawal_fee
                    } ))

let payment (user_state, (_facilitator_address, recipient_address, payment_amount)) =
  let invoice = Invoice.{recipient= recipient_address; amount= payment_amount; memo= ""} in
  issue_user_request
    ( user_state
    , Payment
        { payment_invoice= invoice
        ; payment_fee= TokenAmount.of_int 0 (* TODO: we should get this from facilitator fee schedule *)
        ; payment_expedited= false } )

let make_main_chain_withdrawal_transaction { Operation.withdrawal_amount; Operation.withdrawal_fee} user_address facilitator_keys =
  (* TODO: should the withdrawal fee agree with the facilitator state fee schedule? where to enforce? *)
  let ticket = 0L in (* TODO: implement ticketing *)
  let confirmed_state = Digest.zero in (* TODO: is this just a digest of the facilitator state here? *)
  let bond = 0 in (* TODO: where does this come from? *)
  let operation = Facilitator_contract.make_withdraw_call facilitator_keys.Keypair.address ticket bond confirmed_state in
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
  Ethereum_transaction.sign_transaction facilitator_keys transaction

(* an action made on the side chain may need a corresponding action on the main chain *)
let push_side_chain_action_to_main_chain (facilitator_state : FacilitatorState.t) ((user_state : user_state), (signed_confirmation : Confirmation.t signed)) =
  let confirmation = signed_confirmation.payload in
  let facilitator_address = facilitator_state.keypair.address in
  if not (is_signature_valid Confirmation.digest facilitator_address signed_confirmation.signature confirmation) then
    raise (Internal_error "Invalid facilitator signature on signed confirmation");
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
    Main_chain_action.wait_for_confirmation (user_state.main_chain_user_state,signed_transaction)
    >>= fun (main_chain_user_state,main_chain_confirmation) ->
    return ({ user_state with main_chain_user_state },main_chain_confirmation)
  | Payment _
  | Deposit _ ->
    raise (Internal_error "Side chain confirmation does not need subsequent interaction with main chain")

let collect_account_liquidation_funds = bottom

let request_account_liquidation = bottom

let initiate_individual_exit = bottom

let request_deposit = bottom


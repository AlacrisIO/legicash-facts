open Legicash_base
open Legibase
open Keypairs
open Lib
open Legicash_chains
open Main_chain
open Side_chain
open Main_chain_action

let is_signature_matching address public_key signature payload =
  address_matches_public_key address public_key
  && is_signature_valid public_key signature payload


(** Is the request well-formed?
    TODO: check that the account is active (unless the request is to open the account!),
    that the request revision number matches the user revision number,
    that the request references a recent confirmed main chain state indeed,
    that the request is not expired,
 *)
let is_side_chain_request_well_formed (state, request) =
  match (state, request) with {account_states}, {payload; signature} ->
    match payload
    with
    | { rx_header=
          { facilitator
          ; requester
          ; requester_revision
          ; confirmed_main_chain_state_digest
          ; confirmed_main_chain_state_revision
          ; confirmed_side_chain_state_digest
          ; confirmed_side_chain_state_revision
          ; validity_within }
      ; operation }
    ->
      (* TODO: check confirmed main & side chain state + validity window *)
      let account_option = AddressMap.find_opt requester account_states in
      match operation with
      | Open_account user_key ->
          account_option == None && requester_revision == Revision.one
          && is_signature_matching requester user_key signature payload
      | _ ->
        match account_option with
        | None -> false
        | Some {active; user_revision; user_key} ->
            active
            && requester_revision == Revision.add user_revision Revision.one
            && is_signature_matching requester user_key signature payload


(** Check that the request is basically well-formed, or else fail *)
let check_side_chain_request_well_formed =
  action_assert is_side_chain_request_well_formed


let confirm_request =
  action_seq check_side_chain_request_well_formed
    (action_seq
       (function
           | (state, {payload= {rx_header; operation}}) as rx ->
             match operation with
             | Open_account user_key ->
                 (state, (* { state with active = true; user_key }*) Ok rx)
             | Close_account -> (state, (* with active = false } *) Ok rx)
             | Payment {payment_invoice; payment_fee; payment_expedited} ->
                 bottom ()
             | Deposit
                 { deposit_amount
                 ; deposit_fee
                 ; main_chain_transaction_signed
                 ; main_chain_confirmation
                 ; deposit_expedited } ->
                 bottom ()
             | Withdrawal {withdrawal_invoice; withdrawal_fee} -> bottom ())
       bottom)


let stub_confirmed_main_chain_state = ref Main_chain.genesis_state

let stub_confirmed_main_chain_state_digest =
  ref (get_digest Main_chain.genesis_state)


let genesis_side_chain_state =
  { previous_main_chain_state= get_digest Main_chain.genesis_state
  ; previous_side_chain_state= null_digest
  ; side_chain_revision= Revision.zero
  ; user_accounts= AddressMap.empty
  ; operations= AddressMap.empty }


let stub_confirmed_side_chain_state = ref genesis_side_chain_state

let stub_confirmed_side_chain_state_digest =
  ref (get_digest genesis_side_chain_state)


let get_first_facilitator_state_option (side_chain_user_state, _)
    : (Address.t * user_account_state_per_facilitator) option =
  AddressMap.find_first_opt (constantly true)
    side_chain_user_state.facilitators


let get_first_facilitator =
  action_seq (action_of_pure_action get_first_facilitator_state_option)
    (function
    | state, None -> (state, Error No_facilitator_yet)
    | state, Some (address, _) -> (state, Ok address) )


(** TODO: find and justify a good default validity window in number of blocks *)
let default_validity_window = Int64.of_int 256

let make_rx_header (side_chain_user_state, facilitator_address) =
  match
    AddressMap.find_opt facilitator_address side_chain_user_state.facilitators
  with
  | None -> (side_chain_user_state, Error Not_found)
  | Some facilitator ->
      ( side_chain_user_state
      , Ok
          { facilitator= facilitator_address
          ; requester= side_chain_user_state.main_chain_user_state.address
          ; requester_revision=
              Revision.add facilitator.confirmed_state.user_revision
                Revision.one
              (* TODO: apply the pending transactions!!! *)
          ; confirmed_main_chain_state_digest=
              !stub_confirmed_main_chain_state_digest
          ; confirmed_main_chain_state_revision=
              !stub_confirmed_main_chain_state.revision
          ; confirmed_side_chain_state_digest=
              !stub_confirmed_side_chain_state_digest
          ; confirmed_side_chain_state_revision=
              !stub_confirmed_side_chain_state.side_chain_revision
          ; validity_within= default_validity_window } )


let mk_rx_episteme rx =
  {request= rx; confirmation_option= None; main_chain_confirmation_option= None}


let mk_tx_episteme tx =
  { request= tx.payload.signed_request
  ; confirmation_option= Some tx
  ; main_chain_confirmation_option= None }


let add_user_episteme user_state episteme =
  (* TODO: use lenses?
     update_pending state (fun pending -> episteme :: pending)
     check that active status is correct, that revision match, that facilitator isn't known failed *)
  {user_state with facilitators= user_state.facilitators}


let issue_user_request =
  action_seq
    (fun (user_state, operation) ->
      do_action (user_state, ())
        (action_seq get_first_facilitator
           (action_seq make_rx_header
              (action_of_pure_action (fun (user_state, rx_header) ->
                   sign user_state.main_chain_user_state.private_key
                     {rx_header; operation} )))) )
    (fun (user_state, request) ->
      (add_user_episteme user_state (mk_rx_episteme request), Ok request) )


(** Default (empty) state for a new facilitator *)
let new_facilitator_account_state_per_user user_key =
  {active= false; balance= Int64.zero; user_revision= Revision.zero; user_key}


(** User's view of the default (empty) state for a new facilitator *)
let new_user_account_state_per_facilitator user_key =
  { facilitator_validity= Confirmed
  ; confirmed_state= new_facilitator_account_state_per_user user_key
  ; pending_operations= [] }


(** We assume that the operation will correctly apply:
    balances are sufficient for spending,
    deposits confirmation will check out,
    active revision will only increase, etc.
 *)
let update_facilitator_account_state_per_user_with_trusted_operation
    trusted_operation ({active; balance} as facilitator_account_state_per_user) =
  let f =
    { facilitator_account_state_per_user with
      user_revision=
        Revision.add facilitator_account_state_per_user.user_revision
          Revision.one }
  in
  match trusted_operation with
  | Open_account _ -> {f with active= true}
  | Close_account -> {f with active= false}
  | Payment {payment_invoice; payment_fee} ->
      let decrement = Int64.add payment_invoice.amount payment_fee in
      if Int64.compare balance decrement >= 0 then
        {f with balance= Int64.sub balance decrement}
      else raise (Internal_error "I mistrusted your payment operation")
  | Deposit {deposit_amount; deposit_fee} ->
      if true (* check that everything is correct *) then
        {f with balance= Int64.add balance deposit_amount}
      else raise (Internal_error "I mistrusted your deposit operation")
  | Withdrawal {withdrawal_invoice; withdrawal_fee} ->
      if true (* check that everything is correct *) then
        { f with
          balance=
            Int64.sub balance
              (Int64.add withdrawal_invoice.amount withdrawal_fee) }
      else raise (Internal_error "I mistrusted your withdrawal operation")


(** We assume most recent operation is to the left of the changes list,
 *)
let update_facilitator_account_state_per_user_with_trusted_operation
    trusted_operations facilitator_account_state_per_user =
  List.fold_right
    update_facilitator_account_state_per_user_with_trusted_operation
    trusted_operations facilitator_account_state_per_user


let optimistic_facilitator_account_state (user_state, facilitator_address) =
  match AddressMap.find_opt facilitator_address user_state.facilitators with
  | None ->
      new_facilitator_account_state_per_user
        user_state.main_chain_user_state.public_key
  | Some {facilitator_validity; confirmed_state; pending_operations} ->
    match facilitator_validity with
    | Rejected -> confirmed_state
    | _ ->
        update_facilitator_account_state_per_user_with_trusted_operation
          (List.map (fun x -> x.request.payload.operation) pending_operations)
          confirmed_state


let user_activity_status_for_facilitator (user_state, facilitator_address) =
  match AddressMap.find_opt facilitator_address user_state.facilitators with
  | Some {confirmed_state= {active}} -> active
  | None -> false


let is_account_open (user_state, facilitator_address) =
  user_activity_status_for_facilitator (user_state, facilitator_address)


(**
  TODO: take into account not just the facilitator name, but the fee schedule, too.
  TODO: exception if facilitator dishonest.
 *)
let open_account (user_state, facilitator_address) =
  let activity_status =
    user_activity_status_for_facilitator (user_state, facilitator_address)
  in
  if activity_status then (user_state, Error Already_open)
  else
    issue_user_request
      (user_state, Open_account user_state.main_chain_user_state.public_key)


let close_account (user_state, facilitator_address) =
  let activity_status =
    user_activity_status_for_facilitator (user_state, facilitator_address)
  in
  if activity_status then issue_user_request (user_state, Close_account)
  else (user_state, Error Already_closed)


let lift_main_chain_user_action_to_side_chain action (user_state, input) =
  let main_chain_user_state = user_state.main_chain_user_state in
  let new_main_chain_user_state, result =
    action (main_chain_user_state, input)
  in
  ({user_state with main_chain_user_state= new_main_chain_user_state}, result)


let deposit (user_state, input) =
  lift_main_chain_user_action_to_side_chain transfer_tokens (user_state, input)


let detect_main_chain_facilitator_issues = bottom

let collect_account_liquidation_funds = bottom

let send_user_request = bottom

let send_facilitator_confirmation = bottom

(** missing types to be implemented *)

type facilitator_to_facilitator_message

type user_to_user_message

let send_certified_check check conv = bottom

let commit_facilitator_state = bottom

let send_message = bottom

let request_account_liquidation = bottom

let check_main_chain_for_exits = bottom

let initiate_individual_exit = bottom

let request_deposit = bottom

open Legibase
open Keypair
open Lib
open Main_chain
open Side_chain
open Main_chain_action
open Lens.Infix

let is_signature_matching address public_key signature payload =
  address_matches_public_key address public_key
  && is_signature_valid public_key signature payload


(** Default (empty) state for a new facilitator *)
let new_account_state =
  { active= false
  ; balance= TokenAmount.zero
  ; account_revision= Revision.zero }


(** User's view of the default (empty) state for a new facilitator *)
let new_user_account_state_per_facilitator =
  { facilitator_validity= Confirmed
  ; confirmed_state= new_account_state
  ; pending_operations= [] }

type account_lens = (facilitator_state, account_state) Lens.t

(** Given a signed request, handle the special case of opening an account, and return
    the request, the account state (new or old), the lens to set the account back at the end, and
    the user's public key *)
let ensure_user_account
    : (request signed, request signed*account_state*account_lens*public_key) facilitator_action =
  function
  | {current={accounts; user_keys}} as state,
    ({payload= {rx_header={requester}; operation}} as rx) ->
     let account_lens =
       facilitator_state_current |-- state_accounts |-- AddressMap.lens requester
     in
     let account_option = AddressMap.find_opt requester accounts in
     match operation with
     | Open_account user_key ->
        (match account_option with
         | Some _ -> (state, Error Already_open)
         | None -> (facilitator_state_current |-- state_user_keys |-- AddressMap.lens requester).set
                     user_key state,
                   Ok (rx, new_account_state, account_lens, user_key))
     | _ ->
        (match account_option with
         | None -> (state, Error Assertion_failed)
         | Some account_state ->
            (state, Ok (rx, account_state, account_lens,
                        AddressMap.find requester state.current.user_keys)))

(** Is the request well-formed?
    This function should include all checks that can be made without any non-local side-effect
    beside reading pure or monotonic data, which is allowed.
    Thus, we can later parallelize this check.
 *)
let is_side_chain_request_well_formed
    : facilitator_state * (request signed*account_state*account_lens*public_key) -> bool = function
  | {current={accounts}} as state,
    ({payload={ rx_header=
                  { facilitator
                  ; requester
                  ; requester_revision
                  ; confirmed_main_chain_state_digest
                  ; confirmed_main_chain_state_revision
                  ; confirmed_side_chain_state_digest
                  ; confirmed_side_chain_state_revision
                  ; validity_within }
              ; operation } as payload;
      signature},
     {active; balance; account_revision},
     _,
     user_key) ->
     active = (match operation with Open_account _ -> false | _ -> true)
     && requester_revision = Revision.add account_revision Revision.one
     (* TODO: check confirmed main & side chain state + validity window *)
     && is_signature_matching requester user_key signature payload
     (* Check that the numbers add up: *)
     && match operation with
        | Open_account _ -> true
        | Deposit { deposit_amount
                  ; deposit_fee
                  ; main_chain_transaction_signed=
                      {signature;
                       payload={tx_header={sender;value};operation=main_chain_operation} as payload}
                      as main_chain_transaction_signed
                  ; main_chain_confirmation
                  ; deposit_expedited } ->
           (TokenAmount.compare value (TokenAmount.add deposit_amount deposit_fee)) >= 0
           && (match main_chain_operation with
               | Main_chain.TransferTokens recipient ->
                  recipient = state.keypair.address
               | _ -> false)
           (* TODO: delegate the same signature checking protocol to the main chain *)
           && is_signature_valid user_key signature payload
           && is_confirmation_valid main_chain_confirmation main_chain_transaction_signed
           && (TokenAmount.compare deposit_fee state.fee_schedule.deposit_fee) >= 0
        | Payment {payment_invoice; payment_fee; payment_expedited} ->
           (TokenAmount.compare balance (TokenAmount.add payment_invoice.amount payment_fee)) >= 0
           (* TODO: make per_account_limit work on the entire floating thing *)
           && (TokenAmount.compare state.fee_schedule.per_account_limit payment_invoice.amount) >= 0
           (* TODO: make sure the fee multiplication cannot overflow! *)
           && (TokenAmount.compare
                 (TokenAmount.mul state.fee_schedule.fee_per_billion
                    (TokenAmount.div payment_invoice.amount (TokenAmount.of_int 1000000000)))
                 payment_fee) >= 0
           && (facilitator_state_current |-- state_accounts
               |-- AddressMap.lens payment_invoice.recipient |-- account_state_active).get state
        | Close_account -> true
        | Withdrawal {withdrawal_invoice; withdrawal_fee} -> bottom ()


(** Check that the request is basically well-formed, or else fail *)
let check_side_chain_request_well_formed =
  action_assert is_side_chain_request_well_formed


let make_request_confirmation
    : ( request signed * account_state * account_lens
      , confirmation signed )
      facilitator_action =
 fun (facilitator_state, (signed_request, account_state, account_lens)) ->
  let revision =
    Revision.add facilitator_state.current.facilitator_revision Revision.one
  in
  account_lens.set
    { account_state with
      account_revision=
        signed_request.payload.rx_header.requester_revision }
    ((facilitator_state_current |-- state_facilitator_revision).set
       revision
       facilitator_state)
  , Ok
      (sign facilitator_state.keypair.private_key
         {tx_header= {tx_revision= revision; updated_limit=facilitator_state.current.spending_limit};
          signed_request})

exception Spending_limit_exceeded

(** Facilitator actions to use up some of the limit *)
let spend_spending_limit amount (state, x) =
  if (TokenAmount.compare amount state.current.spending_limit) <= 0 then
    (facilitator_state_current |-- state_spending_limit).set
      (TokenAmount.sub state.current.spending_limit amount)
      state,
    Ok x
  else
    state, Error Spending_limit_exceeded

let maybe_spend_spending_limit is_expedited amount (state, x) =
  if is_expedited then
    spend_spending_limit amount (state, x)
  else
    state, Ok x

exception Already_deposited

(* To prevent double-deposit of a same main_chain_transaction_signed,
   we put those transactions in a set (encoded as AddressMap of unit) of already accounted deposits!
   (Future: prune that set by expiring deposit requests?
   Have more expensive process to account for old deposits?)
 *)
let check_against_double_deposit main_chain_transaction_signed (state, x) =
  let witness = Digest.make main_chain_transaction_signed in
  let lens = (facilitator_state_current |-- state_deposited |-- DigestSet.lens witness) in
  if lens.get state then
    (state, Error Already_deposited)
  else
    lens.set true state, Ok x

(** compute the effects of a request on the account state *)
let effect_request
    : ( request signed * account_state * account_lens * public_key
      , request signed * account_state * account_lens )
      facilitator_action = function
  | state, ({payload= {rx_header; operation}} as rx, account_state, account_lens, user_key) ->
     match operation with
     | Open_account user_key ->
        (state, Ok (rx, {account_state with active= true}, account_lens))
     | Deposit { deposit_amount
               ; deposit_fee
               ; main_chain_transaction_signed
               ; deposit_expedited } ->
        (state, (rx,
                 { account_state with
                   balance = TokenAmount.add account_state.balance deposit_amount},
                 account_lens)) ^|>
          (check_against_double_deposit main_chain_transaction_signed)
          ^>> (maybe_spend_spending_limit deposit_expedited deposit_amount)
     | Payment {payment_invoice; payment_fee; payment_expedited} ->
        (Lens.modify
           (facilitator_state_current |-- state_accounts
            |-- AddressMap.lens payment_invoice.recipient
            |-- account_state_balance)
           (TokenAmount.add payment_invoice.amount)
           state,
         (rx,
          { account_state with
            balance = TokenAmount.sub account_state.balance
                        (TokenAmount.add payment_invoice.amount payment_fee)},
          account_lens)) ^|>
          (maybe_spend_spending_limit payment_expedited payment_invoice.amount)
     | Close_account ->
        ( Lens.modify account_lens
            (fun s -> {s with active= false})
            state
        , Ok (rx, account_state, account_lens) )
     | Withdrawal {withdrawal_invoice; withdrawal_fee} -> bottom ()


(** TODO:
 * save this initial state, and only use the new state if the confirmation was committed to disk,
 i.e. implement a try-catch in our monad
 * commit the confirmation to disk and remote replicas before to return it
 * parallelize, batch, etc., to have decent performance
 *)
let confirm_request : (request signed, confirmation signed) facilitator_action =
  ensure_user_account
  ^>> check_side_chain_request_well_formed
  ^>> effect_request
  ^>> make_request_confirmation


let stub_confirmed_main_chain_state = ref Main_chain.genesis_state

let stub_confirmed_main_chain_state_digest =
  ref (Digest.make Main_chain.genesis_state)


let genesis_side_chain_state =
  { previous_main_chain_state= null_digest
  ; previous_side_chain_state= null_digest
  ; facilitator_revision= Revision.zero
  ; spending_limit= TokenAmount.zero
  ; bond_posted= TokenAmount.zero
  ; accounts= AddressMap.empty
  ; user_keys= AddressMap.empty
  ; operations= AddressMap.empty
  ; deposited= DigestSet.empty
  }


let stub_confirmed_side_chain_state = ref genesis_side_chain_state

let stub_confirmed_side_chain_state_digest =
  ref (Digest.make genesis_side_chain_state)


let get_first_facilitator_state_option (user_state, _)
    : (Address.t * user_account_state_per_facilitator) option =
  AddressMap.find_first_opt (konstant true) user_state.facilitators


let get_first_facilitator =
  (action_of_pure_action get_first_facilitator_state_option)
  ^>> (function
      | state, None -> (state, Error No_facilitator_yet)
      | state, Some (address, _) -> (state, Ok address) )


(** TODO: find and justify a good default validity window in number of blocks *)
let default_validity_window = Duration.of_int 256

let make_rx_header (user_state, facilitator_address) =
  match AddressMap.find_opt facilitator_address user_state.facilitators with
  | None -> (user_state, Error Not_found)
  | Some facilitator ->
      ( user_state
      , Ok
          { facilitator= facilitator_address
          ; requester= user_state.main_chain_user_state.keypair.address
          ; requester_revision=
              Revision.add facilitator.confirmed_state.account_revision
                (Revision.of_int
                   (1 + List.length facilitator.pending_operations))
          ; confirmed_main_chain_state_digest=
              !stub_confirmed_main_chain_state_digest
          ; confirmed_main_chain_state_revision=
              !stub_confirmed_main_chain_state.revision
          ; confirmed_side_chain_state_digest=
              !stub_confirmed_side_chain_state_digest
          ; confirmed_side_chain_state_revision=
              !stub_confirmed_side_chain_state.facilitator_revision
          ; validity_within= default_validity_window } )


let mk_rx_episteme rx =
  {request= rx; confirmation_option= None; main_chain_confirmation_option= None}


let mk_tx_episteme tx =
  { request= tx.payload.signed_request
  ; confirmation_option= Some tx
  ; main_chain_confirmation_option= None }


(** TODO: Handle cases of updates to previous epistemes, rather than just new ones *)
let add_user_episteme user_state episteme =
  let facilitator = episteme.request.payload.rx_header.facilitator in
  let account_state =
    AddressMap.find_defaulting
      (konstant new_user_account_state_per_facilitator)
      facilitator user_state.facilitators
  in
  ( user_state_facilitators |-- AddressMap.lens facilitator
  |-- user_account_state_per_facilitator_pending_operations )
    .set (episteme :: account_state.pending_operations) user_state


let issue_user_request =
  (fun (user_state, operation) ->
    (user_state, ()) ^|>
      get_first_facilitator
      ^>> make_rx_header
      ^>> (action_of_pure_action (fun (user_state, rx_header) ->
               sign user_state.main_chain_user_state.keypair.private_key
                 {rx_header; operation})))
  ^>> (fun (user_state, request) ->
    (add_user_episteme user_state (mk_rx_episteme request), Ok request) )


(** We assume that the operation will correctly apply:
    balances are sufficient for spending,
    deposits confirmation will check out,
    active revision will only increase, etc.
 *)
let update_account_state_with_trusted_operation trusted_operation
    ({active; balance} as account_state) =
  let f =
    { account_state with
      account_revision=
        Revision.add account_state.account_revision Revision.one }
  in
  match trusted_operation with
  | Open_account _ -> {f with active= true}
  | Deposit {deposit_amount; deposit_fee} ->
      if true (* check that everything is correct *) then
        {f with balance= TokenAmount.add balance deposit_amount}
      else raise (Internal_error "I mistrusted your deposit operation")
  | Payment {payment_invoice; payment_fee} ->
      let decrement = TokenAmount.add payment_invoice.amount payment_fee in
      if TokenAmount.compare balance decrement >= 0 then
        {f with balance= TokenAmount.sub balance decrement}
      else raise (Internal_error "I mistrusted your payment operation")
  | Close_account -> {f with active= false}
  | Withdrawal {withdrawal_invoice; withdrawal_fee} ->
      if true (* check that everything is correct *) then
        { f with
          balance=
            TokenAmount.sub balance
              (TokenAmount.add withdrawal_invoice.amount withdrawal_fee) }
      else raise (Internal_error "I mistrusted your withdrawal operation")


(** We assume most recent operation is to the left of the changes list,
 *)
let update_account_state_with_trusted_operations trusted_operations
    account_state =
  List.fold_right update_account_state_with_trusted_operation
    trusted_operations account_state


let optimistic_facilitator_account_state (user_state, facilitator_address) =
  match AddressMap.find_opt facilitator_address user_state.facilitators with
  | None -> new_account_state
  | Some {facilitator_validity; confirmed_state; pending_operations} ->
    match facilitator_validity with
    | Rejected -> confirmed_state
    | _ ->
        update_account_state_with_trusted_operations
          (List.map (fun x -> x.request.payload.operation) pending_operations)
          confirmed_state


let user_activity_status_for_facilitator (user_state, facilitator_address) =
  (optimistic_facilitator_account_state (user_state, facilitator_address))
    .active


let is_account_open (user_state, facilitator_address) =
  user_activity_status_for_facilitator (user_state, facilitator_address)


(**
  TODO: exception if facilitator dishonest.
 *)
let open_account (user_state, facilitator_address) =
  let activity_status =
    user_activity_status_for_facilitator (user_state, facilitator_address)
  in
  if activity_status then (user_state, Error Already_open)
  else
    issue_user_request
      ( user_state
      , Open_account user_state.main_chain_user_state.keypair.public_key )


let close_account (user_state, facilitator_address) =
  let activity_status =
    user_activity_status_for_facilitator (user_state, facilitator_address)
  in
  if activity_status then issue_user_request (user_state, Close_account)
  else (user_state, Error Already_closed)


let lift_main_chain_user_action_to_side_chain action (user_state, input) =
  let new_state, result = action (user_state.main_chain_user_state, input) in
  ({user_state with main_chain_user_state= new_state}, result)


let deposit (user_state, input) =
  lift_main_chain_user_action_to_side_chain transfer_tokens (user_state, input)


(* TODO: take into account not just the facilitator name, but the fee schedule, too. *)

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

(* open account tests *)

let trent_keys =
  Keypair.make_keys_from_hex
    "b6:fb:0b:7e:61:36:3e:e2:f7:48:16:13:38:f5:69:53:e8:aa:42:64:2e:99:90:ef:f1:7e:7d:e9:aa:89:57:86"
    "04:26:bd:98:85:f2:c9:e2:3d:18:c3:02:5d:a7:0e:71:a4:f7:ce:23:71:24:35:28:82:ea:fb:d1:cb:b1:e9:74:2c:4f:e3:84:7c:e1:a5:6a:0d:19:df:7a:7d:38:5a:21:34:be:05:20:8b:5d:1c:cc:5d:01:5f:5e:9a:3b:a0:d7:df"


let alice_keys =
  Keypair.make_keys_from_hex
    "d5:69:84:dc:08:3d:76:97:01:71:4e:eb:1d:4c:47:a4:54:25:5a:3b:bc:3e:9f:44:84:20:8c:52:bd:a3:b6:4e"
    "04:23:a7:cd:9a:03:fa:9c:58:57:e5:14:ae:5a:cb:18:ca:91:e0:7d:69:45:3e:d8:51:36:ea:6a:00:36:10:67:b8:60:a5:b2:0f:11:53:33:3a:ef:2d:1b:a1:3b:1d:7a:52:de:28:69:d1:f6:23:71:bf:81:bf:80:3c:21:c6:7a:ca"


let bob_keys =
  Keypair.make_keys_from_hex
    "f1:d3:cd:20:22:e1:d6:64:98:32:76:04:83:4d:f0:73:06:64:f7:1a:8d:d1:1e:46:a3:3b:4a:0e:bb:40:ca:8e"
    "04:7d:52:54:04:9f:02:3e:e7:aa:ea:1e:fa:4f:17:ae:70:0f:af:67:23:24:02:5a:a9:b5:32:5a:92:1f:d0:f1:51:0e:68:31:f1:bf:90:b4:a1:df:e1:cd:49:e5:03:ec:7d:b5:9f:6e:78:73:d0:3a:3a:09:6c:46:5c:87:22:22:69"


let create_side_chain_user_state_for_testing user_keys main_chain_balance =
  let main_chain_user_state =
    {keypair= user_keys;
     confirmed_state= Digest.zero;
     confirmed_balance= TokenAmount.zero;
     pending_transactions= [];
     nonce= Nonce.zero}
  in
  let user_account_state = new_user_account_state_per_facilitator in
  let facilitators =
    AddressMap.singleton trent_keys.address user_account_state
  in
  { main_chain_user_state
  ; facilitators }


let alice_state = create_side_chain_user_state_for_testing alice_keys 4500

let bob_state = create_side_chain_user_state_for_testing bob_keys 17454

let trent_fee_schedule =
  { deposit_fee= TokenAmount.of_int 5
  ; per_account_limit= TokenAmount.of_int 20000
  ; fee_per_billion= TokenAmount.of_int 42 }

let confirmed_trent_state =
  { previous_main_chain_state = Digest.zero
  ; previous_side_chain_state = Digest.one
  ; facilitator_revision = Revision.of_int 17
  ; spending_limit= TokenAmount.of_int 1000000
  ; bond_posted= TokenAmount.of_int 5000000
  ; accounts = AddressMap.empty
  ; user_keys = AddressMap.empty
  ; operations = AddressMap.empty
  ; deposited = DigestSet.empty
  }

let trent_state =
  { keypair= trent_keys
  ; previous= None
  ; current= confirmed_trent_state
  ; fee_schedule= trent_fee_schedule }



let%test "open_account_and_close_it" =
  let trent_account st = AddressMap.find trent_keys.address st.facilitators in
  (* Capture initial state, check that it is not open *)
  let alice_state_trent = trent_account alice_state in
  let pending_ops0 = alice_state_trent.pending_operations in
  assert (not (is_account_open (alice_state, trent_keys.address))) ;
  (* Step 1a: Alice can't close an account that doesn't exist *)
  ( close_account (alice_state, trent_keys.address) |>
      function
      | _, Ok _ -> raise (Internal_error "shouldn't be able to close a non-open account")
      | _, Error e -> assert (e = Already_closed)) ;
  (* Step 1: Alice issues request to open the account *)
  open_account (alice_state, trent_keys.address) |>
    function
    | _, Error e -> raise e
    | alice_state1, Ok signed_request1 ->
       let pending_ops1 = (trent_account alice_state1).pending_operations in
       (* Opening adds an episteme to the pending_operations *)
       assert (List.length pending_ops1 = List.length pending_ops0 + 1
               && List.tl pending_ops1 = pending_ops0) ;
       (* Step 2: Trent confirm open_account request *)
       confirm_request (trent_state, signed_request1) |>
         function
         | _, Error e -> raise e
         | trent_state1, Ok _ ->
            assert (is_account_open (alice_state1, trent_keys.address)) ;
            (* TODO: add step where Trent replies the confirmation,
               and Alice adds it to her episteme list *)
            (* Step 3a: Attempting to open the account twice should fail *)
            ( open_account (alice_state1, trent_keys.address) |>
                function
                | alice_state2, Ok _ -> raise (Internal_error "succeeded to open an account twice")
                | _, Error exn -> assert ( exn = Already_open ) ) ;
            (* Step 3b: *)
            close_account (alice_state1, trent_keys.address) |>
              function
              | _, Error e -> raise e
              | alice_state2, Ok signed_request2 ->
                 assert (not (is_account_open (alice_state2, trent_keys.address))) ;
                 confirm_request (trent_state1, signed_request2) |>
                   function
                   | _, Error e -> raise e
                   | trent_state2, Ok _ ->
                      (* Step 4: attempting to close the account twice should fail *)
                      close_account (alice_state2, trent_keys.address) |>
                        function
                        | _, Ok _ -> raise (Internal_error "Succeeded at double-closing the account")
                        | _, Error exn -> exn = Already_closed

open Lib
open Action
open Crypto
open Keypair
open Trie
open Main_chain
open Side_chain
open Main_chain_action
open Lens.Infix

(** Default (empty) state for a new facilitator *)
let new_account_state = AccountState.{balance= TokenAmount.zero; account_revision= Revision.zero}

(** User's view of the default (empty) state for a new facilitator *)
let new_user_account_state_per_facilitator =
  UserAccountStatePerFacilitator.{
    facilitator_validity= Confirmed; confirmed_state= new_account_state; pending_operations= []}

type account_lens = (facilitator_state, AccountState.t) Lens.t

let facilitator_account_lens address =
  facilitator_state_current |-- State.accounts
  |-- defaulting_lens (konstant new_account_state) (AccountMap.lens address)

(** Given a signed request, handle the special case of opening an account, and return
    the request, the account state (new or old), the lens to set the account back at the end, and
    the user's public key *)

let ensure_user_account :
  (Request.t signed, Request.t signed * AccountState.t * account_lens * Address.t) facilitator_action =
  function
  | state, ({payload= {rx_header= {requester}}} as rx) ->
    let account_lens = facilitator_account_lens requester in
    let account_state = account_lens.get state in
    (state, Ok (rx, account_state, account_lens, requester))

(** Is the request well-formed?
    This function should include all checks that can be made without any non-local side-effect
    beside reading pure or monotonic data, which is allowed.
    Thus, we can later parallelize this check.
*)
let is_side_chain_request_well_formed :
  facilitator_state * (Request.t signed * AccountState.t * account_lens * Address.t) -> bool =
  function
  | ( state
    , ( { payload=
            { rx_header=
                { requester
                ; requester_revision }
            ; operation } as payload
        ; signature }
      , {balance; account_revision}
      , _
      , _signing_address ) ) ->
    requester_revision = Revision.add account_revision Revision.one
    (* TODO: check confirmed main & side chain state + validity window *)
    && is_signature_valid Request.digest requester signature payload
    (* Check that the numbers add up: *)
    &&
    match operation with
    | Deposit
        { deposit_amount
        ; deposit_fee
        ; main_chain_deposit_signed=
            { signature
            ; payload= {tx_header= {value}; operation= main_chain_operation} as payload
            } as main_chain_deposit_signed
        ; main_chain_deposit_confirmation
        ; deposit_expedited=_deposit_expedited } ->
      TokenAmount.compare value (TokenAmount.add deposit_amount deposit_fee) >= 0
      && ( match main_chain_operation with
        | Main_chain.Operation.TransferTokens recipient -> recipient = state.keypair.address
        | _ -> false )
      (* TODO: delegate the same signature checking protocol to the main chain *)
      && is_signature_valid Transaction.digest requester signature payload
      && Main_chain.is_confirmation_valid main_chain_deposit_confirmation
           main_chain_deposit_signed
      && TokenAmount.compare deposit_fee state.fee_schedule.deposit_fee >= 0
    | Payment {payment_invoice; payment_fee; payment_expedited=_payment_expedited} ->
      TokenAmount.compare balance (TokenAmount.add payment_invoice.amount payment_fee) >= 0
      (* TODO: make per_account_limit work on the entire floating thing *)
      && TokenAmount.compare state.fee_schedule.per_account_limit payment_invoice.amount >= 0
      (* TODO: make sure the fee multiplication cannot overflow! *)
      && TokenAmount.compare payment_fee
           (TokenAmount.mul state.fee_schedule.fee_per_billion
              (TokenAmount.div payment_invoice.amount (TokenAmount.of_int 1000000000)))
         >= 0
    | Withdrawal {withdrawal_amount; withdrawal_fee} ->
      TokenAmount.compare balance (TokenAmount.add withdrawal_amount withdrawal_fee) >= 0
      && TokenAmount.compare withdrawal_fee state.fee_schedule.withdrawal_fee >= 0

(** Check that the request is basically well-formed, or else fail *)
let check_side_chain_request_well_formed = action_assert __LOC__ is_side_chain_request_well_formed

let make_request_confirmation :
  (Request.t signed * AccountState.t * account_lens, Confirmation.t signed) facilitator_action =
  fun (facilitator_state, (signed_request, account_state, account_lens)) ->
    let revision = Revision.add facilitator_state.current.facilitator_revision Revision.one in
    ( account_lens.set
        {account_state with account_revision= signed_request.payload.rx_header.requester_revision}
        ((facilitator_state_current |-- State.facilitator_revision).set revision facilitator_state)
    , Ok
        (sign Confirmation.digest facilitator_state.keypair.private_key
           { tx_header=TxHeader.{ tx_revision= revision
                                ; updated_limit= facilitator_state.current.spending_limit}
           ; Confirmation.signed_request }) )

exception Spending_limit_exceeded

(** Facilitator actions to use up some of the limit *)
let spend_spending_limit amount (state, x) =
  if TokenAmount.compare amount state.current.spending_limit <= 0 then
    ( (facilitator_state_current |-- State.spending_limit).set
        (TokenAmount.sub state.current.spending_limit amount)
        state
    , Ok x )
  else (state, Error Spending_limit_exceeded)

let maybe_spend_spending_limit is_expedited amount (state, x) =
  if is_expedited then spend_spending_limit amount (state, x) else (state, Ok x)

exception Already_posted

(* To prevent double-deposit or double-withdrawal of a same main_chain_transaction_signed,
   we put those transactions in a set of already posted transactions.
   (Future: prune that set by expiring deposit requests?
   Have more expensive process to account for old deposits?)
*)
let check_against_double_accounting main_chain_transaction_signed (state, x) =
  let witness = Main_chain.TransactionSigned.digest main_chain_transaction_signed in
  let lens =
    facilitator_state_current |-- State.main_chain_transactions_posted |-- DigestSet.lens witness
  in
  if lens.get state then (state, Error Already_posted) else (lens.set true state, Ok x)

(** compute the effects of a request on the account state *)
let effect_request :
  ( Request.t signed * AccountState.t * account_lens * Address.t
  , Request.t signed * AccountState.t * account_lens )
    facilitator_action = function
  | state, (({payload= {operation}} as rx), account_state, account_lens, _user_key) ->
    match operation with
    | Deposit
        { deposit_amount
        ; deposit_fee=_deposit_fee
        ; main_chain_deposit_signed
        ; main_chain_deposit_confirmation=_main_chain_deposit_confirmation
        ; deposit_expedited } ->
      ( state
      , ( rx
        , Lens.modify AccountState.balance (TokenAmount.add deposit_amount) account_state
        , account_lens ) )
      ^|> check_against_double_accounting main_chain_deposit_signed
      ^>> maybe_spend_spending_limit deposit_expedited deposit_amount
    | Payment {payment_invoice; payment_fee; payment_expedited} ->
      ( Lens.modify
          (facilitator_account_lens payment_invoice.recipient |-- AccountState.balance)
          (TokenAmount.add payment_invoice.amount)
          state
      , ( rx
        , { account_state with
            balance= TokenAmount.sub account_state.balance
                (TokenAmount.add payment_invoice.amount payment_fee) }
        , account_lens ) )
      ^|> maybe_spend_spending_limit payment_expedited payment_invoice.amount
    | Withdrawal {withdrawal_amount; withdrawal_fee} ->
      ( state
      , Ok
          ( rx
          , { account_state with
              balance= TokenAmount.sub account_state.balance
                  (TokenAmount.add withdrawal_amount withdrawal_fee) }
          , account_lens ) )

(** TODO:
 * save this initial state, and only use the new state if the confirmation was committed to disk,
    i.e. implement a try-catch in our monad
 * commit the confirmation to disk and remote replicas before to return it
 * parallelize, batch, etc., to have decent performance
*)
let confirm_request : (Request.t signed, Confirmation.t signed) facilitator_action =
  ensure_user_account ^>> check_side_chain_request_well_formed ^>> effect_request
  ^>> make_request_confirmation

let stub_confirmed_main_chain_state = ref Main_chain.genesis_state

let stub_confirmed_main_chain_state_digest = ref (Main_chain.State.digest Main_chain.genesis_state)

let genesis_side_chain_state =
  State.{
    previous_main_chain_state= null_digest
  ; previous_side_chain_state= null_digest
  ; facilitator_revision= Revision.zero
  ; spending_limit= TokenAmount.zero
  ; bond_posted= TokenAmount.zero
  ; accounts= AccountMap.empty
  ; operations= ConfirmationMap.empty
  ; main_chain_transactions_posted= DigestSet.empty }

let stub_confirmed_side_chain_state = ref genesis_side_chain_state

let stub_confirmed_side_chain_state_digest = ref (State.digest genesis_side_chain_state)

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
  {request= rx; confirmation_option= None; main_chain_confirmation_option= None}

let [@warning "-32"] mk_tx_episteme tx =
  { request= tx.payload.Confirmation.signed_request
  ; confirmation_option= Some tx
  ; main_chain_confirmation_option= None }

(** TODO: Handle cases of updates to previous epistemes, rather than just new ones *)
let add_user_episteme user_state episteme =
  let facilitator = episteme.request.payload.rx_header.facilitator in
  let account_state =
    UserAccountStateMap.find_defaulting
      (konstant new_user_account_state_per_facilitator)
      facilitator user_state.facilitators
  in
  ( user_state_facilitators |-- UserAccountStateMap.lens facilitator
    |-- UserAccountStatePerFacilitator.pending_operations )
  .set (episteme :: account_state.pending_operations) user_state

let issue_user_request =
  (fun (user_state, operation) ->
     (user_state, ()) ^|> get_first_facilitator ^>> make_rx_header
     ^>> action_of_pure_action (fun (user_state, rx_header) ->
       sign Request.digest user_state.main_chain_user_state.keypair.private_key
         {Request.rx_header; Request.operation} ) )
  ^>> fun (user_state, request) ->
    (add_user_episteme user_state (mk_rx_episteme request), Ok request)

(** We assume that the operation will correctly apply:
    balances are sufficient for spending,
    deposits confirmation will check out,
    active revision will only increase, etc.
*)
let update_account_state_with_trusted_operation
      trusted_operation ({AccountState.balance} as account_state) =
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
  | None -> new_account_state
  | Some {facilitator_validity; confirmed_state; pending_operations} ->
    match facilitator_validity with
    | Rejected -> confirmed_state
    | _ ->
      update_account_state_with_trusted_operations
        (List.map (fun x -> x.request.payload.operation) pending_operations)
        confirmed_state

let lift_main_chain_user_async_action_to_side_chain async_action (user_state, input) =
  let open Lwt in
  async_action (user_state.main_chain_user_state, input)
  >>= fun (new_state, result) ->
  return ({user_state with main_chain_user_state= new_state}, result)

(* TODO: make config item *)
let deposit_fee = TokenAmount.of_int 5

let deposit ((user_state, (_facilitator_address, deposit_amount)) as input) =
(* in Lwt monad, because there's a transfer of tokens in the main chain *)
  let open Lwt in
  input
  |> lift_main_chain_user_async_action_to_side_chain transfer_tokens
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
let withdrawal (user_state, (facilitator_address, withdrawal_amount)) =
  Lwt.return (issue_user_request
                ( user_state
                , Withdrawal
                    { withdrawal_amount= TokenAmount.sub withdrawal_amount withdrawal_fee
                    ; withdrawal_fee
                    } ))

let payment (user_state, (_facilitator_address, recipient_address, payment_amount)) =
  let invoice = Invoice.{recipient= recipient_address; amount= payment_amount; memo= None} in
  issue_user_request
    ( user_state
    , Payment
        { payment_invoice= invoice
        ; payment_fee= TokenAmount.of_int 0 (* TODO: configuration item *)
        ; payment_expedited= false } )

let make_main_chain_withdrawal_transaction { withdrawal_amount; withdrawal_fee } user_address facilitator_state =
    let open Ethereum_abi in
    let open Ethereum_transaction in
    let contract_address = "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" in (* TODO : use actual contract address *)
    let contract_hex_address = Ethereum_util.address_of_hex_string contract_address in
    let facilitator_keys = facilitator_state.keypair in
    let facilitator_address = facilitator_keys.address in
    let amount = withdrawal_amount |> TokenAmount.to_int in
    let withdrawal_call =
      { function_name = "withdrawTokens"
      ; parameters =
          [ abi_address_of_address facilitator_address
          ; abi_address_of_address user_address
          ; abi_uint_of_int amount
          ]
      }
    in
    let encoded_call = encode_function_call withdrawal_call in
    let operation = Main_chain.CallFunction (contract_hex_address, encoded_call) in
    let tx_header =
      { sender= facilitator_address
      ; nonce= Nonce.zero (* TODO: get_nonce facilitator_address *)
      ; gas_price= TokenAmount.of_int 2 (* TODO: what are the right gas policies? *)
      ; gas_limit= TokenAmount.of_int 1000000
      ; value= TokenAmount.zero
      }
    in
    let transaction = { tx_header; operation } in
    sign facilitator_keys.private_key transaction

(* an action made on the side chain may need a corresponding action on the main chain *)
let push_side_chain_action_to_main_chain facilitator_state ((user_state : Side_chain.user_state), (signed_confirmation : Confirmation.t signed)) =
  let confirmation = signed_confirmation.payload in
  let facilitator_address = facilitator_state.keypair.address in
  if not (is_signature_valid facilitator_address signed_confirmation.signature confirmation) then
    raise (Internal_error "Invalid facilitator signature on signed confirmation");
  let signed_request = confirmation.signed_request in
  let request = signed_request.payload in
  let user_keys = user_state.main_chain_user_state.keypair in
  let user_address = user_keys.address in
  if not (is_signature_valid user_address signed_request.signature request) then
    raise (Internal_error "Invalid user signature on signed request");
  match request.operation with
  | Withdrawal details ->
    let open Lwt in
    let signed_transaction = make_main_chain_withdrawal_transaction details user_address facilitator_state in
    wait_for_confirmation (user_state.main_chain_user_state,signed_transaction)
    >>= fun (main_chain_user_state,main_chain_confirmation) ->
    return ({ user_state with main_chain_user_state },main_chain_confirmation)
  | Payment _
  | Deposit _ ->
    raise (Internal_error "Side chain confirmation does not need subsequent interaction with main chain")

let detect_main_chain_facilitator_issues = bottom

let collect_account_liquidation_funds = bottom

let send_user_request = bottom

let send_facilitator_confirmation = bottom

(** missing types to be implemented *)

type facilitator_to_facilitator_message

type user_to_user_message

let [@warning "-32"] send_certified_check _check _conv = bottom

let commit_facilitator_state = bottom

let send_message = bottom

let request_account_liquidation = bottom

let check_main_chain_for_exits = bottom

let initiate_individual_exit = bottom

let request_deposit = bottom

module Test = struct
  open Lwt

  (* open account tests *)

  let trent_keys =
    Keypair.make_keys_from_hex
      "b6:fb:0b:7e:61:36:3e:e2:f7:48:16:13:38:f5:69:53:e8:aa:42:64:2e:99:90:ef:f1:7e:7d:e9:aa:89:57:86"
      "04:26:bd:98:85:f2:c9:e2:3d:18:c3:02:5d:a7:0e:71:a4:f7:ce:23:71:24:35:28:82:ea:fb:d1:cb:b1:e9:74:2c:4f:e3:84:7c:e1:a5:6a:0d:19:df:7a:7d:38:5a:21:34:be:05:20:8b:5d:1c:cc:5d:01:5f:5e:9a:3b:a0:d7:df"

  let trent_address = trent_keys.address

  let alice_keys =
    Keypair.make_keys_from_hex
      "d5:69:84:dc:08:3d:76:97:01:71:4e:eb:1d:4c:47:a4:54:25:5a:3b:bc:3e:9f:44:84:20:8c:52:bd:a3:b6:4e"
      "04:23:a7:cd:9a:03:fa:9c:58:57:e5:14:ae:5a:cb:18:ca:91:e0:7d:69:45:3e:d8:51:36:ea:6a:00:36:10:67:b8:60:a5:b2:0f:11:53:33:3a:ef:2d:1b:a1:3b:1d:7a:52:de:28:69:d1:f6:23:71:bf:81:bf:80:3c:21:c6:7a:ca"

  let alice_address = alice_keys.address

  let bob_keys =
    Keypair.make_keys_from_hex
      "f1:d3:cd:20:22:e1:d6:64:98:32:76:04:83:4d:f0:73:06:64:f7:1a:8d:d1:1e:46:a3:3b:4a:0e:bb:40:ca:8e"
      "04:7d:52:54:04:9f:02:3e:e7:aa:ea:1e:fa:4f:17:ae:70:0f:af:67:23:24:02:5a:a9:b5:32:5a:92:1f:d0:f1:51:0e:68:31:f1:bf:90:b4:a1:df:e1:cd:49:e5:03:ec:7d:b5:9f:6e:78:73:d0:3a:3a:09:6c:46:5c:87:22:22:69"

  let bob_address = bob_keys.address

  let create_side_chain_user_state_for_testing user_keys =
    let main_chain_user_state =
      { keypair= user_keys
      ; confirmed_state= Digest.zero
      ; confirmed_balance= TokenAmount.zero
      ; pending_transactions= []
      ; nonce= Nonce.zero }
    in
    let user_account_state = new_user_account_state_per_facilitator in
    let facilitators = UserAccountStateMap.singleton trent_address user_account_state in
    {main_chain_user_state; facilitators}

  let alice_state = create_side_chain_user_state_for_testing alice_keys

  let trent_fee_schedule =
    { deposit_fee= TokenAmount.of_int 5
    ; withdrawal_fee= TokenAmount.of_int 5
    ; per_account_limit= TokenAmount.of_int 20000
    ; fee_per_billion= TokenAmount.of_int 42 }

  let confirmed_trent_state =
    State.{ previous_main_chain_state= Digest.zero
          ; previous_side_chain_state= Digest.one
          ; facilitator_revision= Revision.of_int 17
          ; spending_limit= TokenAmount.of_int 1000000
          ; bond_posted= TokenAmount.of_int 5000000
          ; accounts= AccountMap.empty
          ; operations= ConfirmationMap.empty
          ; main_chain_transactions_posted= DigestSet.empty }

  let trent_state =
    { keypair= trent_keys
    ; previous= None
    ; current= confirmed_trent_state
    ; fee_schedule= trent_fee_schedule }

  let ( |^>> ) v f = v |> f |> function state, Ok x -> (state, x) | _state, Error y -> raise y
  (* Lwt-monadic version of |^>> *)
  let ( |^>>+ ) v f = v |> f >>= function (state, Ok x) -> return (state, x) | state, Error y -> raise y

  (* deposit and payment test *)
  let%test "deposit_valid" =
    Lwt_main.run (
      let amount_to_deposit = TokenAmount.of_int 523 in
      (* deposit *)
      (alice_state, (trent_address, amount_to_deposit))
      |^>>+ deposit
      >>= fun (alice_state1, signed_request1) ->
      (trent_state, signed_request1) |^>> confirm_request
      |> fun (trent_state1, _signed_confirmation1) ->
      (* verify the deposit to Alice's account on Trent *)
      let trent_accounts = trent_state1.current.accounts in
      let alice_account = AccountMap.find alice_address trent_accounts in
      let alice_expected_deposit = TokenAmount.sub amount_to_deposit deposit_fee in
      assert (alice_account.balance = alice_expected_deposit) ;
      (* open Bob's account *)
      let payment_amount = TokenAmount.of_int 17 in
      (alice_state1, (trent_address, bob_address, payment_amount))
      |^>> payment
      |> fun (_alice_state2, signed_request2) ->
      (trent_state1, signed_request2) |^>> confirm_request
      |> fun (trent_state2, _signed_confirmation2) ->
      (* verify the payment to Bob's account on Trent *)
      let trent_accounts_after_payment = trent_state2.current.accounts in
      let get_trent_account name address =
        try AccountMap.find address trent_accounts_after_payment with Not_found ->
          raise (Internal_error (name ^ " has no account on Trent after payment"))
      in
      let alice_account = get_trent_account "Alice" alice_address in
      let bob_account = get_trent_account "Bob" bob_address in
      (* Alice has payment debited from her earlier deposit; Bob has just the payment in his account *)
      let alice_expected_balance = TokenAmount.sub alice_expected_deposit payment_amount in
      let bob_expected_balance = payment_amount in
      assert (alice_account.balance = alice_expected_balance) ;
      assert (bob_account.balance = bob_expected_balance) ;
      return true
    )

  (* deposit and withdrawal test *)
  let%test "withdrawal_valid" =
    Lwt_main.run (
      (* deposit some funds first *)
      let amount_to_deposit = TokenAmount.of_int 1023 in
      (* deposit *)
      (alice_state, (trent_address, amount_to_deposit))
      |^>>+ deposit
      >>= fun (alice_state1, signed_request1) ->
      (trent_state, signed_request1) |^>> confirm_request
      |> fun (trent_state1, signed_confirmation1) ->
      (* verify the deposit to Alice's account on Trent *)
      let trent_accounts = trent_state1.current.accounts in
      let alice_account = AccountMap.find alice_address trent_accounts in
      let alice_expected_deposit = TokenAmount.sub amount_to_deposit deposit_fee in
      assert (alice_account.balance = alice_expected_deposit) ;
      (* withdrawal back to main chain *)
      let amount_to_withdraw = TokenAmount.of_int 42 in
      (alice_state1, (trent_address, amount_to_withdraw))
      |^>>+ withdrawal
      >>= fun (alice_state2, signed_request2) ->
      (trent_state1, signed_request2) |^>> confirm_request
      |> fun (trent_state2, signed_confirmation2) ->
      let trent_accounts_after_withdrawal = trent_state2.current.accounts in
      let alice_account_after_withdrawal = AccountMap.find alice_address trent_accounts_after_withdrawal in
      let alice_expected_withdrawal = TokenAmount.sub alice_expected_deposit amount_to_withdraw in
      assert (alice_account_after_withdrawal.balance = alice_expected_withdrawal);
      push_side_chain_action_to_main_chain trent_state2 (alice_state2,signed_confirmation2)
      (* TODO: get actual transaction receipt from main chain, check receipt
         maybe this test belongs in Ethereum_transactions
      *)
      >>= fun json -> return true
    )
end

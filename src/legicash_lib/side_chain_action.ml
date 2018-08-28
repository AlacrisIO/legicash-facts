open Legilogic_lib
open Lib
open Hex
open Action
open Yojsoning
open Signing
open Types

open Legilogic_ethereum
open Main_chain

open Side_chain
open Side_chain_facilitator
open Side_chain_user

(** missing types to be implemented *)

module Test = struct
  open Lwt
  open Signing.Test
  open Ethereum_transaction.Test

  let%test "move logs aside" = Logging.log_to_file "test.log"; true

  (* open account tests *)

  let create_side_chain_user_state_for_testing user_keys =
    let main_chain_user_state = Main_chain.UserState.init user_keys in
    let trent_state = get_facilitator_state () in
    let confirmed_state = (facilitator_account_lens user_keys.address).get trent_state in
    let user_account_state = {UserAccountState.empty with confirmed_state} in
    let facilitators = UserAccountStateMap.singleton trent_address user_account_state in
    UserState.{main_chain_user_state; facilitators; notifications = []}

  let make_alice_state () = create_side_chain_user_state_for_testing alice_keys

  (* create accounts, fund them *)
  let create_account_on_testnet keys =
    let open Ethereum_json_rpc in
    (* get hex string version of private key *)
    let private_key_string = keys.Keypair.private_key |> PrivateKey.marshal_string |> unparse_hex_string in
    let password = "" in
    let json = build_json_rpc_call Personal_importRawKey [private_key_string; password] in
    send_rpc_call_to_net json
    >>= fun result_json ->
    let json_keys = YoJson.keys result_json in
    let _ =
      (* OK if we successfully added account, or it existed already *)
      List.mem "result" json_keys ||
      let error_json = YoJson.member "error" result_json in
      let error_message = YoJson.member "message" error_json |> YoJson.to_string in
      error_message = "account already exists" ||
      bork "%s" error_message
    in
    return ()

  let get_prefunded_address () =
    get_first_account ()
    >>= fun first_account ->
    YoJson.to_string first_account
    |> Address.of_0x_string
    |> Lwt.return

  let fund_account ?(min_balance=1000000000) funding_account (keys : Keypair.t) =
    let open Lwt in
    let open Ethereum_transaction in
    send_balance_request_to_net keys.address
    >>= fun json ->
    let json_keys = YoJson.keys json in
    if List.mem "error" json_keys then (
      let error = YoJson.member "error" json |> string_of_yojson in
      bork "%s" error
    );
    let balance = YoJson.member "result" json |> YoJson.to_string |> int_of_string in
    let deficit = min_balance - balance in
    if deficit > 0 then
      let tx_header =
        Main_chain.TxHeader.
          { sender= funding_account
          ; nonce= Nonce.zero
          ; gas_price= TokenAmount.of_int 1
          ; gas_limit= TokenAmount.of_int 1000000
          ; value= TokenAmount.of_int deficit}
      in
      let operation = Main_chain.Operation.TransferTokens keys.address in
      let transaction = Main_chain.{Transaction.tx_header; Transaction.operation} in
      let signed_transaction = Main_chain.Transaction.signed trent_keys transaction in
      send_transaction_to_net signed_transaction
      >>= fun json ->
      let json_keys = YoJson.keys json in
      if List.mem "error" json_keys then
        let error = YoJson.member "error" json |> string_of_yojson in
        bork "%s" error
      else
        return ()
    else
      return ()

  let fund_accounts () =
    get_prefunded_address ()
    >>= fun prefunded_address ->
    unlock_account prefunded_address
    >>= fun _ ->
    Lwt_list.iter_s
      (fun keys ->
         create_account_on_testnet keys
         >>= fun () ->
         unlock_account keys.address
         >>= fun _ ->
         fund_account prefunded_address keys)
      [alice_keys; bob_keys; trent_keys]

  let install_contract () =
    let open Lwt in
    let open Main_chain in
    let open Ethereum_transaction in
    Test.get_first_account ()
    >>= fun contract_account_json ->
    let contract_account = YoJson.to_string contract_account_json in
    let contract_address = Address.of_0x_string contract_account in
    Test.unlock_account contract_address
    >>= fun unlock_contract_json ->
    assert_json_error_free __LOC__ unlock_contract_json;
    let tx_header =
      TxHeader.{ sender= contract_address
               ; nonce= Nonce.zero
               ; gas_price= TokenAmount.of_int 1
               ; gas_limit= TokenAmount.of_int 1000000
               ; value= TokenAmount.zero }
    in
    let operation = Operation.CreateContract Facilitator_contract_binary.contract_bytes in
    let transaction = { Transaction.tx_header; Transaction.operation } in
    let signed_transaction = Transaction.signed Signing.Test.alice_keys transaction in
    send_transaction_to_net signed_transaction
    >>= fun output ->
    assert_json_error_free __LOC__ output;
    let result_json = YoJson.member "result" output in
    let transaction_hash = YoJson.to_string result_json in
    Test.wait_for_contract_execution transaction_hash
    >>= fun () ->
    get_transaction_receipt transaction_hash
    >>= fun receipt_json ->
    assert_json_error_free __LOC__ receipt_json;
    let receipt_result_json = YoJson.member "result" receipt_json in
    let contract_address =
      YoJson.member "contractAddress" receipt_result_json
      |> Address.of_yojson_exn
    in
    Facilitator_contract.set_contract_address contract_address;
    return ()

  (* deposit and payment test *)
  let%test "deposit_and_payment_valid" =
    Lwt_main.run (
      start_facilitator trent_address
      >>= fun () ->
      fund_accounts ()
      >>= fun () ->
      install_contract ()
      >>= fun () ->
      unlock_account alice_keys.address
      >>= fun _ ->
      let amount_to_deposit = TokenAmount.of_int 523 in
      let alice_state_ref = ref (make_alice_state ()) in
      UserAsyncAction.run_lwt alice_state_ref get_facilitator_fee_schedule ()
      >>= fun fee_schedule ->
      UserAsyncAction.run_lwt alice_state_ref deposit (trent_address, amount_to_deposit)
      >>= fun signed_request1 ->
      process_request (signed_request1, false)
      >>= fun confirmation_or_exn1 ->
      ignore (Result.get confirmation_or_exn1);
      let trent_state1 = get_facilitator_state () in
      (* TODO: maybe examine the log for the contract call *)
      (* verify the deposit to Alice's account on Trent *)
      let trent_accounts = trent_state1.current.accounts in
      let alice_account = Side_chain.AccountMap.find alice_address trent_accounts in
      let alice_expected_deposit = amount_to_deposit in
      assert (alice_account.balance = alice_expected_deposit) ;
      (* open Bob's account *)
      let payment_amount = TokenAmount.of_int 17 in
      UserAsyncAction.run_lwt alice_state_ref payment (trent_address, bob_address, payment_amount)
      >>= fun signed_request2 ->
      Lwt_exn.run_lwt process_request (signed_request2, false)
      >>= fun _signed_confirmation2 ->
      (* verify the payment to Bob's account on Trent *)
      let trent_state2 = get_facilitator_state () in
      let trent_accounts_after_payment = trent_state2.current.accounts in
      let get_trent_account name address =
        try Side_chain.AccountMap.find address trent_accounts_after_payment with Not_found ->
          bork "%s has no account on Trent after payment" name in
      let alice_account = get_trent_account "Alice" alice_address in
      let bob_account = get_trent_account "Bob" bob_address in
      (* Alice has payment debited from her earlier deposit; Bob has just the payment in his account *)
      let payment_fee = payment_fee_for fee_schedule payment_amount in
      let alice_expected_balance =
        TokenAmount.(sub alice_expected_deposit (add payment_amount payment_fee)) in
      let bob_expected_balance = payment_amount in
      assert (alice_account.balance = alice_expected_balance) ;
      assert (bob_account.balance = bob_expected_balance) ;
      (* test whether retrieving saved facilitator state yields the same state
         like similar test in Side_chain.Test;  here we have nonempty account, confirmation maps *)
      let trent_state3 = get_facilitator_state () in
      Side_chain.FacilitatorState.save trent_state3
      >>= Db.commit
      >>= fun () ->
      let retrieved_state = Side_chain.FacilitatorState.load trent_address in
      return (FacilitatorState.to_yojson_string retrieved_state
              = FacilitatorState.to_yojson_string trent_state3))

  (* deposit and withdrawal test *)
  let%test "withdrawal_valid" =
    Lwt_main.run (
      start_facilitator trent_address
      >>= fun () ->
      (* previous test installs contract on test net *)
      unlock_account ~duration:60 alice_keys.address
      >>= fun _ ->
      (* deposit some funds first *)
      let amount_to_deposit = TokenAmount.of_int 1023 in
      let alice_state_ref = ref (make_alice_state ()) in
      let initial_balance =
        (UserAccountStateMap.find trent_address !alice_state_ref.facilitators).confirmed_state.balance in
      UserAsyncAction.run_lwt alice_state_ref get_facilitator_fee_schedule ()
      >>= fun fee_schedule ->
      (* deposit *)
      UserAsyncAction.run_lwt alice_state_ref deposit (trent_address, amount_to_deposit)
      >>= fun signed_request1 ->
      Lwt_exn.run_lwt process_request (signed_request1, false)
      >>= fun _confirmation1 ->
      let trent_state1 = get_facilitator_state () in
      (* verify the deposit to Alice's account on Trent *)
      let trent_accounts = trent_state1.current.accounts in
      let alice_account = Side_chain.AccountMap.find alice_address trent_accounts in
      let alice_expected_deposit = amount_to_deposit in
      let alice_balance_expected_after_deposit = TokenAmount.add initial_balance alice_expected_deposit in
      assert (alice_account.balance = alice_balance_expected_after_deposit);
      (* withdrawal back to main chain *)
      let amount_to_withdraw = TokenAmount.of_int 42 in
      let withdrawal_fee = fee_schedule.withdrawal_fee in
      UserAsyncAction.run_lwt alice_state_ref withdrawal (trent_address, amount_to_withdraw)
      >>= fun signed_request2 ->
      Lwt_exn.run_lwt process_request (signed_request2, false)
      >>= fun confirmation2 ->
      let trent_state2 = get_facilitator_state () in
      let trent_accounts_after_withdrawal = trent_state2.current.accounts in
      let alice_account_after_withdrawal =
        Side_chain.AccountMap.find alice_address trent_accounts_after_withdrawal in
      let alice_expected_withdrawal =
        TokenAmount.sub alice_balance_expected_after_deposit
          (TokenAmount.add amount_to_withdraw withdrawal_fee) in
      assert (alice_account_after_withdrawal.balance = alice_expected_withdrawal);
      let trent_state3 = get_facilitator_state () in
      UserAsyncAction.run_lwt alice_state_ref
        (push_side_chain_action_to_main_chain trent_state3) confirmation2
      (* TODO: get actual transaction receipt from main chain, check receipt
         maybe this t est belongs in Ethereum_transactions
      *)
      >>= fun _ -> return true)
end

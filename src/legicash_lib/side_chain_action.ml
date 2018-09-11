open Legilogic_lib
open Lib
open Signing
open Action
open Lwt_exn
open Types

open Legilogic_ethereum
open Main_chain

open Side_chain
open Side_chain_facilitator
open Side_chain_user

module Test = struct
  open Signing.Test
  open Ethereum_transaction.Test
  open Side_chain_facilitator.Test

  let%test "move logs aside" = Logging.set_log_file "test.log"; true

  (* open account tests *)

  let create_side_chain_user_state_for_testing user_keys =
    let main_chain_user_state = Main_chain.UserState.init user_keys in
    let trent_state = get_facilitator_state () in
    let confirmed_state = (facilitator_account_lens user_keys.address).get trent_state in
    let user_account_state = {UserAccountState.empty with confirmed_state} in
    let facilitators = UserAccountStateMap.singleton trent_address user_account_state in
    UserState.{main_chain_user_state; facilitators;
               notification_counter = Revision.zero; notifications = []}

  let make_alice_state () = create_side_chain_user_state_for_testing alice_keys

  (* create accounts, fund them *)
  let create_account_on_testnet : (keypair, address) Lwt_exn.arr =
    fun keys ->
      let password = "" in
      Ethereum_transaction.ensure_private_key (keys, password)

  let get_prefunded_address = get_first_account

  let fund_account ?(min_balance=TokenAmount.of_int 1000000000)
        funding_account (address : Address.t) =
    Ethereum_json_rpc.eth_get_balance (address, Latest)
    >>= fun balance ->
    if TokenAmount.compare min_balance balance > 0 then
      let tx_header =
        Main_chain.TxHeader.
          { sender= funding_account
          ; nonce= Nonce.zero (* TODO: if we need this, it should be properly synched, if not, it shouldn't be needed in the interface *)
          ; gas_price= TokenAmount.of_int 1
          ; gas_limit= TokenAmount.of_int 1000000
          ; value= min_balance} in
      let operation = Main_chain.Operation.TransferTokens address in
      let transaction = Main_chain.{Transaction.tx_header; Transaction.operation} in
      Ethereum_json_rpc.eth_send_transaction transaction
      >>= const ()
    else
      return ()

  let fund_accounts () =
    get_prefunded_address ()
    >>= fun prefunded_address ->
    unlock_account prefunded_address
    >>= fun _ ->
    list_iter_s
      (fun keys ->
         create_account_on_testnet keys
         >>= unlock_account
         >>= fun _ -> fund_account prefunded_address keys.address)
      [alice_keys; bob_keys; trent_keys]

  let contract_address_key = "legicash.contract-address"

  (* TODO: use beyond testing *)
  let install_contract () =
    let open Main_chain in
    (* TODO: use real side chain account *)
    get_first_account ()
    >>= fun contract_address ->
    unlock_account contract_address
    >>= fun _unlock_contract_json ->
    let tx_header =
      TxHeader.{ sender= contract_address
               ; nonce= Nonce.zero
               ; gas_price= TokenAmount.of_int 1
               ; gas_limit= TokenAmount.of_int 1000000
               ; value= TokenAmount.zero }
    in
    let operation = Operation.CreateContract Facilitator_contract_binary.contract_bytes in
    let transaction = { Transaction.tx_header; Transaction.operation } in
    Ethereum_json_rpc.eth_send_transaction transaction
    >>= fun transaction_hash ->
    wait_for_contract_execution transaction_hash
    >>= fun () ->
    Ethereum_json_rpc.eth_get_transaction_receipt transaction_hash
    >>= arr Option.get
    >>= fun receipt ->
    let contract_address = Option.get receipt.contractAddress in
    Facilitator_contract.set_contract_address contract_address;
    Address.to_0x_string contract_address
    |> (of_lwt (Db.put contract_address_key) >>> of_lwt Db.commit)

  let load_contract () =
    match Db.get contract_address_key with
    | Some addr ->
      Facilitator_contract.set_contract_address (Address.of_0x_string addr);
      return ()
    | None -> bork "Could not load contract address"

  (* deposit and payment test *)
  let%test "deposit_and_payment_valid" =
    Lwt_exn.run
      (fun () ->
         start_facilitator trent_address
         >>= fund_accounts
         >>= install_contract
         >>= fun () ->
         unlock_account alice_keys.address
         >>= fun _ ->
         let amount_to_deposit = TokenAmount.of_int 523 in
         let alice_state_ref = ref (make_alice_state ()) in
         UserAsyncAction.run_lwt_exn alice_state_ref deposit (trent_address, amount_to_deposit)
         >>= fun signed_request1 ->
         post_user_transaction_request signed_request1
         >>= fun _transaction1 ->
         let trent_state1 = get_facilitator_state () in
         (* TODO: maybe examine the log for the contract call *)
         (* verify the deposit to Alice's account on Trent *)
         let trent_accounts = trent_state1.current.accounts in
         let alice_account = Side_chain.AccountMap.find alice_address trent_accounts in
         let alice_expected_deposit = amount_to_deposit in
         assert (alice_account.balance = alice_expected_deposit) ;
         (* open Bob's account *)
         let payment_amount = TokenAmount.of_int 17 in
         UserAsyncAction.run_lwt_exn alice_state_ref payment (trent_address, bob_address, payment_amount, "")
         >>= fun signed_request2 ->
         post_user_transaction_request signed_request2
         >>= fun _transaction2 ->
         (* verify the payment to Bob's account on Trent *)
         let trent_state2 = get_facilitator_state () in
         let trent_accounts_after_payment = trent_state2.current.accounts in
         let get_trent_account name address =
           try return (Side_chain.AccountMap.find address trent_accounts_after_payment)
           with Not_found -> bork "%s has no account on Trent after payment" name in
         get_trent_account "Alice" alice_address
         >>= fun alice_account ->
         get_trent_account "Bob" bob_address
         >>= fun bob_account ->
         (* Alice has payment debited from her earlier deposit; Bob has just the payment in his account *)
         UserAsyncAction.run_lwt_exn alice_state_ref get_facilitator_fee_schedule ()
         >>= fun fee_schedule ->
         let payment_fee = payment_fee_for fee_schedule payment_amount in
         let alice_expected_balance =
           TokenAmount.(sub alice_expected_deposit (add payment_amount payment_fee)) in
         let bob_expected_balance = payment_amount in
         assert (alice_account.balance = alice_expected_balance) ;
         assert (bob_account.balance = bob_expected_balance) ;
         (* test whether retrieving saved facilitator state yields the same state
            like similar test in Side_chain.Test;  here we have nonempty account, confirmation maps *)
         let trent_state3 = get_facilitator_state () in
         of_lwt Side_chain.FacilitatorState.save trent_state3
         >>= of_lwt Db.commit
         >>= fun () ->
         let retrieved_state = Side_chain.FacilitatorState.load trent_address in
         return (FacilitatorState.to_yojson_string retrieved_state
                 = FacilitatorState.to_yojson_string trent_state3))
      ()

  (* deposit and withdrawal test *)
  let%test "withdrawal_valid" =
    Lwt_exn.run
      (fun () ->
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
         (* deposit *)
         UserAsyncAction.run_lwt_exn alice_state_ref deposit (trent_address, amount_to_deposit)
         >>= fun signed_request1 ->
         post_user_transaction_request signed_request1
         >>= fun _transaction1 ->
         let trent_state1 = get_facilitator_state () in
         (* verify the deposit to Alice's account on Trent *)
         let trent_accounts = trent_state1.current.accounts in
         let alice_account = Side_chain.AccountMap.find alice_address trent_accounts in
         let alice_expected_deposit = amount_to_deposit in
         let alice_balance_expected_after_deposit = TokenAmount.add initial_balance alice_expected_deposit in
         assert (alice_account.balance = alice_balance_expected_after_deposit);
         (* withdrawal back to main chain *)
         let amount_to_withdraw = TokenAmount.of_int 42 in
         UserAsyncAction.run_lwt_exn alice_state_ref get_facilitator_fee_schedule ()
         >>= fun fee_schedule ->
         let withdrawal_fee = fee_schedule.withdrawal_fee in
         UserAsyncAction.run_lwt_exn alice_state_ref withdrawal (trent_address, amount_to_withdraw)
         >>= fun signed_request2 ->
         post_user_transaction_request signed_request2
         >>= fun transaction2 ->
         let trent_state2 = get_facilitator_state () in
         let trent_accounts_after_withdrawal = trent_state2.current.accounts in
         let alice_account_after_withdrawal =
           Side_chain.AccountMap.find alice_address trent_accounts_after_withdrawal in
         let alice_expected_withdrawal =
           TokenAmount.sub alice_balance_expected_after_deposit
             (TokenAmount.add amount_to_withdraw withdrawal_fee) in
         assert (alice_account_after_withdrawal.balance = alice_expected_withdrawal);
         UserAsyncAction.run_lwt_exn alice_state_ref
           (push_side_chain_withdrawal_to_main_chain trent_address) transaction2
         (* TODO: get actual transaction receipt from main chain, check receipt
            maybe this t est belongs in Ethereum_transactions
         *)
         >>= fun _ -> return true)
      ()
end

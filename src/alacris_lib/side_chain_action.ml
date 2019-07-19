open Legilogic_lib
open Signing
open Action
open Lwt_exn

open Legilogic_ethereum
open Side_chain
open Side_chain_operator
open Side_chain_user
open Contract_config

let create_side_chain_contract installer =
  (** TODO: persist this signed transaction before to send it to the network, to avoid double-send *)
  Ethereum_user.create_contract ~sender:installer ~code:Operator_contract_binary.contract_bytes ?gas_limit:None ~value:TokenAmount.zero
  >>= Ethereum_user.confirm_pre_transaction installer
  >>= fun (_tx, _, confirmation) -> return confirmation.transaction_hash

(* TODO: for compatibility with our current Docker setup, we look for the file in the config/
   directory, but create it in the _run/config/ directory. We need a better story. *)
let ensure_side_chain_contract installer =
  ensure_contract_of_config_file "contract_config.json"
    (fun () -> create_side_chain_contract installer)

module Test = struct
  open Lib.Test
  open Signing.Test
  open Batch.Test
  open Side_chain_operator.Test

  let%test "move logs aside" = Logging.set_log_file "test.log"; true

  let get_user_balance address =
    (get_operator_state () |> (operator_account_lens address).get).balance
  let get_alice_balance () = get_user_balance alice_address
  let get_bob_balance () = get_user_balance bob_address

  (* deposit, payment and withdrawal test *)
  let%test "deposit_and_payment_and_withdrawal" =
    Signing.Test.register_test_keypairs ();

    Side_chain_client.Test.post_user_transaction_request_hook :=
      Side_chain_operator.oper_post_user_transaction_request;

    Lwt_exn.run
      (fun () ->
        Logging.log "deposit_and_payment_and_withdrawal, step 1";
        of_lwt Db.open_connection "unit_test_db"
        >>= fun () ->
        Logging.log "deposit_and_payment_and_withdrawal, step 2";
        (* TODO consolidate integration tests into single entry point with
         * shared initialization phase rather than leaving them scattered about
         * the repo. One problem with the present setup is we cannot shut down
         * the following reactor once flipping it on, meaning we're likely to
         * encounter subtle time-dependent bugs in future tests (until we
         * reorganize) *)
        Logging.log "deposit_and_payment_and_withdrawal, step 4";
        let operator = trent_address in
        ensure_test_accounts ()
        >>= fun _ ->
        Logging.log "deposit_and_payment_and_withdrawal, step 5";
        Mkb_json_rpc.init_mkb_server ()
        >>= fun _ -> start_operator_for_test operator
        >>= fun () -> State_update.Test.start_state_update_for_test_periodic_daemon operator
        >>= fun () ->
        Logging.log "deposit_and_payment_and_withdrawal, step 6";
        let initial_alice_balance = get_alice_balance () in
        let initial_bob_balance = get_bob_balance () in

        (* 1- Test deposit *)
        Logging.log "deposit_and_payment_and_withdrawal, step 7 alice_address=%s" (Address.to_0x alice_address);
        let deposit_amount = TokenAmount.of_string "500000000000000000" in
        User.transaction
          alice_address
          deposit
          DepositWanted.{ operator
                        ; deposit_amount
                        ; request_guid = Types.RequestGuid.nil
                        ; requested_at = Types.Timestamp.now () }
        >>= fun (_commitment, _confirmation) ->
        Logging.log "deposit_and_payment_and_withdrawal, step 8";
        let alice_balance_after_deposit = get_alice_balance () in
        expect_equal "Alice balance after deposit" TokenAmount.to_string
          alice_balance_after_deposit
          (TokenAmount.add initial_alice_balance deposit_amount);

        (* 2- Test payment *)
        Logging.log "deposit_and_payment_and_withdrawal, step 9";
        let payment_amount = TokenAmount.of_string "170000000000000000" in
        User.transaction
          alice_address
          payment
          PaymentWanted.{ operator
                        ; recipient         = bob_address
                        ; amount            = payment_amount
                        ; memo              = "test"
                        ; payment_expedited = false
                        ; request_guid      = Types.RequestGuid.nil
                        ; requested_at      = Types.Timestamp.now () }

        >>= fun (_commitment2, _confirmation2) ->
        Logging.log "deposit_and_payment_and_withdrawal, step 10";
        let bob_balance_after_payment = get_bob_balance () in
        expect_equal "Bob balance after payment"
          TokenAmount.to_string
          bob_balance_after_payment
          (TokenAmount.add initial_bob_balance payment_amount);
        get_operator_fee_schedule trent_address

        >>= fun fee_schedule ->
        Logging.log "deposit_and_payment_and_withdrawal, step 11";
        let payment_fee = payment_fee_for fee_schedule payment_amount in
        let alice_balance_after_payment = get_alice_balance () in
        expect_equal "Alice balance after payment"
          TokenAmount.to_string
          alice_balance_after_payment
          TokenAmount.(sub alice_balance_after_deposit
                         (add payment_amount payment_fee));

        (* 3- Test Withdrawal -- withdraw all that was deposited *)
        let withdrawal_amount = TokenAmount.sub payment_amount fee_schedule.withdrawal_fee in
        User.transaction
          bob_address
          withdrawal
          WithdrawalWanted.{ operator
                           ; withdrawal_amount
                           ; request_guid = Types.RequestGuid.nil
                           ; requested_at = Types.Timestamp.now () }

        >>= fun (_commitment, _confirmation) ->
        Logging.log "deposit_and_payment_and_withdrawal, step 12";
        let bob_balance_after_withdrawal = get_bob_balance () in
        expect_equal "Bob balance after withdrawal" TokenAmount.to_string
          bob_balance_after_withdrawal
          initial_bob_balance;
        (* TODO: check main-chain balance, too! *)

        return true)
      ()
end

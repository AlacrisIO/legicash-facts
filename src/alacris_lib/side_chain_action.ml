open Legilogic_lib
open Hex
open Signing
open Types
open Action
open Lwt_exn
open Legilogic_ethereum
open Side_chain
open Side_chain_operator
open Side_chain_user

let contract_address_key = "alacris.contract-address"

exception Invalid_contract

(* TODO: Issue a warning if it wasn't confirmed yet? *)
let check_side_chain_contract_created contract_address =
  Ethereum_json_rpc.(eth_get_code (contract_address, BlockParameter.Latest))
  >>= fun code ->
  let code_red = remove_0x_from_string (Hex.unparse_0x_bytes code) in
  let contract_code_red = remove_0x_from_string (Hex.unparse_0x_bytes Operator_contract_binary.contract_bytes) in
  let len_red = String.length code_red in
  let contract_len_red = String.length contract_code_red in
  let contract_code_red_sub = String.sub contract_code_red (contract_len_red - len_red) len_red in
  if code_red = contract_code_red_sub then
    return (contract_address, Revision.zero) (* Clearly wrong. We need the revision on input *)
  else
    (let addr = Address.to_0x contract_address in
     Logging.log "Saved contract address %s invalid" addr;
     Printf.eprintf
       "Found contract address %s, but it doesn't contain the contract we expect:
        It contains code %s
        but we expected: %s
        Did you reset the state of the test ethereum network without resetting the
        state of the test side-chain? If so, kill the side_chain_server and the
        side_chain_client, and try again after resetting their state with `make clean`.\n"
       addr
       (Hex.unparse_0x_bytes code)
       (Hex.unparse_0x_bytes Operator_contract_binary.contract_bytes);
     fail Invalid_contract)




let print_and_retrieve_transaction_hash : Digest.t -> (Address.t * Revision.t) Lwt_exn.t =
  fun transaction_hash ->
  Operator_contract.retrieve_contract_address_quadruple transaction_hash
  >>= fun (contract_address, code_hash, creation_hash, creation_block) ->
  Logging.log "contract_address=%s" (Address.to_0x contract_address);
  Logging.log "code_hash=%s" (Digest.to_0x code_hash);
  Logging.log "creation_hash=%s" (Digest.to_0x creation_hash);
  Logging.log "creation_block=%s" (Revision.to_string creation_block);
  Logging.log "E N T R I E S to put in the side_chain_client_config.json file";
  Logging.log "  \"contract_address\": \"%s\",\n  \"code_hash\": \"%s\",\n  \"creation_hash\": \"%s\",\n  \"creation_block\": %s," (Address.to_0x contract_address) (Digest.to_0x code_hash) (Digest.to_0x creation_hash) (Revision.to_string creation_block);
  Address.to_0x contract_address
  |> of_lwt Lwter.(Db.put contract_address_key >>> Db.commit)
  >>= const (contract_address, creation_block)

let create_side_chain_contract (installer_address : Address.t) : (Address.t*Revision.t) Lwt_exn.t =
  (** TODO: persist this signed transaction before to send it to the network, to avoid double-send *)
  Ethereum_user.create_contract ~sender:installer_address ~code:Operator_contract_binary.contract_bytes ?gas_limit:None ~value:TokenAmount.zero
  >>= Ethereum_user.confirm_pre_transaction installer_address
  >>= fun (_tx, _, confirmation) ->
  print_and_retrieve_transaction_hash confirmation.transaction_hash


let ensure_side_chain_contract_created (installer_address : Address.t) : Address.t Lwt_exn.t =
  Logging.log "Ensuring the contract is installed...";
  (match Db.get contract_address_key with
   | Some addr ->
     addr |> catching_arr Address.of_0x >>= check_side_chain_contract_created
   | None ->
     Logging.log "Not found, creating the contract...";
     create_side_chain_contract installer_address)
  >>= fun (contract_address, contract_block_number) ->
  Operator_contract.set_contract_address contract_address;
  Operator_contract.set_contract_block_number contract_block_number;
  return contract_address

module Test = struct
  open Lib.Test
  open Signing.Test
  open Ethereum_user.Test
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
        of_lwt Db.open_connection "unit_test_db" >>= fun () ->
        Logging.log "deposit_and_payment_and_withdrawal, step 2";

        (* TODO replace mutable contract address plumbing w/ more elegant +
         * functional style *)
        get_contract_address_for_client_exn () >>= fun contract_address ->
        Logging.log "deposit_and_payment_and_withdrawal, step 3";
        Operator_contract.set_contract_address contract_address;
        let operator = trent_address in
        register_keypair "zander" Signing.Test.zander_keys;
        (* TODO consolidate integration tests into single entry point with
         * shared initialization phase rather than leaving them scattered about
         * the repo. One problem with the present setup is we cannot shut down
         * the following reactor once flipping it on, meaning we're likely to
         * encounter subtle time-dependent bugs in future tests (until we
         * reorganize) *)
        Logging.log "deposit_and_payment_and_withdrawal, step 4";
        fund_accounts ()
        >>= fun () ->
        Logging.log "deposit_and_payment_and_withdrawal, step 5";
        Mkb_json_rpc.init_mkb_server ()
(*        >>= fun () ->
        load_operator_state operator *)
        >>= fun _ ->
        start_operator_nocheck_test operator
        >>= fun () -> start_state_update_nocheck_periodic_operator operator
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

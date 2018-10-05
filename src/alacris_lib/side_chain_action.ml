open Legilogic_lib
open Lib
open Signing
open Action
open Lwt_exn
open Json_rpc

open Legilogic_ethereum
open Ethereum_chain

open Side_chain
open Side_chain_facilitator
open Side_chain_user

let contract_address_key = "alacris.contract-address"

exception Invalid_contract

(* TODO: Issue a warning if it wasn't confirmed yet? *)
let check_side_chain_contract_created contract_address =
  Ethereum_json_rpc.(eth_get_code (contract_address, BlockParameter.Latest))
  >>= fun code ->
  if code = Facilitator_contract_binary.contract_bytes then
    return contract_address
  else
    (let addr = Address.to_0x contract_address in
     Logging.log "Saved contract address %s invalid" addr;
     Printf.eprintf
       "Found contract address %s, but it doesn't contain the contract we expect.
        Did you reset the state of the test ethereum network without resetting the
        state of the test side-chain? If so, kill the side_chain_server and the
        side_chain_client, and try again after resetting their state with `make clean`.\n"
       addr;
     raise Invalid_contract)

let create_side_chain_contract installer_address =
  (** TODO: persist this signed transaction before to send it to the network, to avoid double-send *)
  Ethereum_user.make_signed_transaction
    installer_address
    (Operation.CreateContract Facilitator_contract_binary.contract_bytes)
    TokenAmount.zero
    (TokenAmount.of_int 1000000)
  >>= Ethereum_user.(user_action installer_address confirm_transaction)
  >>= fun (_tx, confirmation) ->
  Ethereum_json_rpc.eth_get_transaction_receipt confirmation.transaction_hash
  >>= function
  | None -> bork "No tx receipt for contract creation"
  | Some receipt ->
    let contract_address = receipt.contract_address |> Option.get in
    Address.to_0x contract_address
    |> of_lwt Lwter.(Db.put contract_address_key >>> Db.commit)
    >>= const contract_address

let ensure_side_chain_contract_created installer_address =
  Logging.log "Ensuring the contract is installed...";
  (match Db.get contract_address_key with
   | Some addr ->
     addr |> catching_arr Address.of_0x >>= check_side_chain_contract_created
   | None ->
     Logging.log "Not found, creating the contract...";
     create_side_chain_contract installer_address)
  >>= fun contract_address ->
  Facilitator_contract.set_contract_address contract_address;
  return contract_address

module Test = struct
  open Lib.Test
  open Signing.Test
  open Ethereum_transaction.Test
  open Side_chain_facilitator.Test

  let%test "move logs aside" = Logging.set_log_file "test.log"; true

  let get_user_balance address =
    (get_facilitator_state () |> (facilitator_account_lens address).get).balance
  let get_alice_balance () = get_user_balance alice_address
  let get_bob_balance () = get_user_balance bob_address

  (* deposit, payment and withdrawal test *)
  let%test "deposit_and_payment_and_withdrawal" =
    Signing.Test.register_test_keypairs ();
    Side_chain_client.Test.post_user_transaction_request_hook :=
      Side_chain_facilitator.post_user_transaction_request;
    try
      Lwt_exn.run
        (fun () ->
           of_lwt Db.open_connection "unit_test_db" >>= fun () ->
           get_prefunded_address () >>= fun prefunded_address ->
           ensure_side_chain_contract_created prefunded_address >>= fun contract_address ->
           Logging.log "Contract address: %s" (Address.to_0x contract_address); return ()
           >>= fund_accounts >>= fun () ->
           let facilitator = trent_address in
           start_facilitator facilitator >>= fun () ->
           let initial_alice_balance = get_alice_balance () in
           let initial_bob_balance = get_bob_balance () in

           (* 1- Test deposit *)
           let deposit_amount = TokenAmount.of_string "500000000000000000" in
           User.transaction alice_address deposit
             DepositWanted.{facilitator; deposit_amount}
           >>= fun (_commitment, _confirmation) ->
           let alice_balance_after_deposit = get_alice_balance () in
           expect_equal "Alice balance after deposit" TokenAmount.to_string
             alice_balance_after_deposit
             (TokenAmount.add initial_alice_balance deposit_amount);

           (* 2- Test payment *)
           let payment_amount = TokenAmount.of_string "170000000000000000" in
           User.transaction alice_address payment
             PaymentWanted.{facilitator ; recipient= bob_address ; amount= payment_amount
                           ; memo="test" ; payment_expedited= false}
           >>= fun (_commitment2, _confirmation2) ->
           let bob_balance_after_payment = get_bob_balance () in
           expect_equal "Bob balance after payment" TokenAmount.to_string
             bob_balance_after_payment
             (TokenAmount.add initial_bob_balance payment_amount) ;
           get_facilitator_fee_schedule trent_address
           >>= fun fee_schedule ->
           let payment_fee = payment_fee_for fee_schedule payment_amount in
           let alice_balance_after_payment = get_alice_balance () in
           expect_equal "Alice balance after payment" TokenAmount.to_string
             alice_balance_after_payment
             TokenAmount.(sub alice_balance_after_deposit (add payment_amount payment_fee));

           (* 3- Test Withdrawal -- withdraw all that was deposited *)
           let withdrawal_amount = TokenAmount.sub payment_amount fee_schedule.withdrawal_fee in
           User.transaction bob_address withdrawal
             WithdrawalWanted.{facilitator; withdrawal_amount}
           >>= fun (_commitment, _confirmation) ->
           let bob_balance_after_withdrawal = get_bob_balance () in
           expect_equal "Bob balance after withdrawal" TokenAmount.to_string
             bob_balance_after_withdrawal
             initial_bob_balance;
           (* TODO: check main-chain balance, too! *)

           return true)
        ()
    with e ->
      Logging.log "Error: %s" (e |> exn_to_yojson |> Yojsoning.string_of_yojson);
      false
end

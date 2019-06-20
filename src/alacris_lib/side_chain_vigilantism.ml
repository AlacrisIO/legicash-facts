open Legilogic_lib
open Action
open Types
open Signing


open Legilogic_ethereum
open Side_chain_server_config
open Ethereum_json_rpc
open Ethereum_abi
open Ethereum_watch
open Operator_contract
open Side_chain
open Side_chain_operator
open Side_chain_user



let treat_individual_claim_bad_ticket : (LogObject.t * abi_value list) -> unit Lwt_exn.t =
  fun (_x_log, x_abi_list) ->
  let open Lwt_exn in
  let operator = retrieve_address_from_abi_value (List.nth x_abi_list 0) in
  let claimant = retrieve_address_from_abi_value (List.nth x_abi_list 1) in
  let operator_revision = retrieve_revision_from_abi_value (List.nth x_abi_list 2) in
  let value = retrieve_tokenamount_from_abi_value (List.nth x_abi_list 3) in
  let bond = retrieve_tokenamount_from_abi_value (List.nth x_abi_list 4) in
  let confirmed_state = retrieve_digest_from_abi_value (List.nth x_abi_list 5) in
  let confirmed_revision = retrieve_revision_from_abi_value (List.nth x_abi_list 6) in
  let confirmed_pair : PairRevisionDigest.t = (confirmed_revision, confirmed_state) in
  let contract_address = get_contract_address () in
  if (Revision.compare operator_revision confirmed_revision) > 0 then
    (let operation = make_challenge_withdrawal_too_large_revision ~contract_address ~claimant ~operator ~operator_revision ~value ~bond ~confirmed_pair in
     Ethereum_user.post_operation ~operation ~sender:operator ~value_send:TokenAmount.zero
     >>= fun _ -> return ()
    )
  else
    return ()

let treat_individual_claim : (LogObject.t * abi_value list) -> unit Lwt_exn.t =
  fun x ->
  treat_individual_claim_bad_ticket x


let treat_sequence_claims : (LogObject.t * abi_value list) list -> unit Lwt_exn.t =
  fun x_list ->
  let open Lwt_exn in
  let len : int = List.length x_list in
  Logging.log "treat_sequence_claims with len=%i" len;
  let rec treat_single : int -> unit Lwt_exn.t =
    fun pos_in ->
    treat_individual_claim (List.nth x_list pos_in)
    >>= fun () ->
    let pos_out = pos_in + 1 in
    if pos_out == len then
      return ()
    else
      treat_single pos_out
  in treat_single 0


let search_fraud : contract_address:Address.t -> operator:Address.t -> Revision.t -> Revision.t Lwt_exn.t =
  fun ~contract_address ~operator rev_in ->
  let open Lwt_exn in
  retrieve_relevant_list_logs_data
    ~delay:Side_chain_server_config.delay_wait_ethereum_watch_in_seconds
    ~start_revision:rev_in
    ~max_number_iteration:None
    ~contract_address
    ~transaction_hash:None
    ~topics:[topic_of_claim_withdrawal]
    [Address; Address; Uint 64; Uint 256; Uint 256; Bytes 32; Uint 64]
    [Some (Address_value operator); None; None; None; None; None; None]
  >>= fun (rev_out, llogs) ->
  Logging.log "Before call to treat_sequence_claims";
  treat_sequence_claims llogs
  >>= fun () -> return rev_out



let rec search_fraud_iter_if_failing : contract_address:Address.t -> operator:Address.t -> Revision.t -> Revision.t Lwt.t =
  fun ~contract_address ~operator rev_in ->
  let open Lwt in
  search_fraud ~contract_address ~operator rev_in
  >>= function
  | Ok rev_out ->
     Logging.log "search_fraud_iter_if_failing, success returning rev_out=%s" (Revision.to_string rev_out);
     Lwt.return rev_out
  | _ ->
     Logging.log "Reiterating the search_fraud_iter_if_failing";
     search_fraud_iter_if_failing ~contract_address ~operator rev_in



let inner_vigilant_thread () =
  let open Lwt in
  let contract_address = get_contract_address () in
  let operator = Signing.Test.trent_address in
  let rec do_search : Revision.t -> Revision.t Lwt.t =
    fun start_ref ->
    Logging.log "Begin of do_search in inner_vigilant_thread";
    search_fraud_iter_if_failing ~contract_address ~operator start_ref
    >>= fun return_ref ->
    Lwt_unix.sleep Side_chain_server_config.delay_wait_ethereum_watch_in_seconds
    >>= fun () -> do_search return_ref
  in do_search Revision.zero



let start_vigilantism_state_update_operator () =
  Logging.log "Beginning of the inner_vigilant_thread";
  Lwt.async inner_vigilant_thread;
  Lwt_exn.return ()


let get_keypair_of_address_noexn : Address.t -> keypair =
  fun address ->
  match get_keypair_of_address address with
  | Ok x -> x
  | Error _ -> Lib.bork "Failed to find address"

module Test = struct
  open Signing.Test
  open Ethereum_user.Test

  let%test "move logs aside" = Logging.set_log_file "test.log"; true



  (* deposit, payment and withdrawal test *)
  let%test "deposit_withdraw_wrong_operator_version" =
    Signing.Test.register_test_keypairs ();
    Side_chain_client.Test.post_user_transaction_request_hook :=
      Side_chain_operator.oper_post_user_transaction_request;
    let open Lwt_exn in
    Lwt_exn.run
      (fun () ->
        Logging.log "deposit_withdraw_wrong_operator_version, step 1";
        let operator = trent_address in
        of_lwt Db.open_connection "unit_test_db"
        >>= fun () ->
        Logging.log "deposit_withdraw_wrong_operator_version, step 2";
        get_contract_address_for_client_exn ()
        >>= fun contract_address ->
        Logging.log "deposit_withdraw_wrong_operator_version, step 3";
        Operator_contract.set_contract_address contract_address;
        State_update.start_state_update_operator ()
        >>= fun _ ->
        Logging.log "deposit_withdraw_wrong_operator_version, step 4";
        fund_accounts ()
        >>= fun () ->
        Logging.log "deposit_withdraw_wrong_operator_version, step 5";
        Mkb_json_rpc.init_mkb_server ()
        >>= fun () ->
        Logging.log "deposit_withdraw_wrong_operator_version, step 6";
        start_operator operator
        >>= fun () ->
        Logging.log "deposit_withdraw_wrong_operator_version, step 7";
        start_vigilantism_state_update_operator ()
        >>= fun () ->
        Logging.log "deposit_withdraw_wrong_operator_version, step 8";
        start_state_update_nocheck_periodic_operator ()
        >>= fun () ->
	Logging.log "deposit_withdraw_wrong_operator_version, step 9";
        let deposit_amount = TokenAmount.of_string "500000000000000000" in
        User.transaction
          alice_address
          deposit
          DepositWanted.{ operator
                        ; deposit_amount
                        ; request_guid = Types.RequestGuid.nil
                        ; requested_at = Types.Timestamp.now () }
        >>= fun (commitment, _confirmation) ->
	Logging.log "deposit_withdraw_wrong_operator_version, step 10";
        Logging.log "Making the fake transaction that should be rejected";
        let withdrawal_amount = TokenAmount.of_string "100000000000000000" in
        let withdrawal_fee = TokenAmount.of_string "100000000000000" in
        let pre_oper : UserOperation.withdrawal_details = {withdrawal_amount
                        ; withdrawal_fee
                        ; request_guid = Types.RequestGuid.nil
                        ; requested_at = Types.Timestamp.now () } in
        let withdraw : UserOperation.t = Withdrawal pre_oper in
        let rx_header = RxHeader.{
              operator=Address.zero
              ; requester= Address.zero
              ; requester_revision=Revision.zero
              ; confirmed_main_chain_state_digest=Digest.zero
              ; confirmed_main_chain_state_revision=Revision.zero
              ; confirmed_side_chain_state_digest=Digest.zero
              ; confirmed_side_chain_state_revision=Revision.zero
              ; validity_within=Duration.zero } in
        let keypair = get_keypair_of_address_noexn alice_address in
        let user_trans_req : UserTransactionRequest.t = {rx_header; operation=withdraw} in
        let user_trans_req_sign : UserTransactionRequest.t signed = SignedUserTransactionRequest.make keypair user_trans_req in
        let tx_request : TransactionRequest.t = `UserTransaction user_trans_req_sign in
        let tx_revision = Revision.zero in
        let updated_limit = TokenAmount.zero in
        let tx_header : TxHeader.t = TxHeader.{tx_revision; updated_limit} in
        let transaction : Transaction.t = {tx_header; tx_request} in
        let state_digest = Digest.zero in
        let signature = commitment.signature in
        let key = Revision.of_int 10 in
        let trie = Digest.zero in
        let leaf = Digest.zero in
        let steps = [] in
        let spending_limit = TokenAmount.zero in
        let accounts = Digest.zero in
        let main_chain_transactions_posted = Digest.zero in
        let operator_revision = Revision.zero in
        let tc : TransactionCommitment.t =
          TransactionCommitment.{ transaction; tx_proof={key; trie; leaf; steps};
                                  operator_revision; spending_limit;
                                  accounts; main_chain_transactions_posted; signature;
                                  state_digest } in
        let confirmed_pair = (Revision.zero, Digest.zero) in
        Logging.log "deposit_withdraw_wrong_operator_version, step 10 address=%s" (Address.to_0x operator);
        Logging.log "deposit_withdraw_wrong_operator_version, step 10 alice_address=%s" (Address.to_0x alice_address);
        post_claim_withdrawal_operation_exn ~confirmed_pair tc ~sender:alice_address ~operator
        >>= fun block_nbr ->
	Logging.log "deposit_withdraw_wrong_operator_version, step 11";
        let addi_term = (Revision.add Side_chain_server_config.challenge_period_in_blocks (Revision.of_int 2)) in
        let min_block_length =  (Revision.add block_nbr addi_term) in
        wait_for_min_block_depth min_block_length
        >>= fun () ->
	Logging.log "deposit_withdraw_wrong_operator_version, step 12";
        get_claim_withdrawal_status ~confirmed_pair tc ~claimant:alice_address ~sender:alice_address ~operator
        >>= fun ret_value ->
	Logging.log "deposit_withdraw_wrong_operator_version, step 13 ret_value=%s" (Revision.to_string ret_value);
        if (Revision.equal ret_value (Revision.of_int 1)) then
          return true
        else
          return false)
      ()
end

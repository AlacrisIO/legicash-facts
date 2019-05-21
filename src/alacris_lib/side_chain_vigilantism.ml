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
open State_update



let treat_individual_claim_bad_ticket : (LogObject.t * abi_value list) -> unit Lwt_exn.t =
  fun (_x_log, x_abi_list) ->
  let open Lwt_exn in
  let operation_revision = retrieve_revision_from_abi_value (List.nth x_abi_list 1) in
  let confirmed_revision = retrieve_revision_from_abi_value (List.nth x_abi_list 4) in
  if (Revision.compare operation_revision confirmed_revision) > 0 then
    (let operator = retrieve_address_from_abi_value (List.nth x_abi_list 0) in
     let value = retrieve_tokenamount_from_abi_value (List.nth x_abi_list 2) in
     let confirmed_state = retrieve_digest_from_abi_value (List.nth x_abi_list 3) in
     let bond = retrieve_tokenamount_from_abi_value (List.nth x_abi_list 5) in
     let confirmed_pair : PairRevisionDigest.t = (confirmed_revision, confirmed_state) in
     let contract_address = get_contract_address () in
     let oper = make_challenge_withdrawal_too_large_revision ~contract_address ~operator operation_revision ~value ~bond ~confirmed_pair in
     let (value_send : TokenAmount.t) = TokenAmount.zero in
     post_operation_general oper operator value_send
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
    ~contract_address
    ~transaction_hash:None
    ~topics:[topic_of_claim_withdrawal]
    [Address; Uint 64; Uint 256; Bytes 32; Uint 64; Uint 256; Uint 256; Uint 64]
    [Some (Address_value operator); None; None; None; None; None; None; None]
  >>= fun (rev_out, llogs) ->
  treat_sequence_claims llogs
  >>= fun () -> return rev_out



let rec search_fraud_iter_if_failing : contract_address:Address.t -> operator:Address.t -> Revision.t -> Revision.t Lwt.t =
  fun ~contract_address ~operator rev_in ->
  let open Lwt in
  search_fraud ~contract_address ~operator rev_in
  >>= function
  | Ok rev_out -> Lwt.return rev_out
  | _ -> search_fraud_iter_if_failing ~contract_address ~operator rev_in



let inner_vigilant_thread () =
  let open Lwt in
  let contract_address = get_contract_address () in
  let operator = Signing.Test.trent_address in
  let rec do_search : Revision.t -> Revision.t Lwt.t =
    fun start_ref ->
    search_fraud_iter_if_failing ~contract_address ~operator start_ref
    >>= fun return_ref ->
    Lwt_unix.sleep Side_chain_server_config.delay_wait_ethereum_watch_in_seconds
    >>= fun () -> do_search return_ref
  in do_search Revision.zero



let start_vigilantism_state_update_operator () =
  Logging.log "Beginning of start_state_update_operator";
  Lwt.async inner_vigilant_thread;
  Lwt_exn.return ()

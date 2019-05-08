
let treat_individual_claim : (LogObject.t * abi_value list) -> unit Lwt_exn.t =
  fun (x_log, x_abi_list) ->
  

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


let search_fraud : Address.t -> Revision.t -> Revision.t Lwt_exn.t =
  fun contract_address rev_in ->
  retrieve_relevant_list_logs_data
    ~delay:Side_chain_server_config.delay_wait_ethereum_watch_in_seconds
    ~start_revision:rev_in
    ~contract_address
    ~transaction_hash:None
    ~topics:[topic_of_claim_withdrawal]
    [Address; Uint 64; Uint 256; Bytes 32; Uint 256; Uint 256; Uint 64]
    [Some (Address_value operator); None; None; None; None; None; None]
  >>= fun (rev_out, llogs) ->
  treat_sequence_claims llogs
  >>= return rev_out



let search_fraud_iter_if_failing : Revision.t -> Revision.t Lwt.t =
  fun rev_in ->
  let open Lwt in
  search_fraud rev_in
  >>= function
  | Ok rev_out -> Lwt.return rev_out
  | _ -> search_iter_if_failing rev_in



let inner_vigilant_thread () =
  let open Lwt in
  let start_ref : Revision.t = ref Revision.zero in
  let rec do_search : unit -> unit Lwt.t =
    fun () ->
    search_fraud_iter_if_failing !start_ref
    >>= fun return_ref ->
    start_ref := return_ref;
    Lwt_unix.sleep Side_chain_server_config.delay_wait_ethereum_watch_in_seconds
    >>= fun () -> do_search ()
  in do_search ()



let start_vigilantism_state_update_operator () =
  Logging.log "Beginning of start_state_update_operator";
  Lwt.async inner_vigilant_thread;
  Lwt_exn.return ()

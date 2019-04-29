
let check_individual_claim : (LogObject.t * (abi_value list)) -> unit Lwt_exn.t =
  fun _addr ->
  Lwt_exn.return ()


let search_for_claim_withdrawal : operator:Address.t -> unit Lwt_exn.t =
  let open Lwt_exn in
  let contract_address = get_contract_address () in
  let topics = [topic_of_claim_withdrawal] in
  let (list_data_type : abi_type list) = [Address; Uint 64; Uint 256; Bytes 32; Uint 256; Uint 256; Uint 64] in
  let (data_value_search : abi_value option list) = [Some (Address_value operator);
                                                     None; None; None; None; None; None] in
  let rec fct_search start_block =
    retrieve_last_entries (Revision.add start_block Revision.one)
      ~contract_address  ~topics
    >>= fun (end_block, entries) ->
    let only_matches_record = flip List.filter entries @@ fun l ->
       is_matching_data (decode_data l.data list_data_type)
         data_value_search in
    let final_matches = flip List.map only_matches_record @@ fun l ->
                                                             (l, decode_data l.data list_data_type) in
    let _list_return = List.map check_individual_claim final_matches in
    return ()
    >>= fun () ->
    sleep_delay_exn (Float.from_int 1)
    >>= fun () -> fct_search end_block in
  fct_search Revision.zero

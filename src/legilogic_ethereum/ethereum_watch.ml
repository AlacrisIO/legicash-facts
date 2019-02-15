open Legilogic_lib
open Lib
open Types
open Action
open Signing
open Integer
open Ethereum_json_rpc
open Ethereum_abi

let starting_watch_ref : (Revision.t ref) = ref Revision.zero
   
(* 'state is Revision.t *)
let stream_of_poller : delay:float -> (unit, 'value, 'state) async_exn_action -> 'state ->
  'value AsyncStream.t Lwt.t =
  let open Lwter in
  fun ~delay poller state ->
    let nap () = Lwt_unix.sleep delay in
    let rec continue state () =
      poller () state
      >>= function
      | Ok value, new_state ->
        Lwt.return @@ AsyncStream.cons value
                        (nap () >>= continue new_state |> join)
      | Error _, new_state ->
        nap () >>= continue new_state
    in
    continue state ()

let main_chain_block_notification_stream
      ?(delay=30.0) ?(start_block=Revision.zero)
      ?(get_block=(Ethereum_json_rpc.eth_block_number)) () =
  let rec poller () next_block =
    let open Lwt in
    get_block ()
    >>= function
    | Ok block_number ->
      (* Is this block at least as big as next_block? *)
      if Revision.compare block_number next_block >= 0 then
        (* This is a previously unobserved block at or past the next_block,
           so send a notification about it. The happy path. *)
        Lwt.return (Ok block_number, Revision.(add one block_number))
      else
        Lwt.return (Error (Internal_error "Start block not reached yet"), next_block)
    | Error e -> Lwt.return (Error e, next_block) in
  stream_of_poller ~delay poller start_block



(* Reverse operation: Turning a Lwt.t into a Lwt_exn.t *)
let sleep_delay_exn : float -> unit Lwt_exn.t = Lwt_exn.of_lwt Lwt_unix.sleep

(* Look for confirmed or not confirmed blocks. NEED TO ADD: NUMBER of confirmation *)
let retrieve_last_entries (start_block : Revision.t) (contract_address : Address.t) (topics : Bytes.t option list) : (Revision.t * (LogObject.t list)) Lwt_exn.t =
  Lwt_exn.bind (eth_block_number ())
    (fun (to_block : Revision.t) ->
      let (eth_object : EthObject.t) = {
          from_block=(Some (Block_number start_block));
          to_block=(Some (Block_number to_block));
          address =(Some contract_address);
          topics=(Some topics);
          blockhash=None} in
      Logging.log "retrieve_last_entries. Before call to eth_get_logs";
      Lwt_exn.bind (eth_get_logs eth_object)
        (fun (recLLO : EthListLogObjects.t) ->
          Logging.log "retrieve_last_entries, After call to eth_get_logs";
          Lwt_exn.return (to_block,recLLO)))





  

let retrieve_relevant_list_logs
      (delay : float) (contract_address : Address.t) (topics : Bytes.t option list) : LogObject.t list Lwt_exn.t =
  (*  starting_watch_ref := (Revision.of_int 0); *)
  let rec fct_downloading (start_block : Revision.t) : LogObject.t list Lwt_exn.t =
    let (start_block_p_one : Revision.t) = (Revision.add start_block Revision.one) in 
    Lwt_exn.bind (retrieve_last_entries start_block_p_one contract_address topics)
      (fun (x : (Revision.t * (LogObject.t list))) ->
        let (x_to, x_llogs) = x in
        let (len : int) = List.length x_llogs in
        starting_watch_ref := x_to;
        if (len == 0) then
          Lwt_exn.bind (sleep_delay_exn delay) (fun () -> fct_downloading x_to)
        else
          Lwt_exn.return x_llogs
      )
  in fct_downloading !starting_watch_ref


   
let is_matching_data (x_data : abi_value list) (x_data_filter: abi_value option list) : bool =
  let len1 = List.length x_data in
  let len2 = List.length x_data_filter in
  if (len1 != len2) then
    bork "We should have len1 = len2";
  let is_ok_ent (x: abi_value) (x_filter: abi_value option) : bool =
    match (x_filter) with
    | None -> true
    | Some x_filt_val -> (equal x_filt_val x) in
  let list_bool = List.init len1 (fun i ->
    let x_filter = List.nth x_data_filter i in
    let x = List.nth x_data i in
    let (res : bool) = is_ok_ent x x_filter in
    Logging.log "res=%B" res;
    res) in
  let test = List.exists (fun eval -> eval == false) list_bool in
  Logging.log "is_matching_data test=%B" test;
  if test then
    false
  else
    true
    


let retrieve_relevant_list_logs_data
      (delay : float) (contract_address : Address.t) (topics : Bytes.t option list) (list_data_type : abi_type list) (data_value_search : abi_value option list)  : (LogObject.t * (abi_value list)) list Lwt_exn.t =
  (*  starting_watch_ref := (Revision.of_int 0); *)
  let rec fct_downloading (start_block : Revision.t) : (LogObject.t * (abi_value list)) list Lwt_exn.t =
    let (start_block_p_one : Revision.t) = (Revision.add start_block Revision.one) in
    Logging.log "Starting of fct_downloading";
    Lwt_exn.bind (retrieve_last_entries start_block_p_one contract_address topics)
      (fun (x : (Revision.t * (LogObject.t list))) ->
        let (x_to, x_llogs) = x in
        starting_watch_ref := x_to;
        Logging.log "retrieve_relevant_list_logs_data, step 1";
        let x_llogs_filt = List.filter (fun (e_log : LogObject.t) ->
                               Logging.log "retrieve_relevant_list_logs_data, step 1.1";
                               let list_value = decode_data e_log.data list_data_type in
                               Logging.log "retrieve_relevant_list_logs_data, step 1.2";
                               is_matching_data list_value data_value_search) x_llogs in
        Logging.log "retrieve_relevant_list_logs_data, step 2";
        let x_llogs_ret = List.map (fun (e_log : LogObject.t) ->
                              let list_value = decode_data e_log.data list_data_type in
                              (e_log, list_value)) x_llogs_filt in
        Logging.log "retrieve_relevant_list_logs_data, step 3";
        let (len : int) = List.length x_llogs_ret in
        if (len == 0) then
          Lwt_exn.bind (sleep_delay_exn delay) (fun () -> fct_downloading x_to)
        else
          Lwt_exn.return x_llogs_ret
      )
  in fct_downloading !starting_watch_ref

let retrieve_relevant_single_logs_data
      (delay : float) (contract_address : Address.t) (topics : Bytes.t option list) (list_data_type : abi_type list) (data_value_search : abi_value option list) : (LogObject.t * (abi_value list)) Lwt_exn.t =
  Lwt_exn.bind (retrieve_relevant_list_logs_data delay contract_address topics list_data_type data_value_search)
    (fun (llogs : (LogObject.t * (abi_value list)) list) ->
      let (len : int) = List.length llogs in
      if len > 1 then
        bork "The length should be exactly 1"
      else
	Lwt_exn.return (List.hd llogs))

   

let retrieve_relevant_single_logs
      (delay : float) (contract_address : Address.t) (topics : Bytes.t option list) : LogObject.t Lwt_exn.t =
  Lwt_exn.bind (retrieve_relevant_list_logs delay contract_address topics)
    (fun (llogs : LogObject.t list) ->
      let (len : int) = List.length llogs in
      if len > 1 then
        bork "The length should be exactly 1"
      else
        Lwt_exn.return (List.hd llogs))
       
      

  

(* GROUP cases *)

  
(* The code below is objectively a hack. It is introduced since array data in LogObject is hard to understand *)
let retrieve_last_entries_group (start_block : Revision.t) (contract_address : Address.t) (list_topics : Bytes.t option list list) : (Revision.t * (LogObject.t list list)) Lwt_exn.t =
  let fct_single (to_block : Revision.t) (x_topic : Bytes.t option list) : EthListLogObjects.t Lwt_exn.t =
    let (eth_object : EthObject.t) = {
        from_block=(Some (Block_number start_block));
        to_block=(Some (Block_number to_block));
        address =(Some contract_address);
        topics=(Some x_topic);
        blockhash=None} in
    Logging.log "retrieve_last_entries_group. Before call to eth_get_logs";
    eth_get_logs eth_object in
  let fct (to_block : Revision.t) : EthListLogObjects.t list Lwt_exn.t =
    Lwt_exn.list_map_s (fun (x_topic : Bytes.t option list) ->
        fct_single to_block x_topic) list_topics in
  Lwt_exn.bind (eth_block_number ())
    (fun (to_block : Revision.t) ->
      Lwt_exn.bind (fct to_block)
        (fun (list_recLLO : EthListLogObjects.t list) ->
          Logging.log "retrieve_last_entries_group. After call to eth_get_logs";
          Lwt_exn.return (to_block,list_recLLO)))
  
  
let retrieve_relevant_list_logs_group (delay : float) (contract_address : Address.t) (list_topics : Bytes.t option list list) : EthListLogObjects.t list Lwt_exn.t =
  let rec fct_downloading (start_block : Revision.t) : EthListLogObjects.t list Lwt_exn.t =
    let (start_block_p_one : Revision.t) = (Revision.add start_block Revision.one) in 
    Lwt_exn.bind (retrieve_last_entries_group start_block_p_one contract_address list_topics)
      (fun (x : (Revision.t * (EthListLogObjects.t list))) ->
        let (x_to, x_llogs_group) = x in
        let (list_len : int list) = List.map (fun x -> List.length x) x_llogs_group in
        let (sum_term : int) = sum_int_list list_len in 
        starting_watch_ref := x_to;
        if (sum_term == 0) then
          Lwt_exn.bind (sleep_delay_exn delay) (fun () -> fct_downloading x_to)
        else
          Lwt_exn.return x_llogs_group
      )
  in fct_downloading !starting_watch_ref


   

   

   
(* TODO: implement following operations:
   ---Watch Ethereum blocks on the ethereum blockchain.
   ---Distinguish between confirmed blocks and not quite confirmed blocks.
   There is no notion of confirmed ethereum block.
   A block is considered "confirmed" after 120 blocks.

   We need a configuration file on input.
*)


module Test = struct
(*
     let%test "exercise main_chain_block_notification_stream" =
     let open Revision in
     let open Lwt_exn in
     let current_block = ref zero in (* Mock for current mainchain block num *)
     let throw_error = ref None in (* Whether to throw when getting block *)
     let get_block ?timeout ?log () =
     ignore timeout ; ignore log ;
     match !throw_error with
     | None -> let cb = !current_block in current_block := succ cb; return cb
     | Some e -> throw_error := None; fail e in
     let start_block = of_int 10 in
     Lwt_exn.run
     (of_lwt (main_chain_block_notification_stream ~start_block ~get_block)
     >>> catching_lwt (AsyncStream.split 2)
     >>> (fun (l, s) ->
     assert(l = [start_block; add one start_block]);
     catching_lwt (AsyncStream.split 1) s)
     >>> (fun (l, _s) ->
     assert(l = [add start_block (of_int 2)]);
     return true
     (* Deals gracefully with errors? *)
     (*         throw_error := Some (Internal_error "You FAIL!!!"); *)
     (*         trying (catching_lwt (AsyncStream.split 1)) s)
     >>> (function
     | Error (Internal_error "You FAIL!!!") -> return true
     | Error e -> raise e
     | Ok (l, _) -> raise (Internal_error "blah %s, _" (string_of_yojson (`List (List.map Revision.to_string l))))) *)
     ))
     ()
  *)
end

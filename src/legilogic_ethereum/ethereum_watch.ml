open Legilogic_lib
open Lib
open Types
open Action
open Signing
open Integer
open Ethereum_chain
open Ethereum_json_rpc
open Ethereum_abi
open Side_chain_server_config

(* TODO capturing `starting_watch_ref` in a state monad or similar would be a
 * much better approach than using mutable global state *)
(* let starting_watch_ref : (Revision.t ref) = ref Revision.zero *)

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
let retrieve_last_entries (start_block:      Revision.t)
                          (contract_address: Address.t)
                          (topics:           Bytes.t option list)
                        : (Revision.t * (LogObject.t list)) Lwt_exn.t =
  let open Lwt_exn in
  eth_get_balance (contract_address, BlockParameter.Pending)
  >>= fun x ->
  Logging.log "retrieve_last_entries contract address balance=%s" (TokenAmount.to_string x);
  Lwt_exn.return ()
  >>= fun () ->
  eth_block_number ()
  >>= fun (to_block: Revision.t) ->
  Logging.log "retrieve_last_entries. Before call to eth_get_logs";
  eth_get_logs { from_block = Some (Block_number start_block)
               ; to_block   = Some (Block_number to_block)
               ; address    = Some contract_address
               ; topics     = Some topics
               ; blockhash  = None
    }
  >>= fun (recLLO : EthListLogObjects.t) ->
  Logging.log "retrieve_last_entries, After call to eth_get_logs";
  return (to_block,recLLO)


let is_matching_data (x_data:        abi_value list)
                     (x_data_filter: abi_value option list)
                   : bool =
  let len1 = List.length x_data
  and len2 = List.length x_data_filter

  in
  if (len1 != len2) then
       false
  else
    (let is_ok_ent (x: abi_value) (x_filter: abi_value option) : bool =
       match x_filter with | None            -> true
                           | Some x_filt_val -> equal x_filt_val x
     in not @@ List.exists ((==) false) (List.init len1 @@ fun i ->
        is_ok_ent (List.nth x_data        i)
          (List.nth x_data_filter i))
    )

let string_of_option_digest : Digest.t option -> string =
  fun edig ->
  match edig with
  | None -> "0xOPTIONAL"
  | Some x -> Digest.to_0x x


let print_list_entries : EthListLogObjects.t -> string =
  fun entries ->
  let entries_b : LogObject.t list = entries in 
  let list_str : string list = List.map (fun (x : LogObject.t) -> string_of_option_digest (x.transactionHash)) entries_b in
  let estri = "\n" in
  String.concat estri list_str



            
let retrieve_relevant_list_logs_data (delay:             float)
                                     (contract_address:  Address.t)
                                     (trans_hash: Digest.t option)
                                     (topics:            Bytes.t option list)
                                     (list_data_type:    abi_type list)
                                     (data_value_search: abi_value option list)
                                   : (LogObject.t * (abi_value list)) list Lwt_exn.t =
  let open Lwt_exn in
  Logging.log "|list_data_type|=%d" (List.length list_data_type);
  Logging.log "|data_value_search|=%d" (List.length data_value_search);
  let starting_watch_ref : (Revision.t ref) = ref Revision.zero in
  let iter_state_ref : (int ref) = ref 0 in
  let rec fct_downloading start_block iter_state =
    retrieve_last_entries (Revision.add start_block Revision.one)
                          contract_address
                          topics

    >>= fun (start_block_in, entries) ->
        Logging.log "retrieve_relevant trans_hash=%s" (string_of_option_digest trans_hash);
        Logging.log "List transaction_hashe=%s" (print_list_entries entries);
        let only_matches_a = flip List.filter entries @@ fun l ->
          is_matching_data (decode_data l.data list_data_type)
                           data_value_search
        in let only_matches_b = List.filter (fun (l : LogObject.t) ->
                                 match trans_hash with
                                 | None -> true
                                 | Some trans_hash_a ->
                                    (match l.transactionHash with
                                     | None -> true
                                     | Some trans_hash_b -> Digest.equal trans_hash_a trans_hash_b))
                               only_matches_a
        in let relevant = flip List.map only_matches_b @@ fun l ->
          (l, decode_data l.data list_data_type)

        in if List.length relevant == 0 then
             sleep_delay_exn delay >>= fun () ->
             if iter_state == 5 then
               (starting_watch_ref := Revision.zero;
                iter_state_ref := 0;
                fct_downloading !starting_watch_ref !iter_state_ref
               )
             else
               (starting_watch_ref := start_block_in;
                iter_state_ref := iter_state + 1;
                fct_downloading start_block_in !iter_state_ref
               )
        else
          (Logging.log "|only_matches_a|=%d   |only_matches_b|=%d   |relevant|=%d" (List.length only_matches_a) (List.length only_matches_b)  (List.length relevant);
           return relevant)

  in fct_downloading !starting_watch_ref !iter_state_ref


let retrieve_relevant_single_logs_data (delay:             float)
                                       (contract_address:  Address.t)
                                       (trans_hash: Digest.t option)
                                       (topics:            Bytes.t option list)
                                       (list_data_type:    abi_type list)
                                       (data_value_search: abi_value option list)
                                     : (LogObject.t * (abi_value list)) Lwt_exn.t =
  let open Lwt_exn in

  retrieve_relevant_list_logs_data
    delay
    contract_address
    trans_hash
    topics
    list_data_type
    data_value_search

  >>= fun (llogs : (LogObject.t * (abi_value list)) list) ->
  let len = List.length llogs in
  if len > 1 then
    bork "The length should be exactly 1"
  else
    return (List.hd llogs)




(* GROUP cases *)


(* The code below is objectively a hack. It is introduced since array data in
 * LogObject is hard to understand *)
let retrieve_last_entries_group (start_block:      Revision.t)
                                (contract_address: Address.t)
                                (list_topics:      Bytes.t option list list)
                              : (Revision.t * (LogObject.t list list)) Lwt_exn.t =
  let open Lwt_exn in

  let f (to_block: Revision.t) : EthListLogObjects.t list Lwt_exn.t =
    flip list_map_s list_topics @@ fun (x_topic: Bytes.t option list) ->
      eth_get_logs { from_block = Some (Block_number start_block)
                   ; to_block   = Some (Block_number to_block)
                   ; address    = Some contract_address
                   ; topics     = Some x_topic
                   ; blockhash  = None
                   }

  in eth_block_number ()
    >>= fun to_block -> f to_block
    >>= fun entries  -> return (to_block, entries)


let retrieve_relevant_list_logs_group (delay : float) (contract_address : Address.t) (list_topics : Bytes.t option list list) : EthListLogObjects.t list Lwt_exn.t =
  let starting_watch_ref : (Revision.t ref) = ref Revision.zero in
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


let wait_for_contract_event (contract_address:  Address.t)
      (trans_hash:        Digest.t option)
      (topics:            Bytes.t option list)
      (list_data_type:    abi_type list)
      (data_value_search: abi_value option list)
      
    : (LogObject.t * (abi_value list)) Lwt_exn.t =
  Logging.log "Beginning of wait_for_contract_event";
  retrieve_relevant_single_logs_data
    Side_chain_server_config.delay_wait_ethereum_watch_in_seconds
    contract_address
    trans_hash
    topics
    list_data_type
    data_value_search




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

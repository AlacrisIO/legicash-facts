open Legilogic_lib
open Lib
open Types
open Action
open Signing
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

(* Look for some event logs from a starting point from a specific contract address.
   In return the list of logs matched and the latest block number that was searched.
   This allows to build iterating systems *)
let retrieve_last_entries : Revision.t -> contract_address:Address.t -> topics:Bytes.t option list -> (Revision.t * (LogObject.t list)) Lwt_exn.t =
  fun start_block ~contract_address ~topics ->
  let open Lwt_exn in
  eth_block_number ()
  >>= fun current_block ->
  let block_depth_for_receipt = Side_chain_server_config.minNbBlockConfirm in
  let to_block = Revision.sub current_block block_depth_for_receipt in
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



(* Given a list of abi values from an event and a filtering system
   check if the event log matches this. If the length of the filter
   is different from the length of the values, then returns false
   since it cannot be matching *)
let is_matching_data : abi_value list -> abi_value option list -> bool =
  fun x_data x_data_filter ->
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


(* For debugging purpose, printing the digest *)
let string_of_option_digest : Digest.t option -> string =
  fun edig ->
  match edig with
  | None -> "NONE"
  | Some x -> Digest.to_0x x


let print_list_entries : EthListLogObjects.t -> string =
  fun entries ->
  let list_str : string list = List.map (fun (x : LogObject.t) -> string_of_option_digest (x.transactionHash)) entries in
  let estri = "\n" in
  String.concat estri list_str



(* We will iterate over the logs. Search for the ones matching the topics, event values and maybe
   transaction hash. We iterate until we find at least one entry that matches *)
let retrieve_relevant_list_logs_data : delay:float -> start_revision:Revision.t -> contract_address:Address.t -> transaction_hash:Digest.t option -> topics:Bytes.t option list -> abi_type list -> abi_value option list -> (Revision.t * (LogObject.t * abi_value list) list) Lwt_exn.t =
  fun ~delay  ~start_revision  ~contract_address  ~transaction_hash  ~topics list_data_type data_value_search ->
  let open Lwt_exn in
  Logging.log "|list_data_type|=%d" (List.length list_data_type);
  Logging.log "|data_value_search|=%d" (List.length data_value_search);
  let starting_watch_ref : (Revision.t ref) = ref start_revision in
  let iter_state_ref : (int ref) = ref 0 in
  let rec fct_downloading start_block iter_state =
    retrieve_last_entries (Revision.add start_block Revision.one)
      ~contract_address  ~topics
    >>= fun (start_block_in, entries) ->
        Logging.log "retrieve_relevant transaction_hash=%s" (string_of_option_digest transaction_hash);
        Logging.log "List transaction_hash=%s" (print_list_entries entries);
        let only_matches_record = flip List.filter entries @@ fun l ->
          is_matching_data (decode_data l.data list_data_type)
                           data_value_search
        in let only_matches_hash = List.filter (fun (l : LogObject.t) ->
                                 match transaction_hash with
                                 | None -> true
                                 | Some transaction_hash_search ->
                                    (match l.transactionHash with
                                     | None -> true
                                     | Some transaction_hash_log -> Digest.equal transaction_hash_log transaction_hash_search))
                               only_matches_record
        in let relevant = flip List.map only_matches_hash @@ fun l ->
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
          (Logging.log "|only_matches_record|=%d   |only_matches_hash|=%d   |relevant|=%d" (List.length only_matches_record) (List.length only_matches_hash)  (List.length relevant);
           return (start_block_in, relevant))
  in fct_downloading !starting_watch_ref !iter_state_ref



(* We call for the relevant_list logs and then we return one entry if there is just one.
   If there is more than one, then bork *)
let retrieve_relevant_single_logs_data : delay:float -> contract_address:Address.t -> transaction_hash:Digest.t option -> topics:Bytes.t option list -> abi_type list -> abi_value option list -> (LogObject.t * (abi_value list)) Lwt_exn.t =
  fun ~delay  ~contract_address  ~transaction_hash  ~topics  list_data_type  data_value_search  ->
  let open Lwt_exn in
  let start_revision = Revision.zero in
  retrieve_relevant_list_logs_data
    ~delay
    ~start_revision
    ~contract_address
    ~transaction_hash
    ~topics
    list_data_type
    data_value_search
  >>= fun ((_rev, llogs) : (Revision.t * (LogObject.t * (abi_value list)) list)) ->
  let len = List.length llogs in
  if len > 1 then
    bork "The length should be exactly 1"
  else
    return (List.hd llogs)


(* We wait for contract event. Only difference is that delay is computed from the
   input file *)
let wait_for_contract_event ~contract_address ~transaction_hash ~topics list_data_type data_value_search =
  Logging.log "Beginning of wait_for_contract_event";
  retrieve_relevant_single_logs_data
    ~delay:Side_chain_server_config.delay_wait_ethereum_watch_in_seconds
    ~contract_address
    ~transaction_hash
    ~topics
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

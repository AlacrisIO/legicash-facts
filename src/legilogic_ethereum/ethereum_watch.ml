open Legilogic_lib
open Lib
open Types
open Action
open Signing
open Ethereum_json_rpc


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

let topics_type_conversion (topics : Bytes.t list) : Bytes.t option list =
  let (topics_opt : Bytes.t option list) = List.map (fun x -> Some x) topics in
  List.cons None topics_opt

                                              
(* Look for confirmed or not confirmed blocks. NEED TO ADD: NUMBER of confirmation *)
let retrieve_last_entries (start_block : Revision.t) (contract_address : Address.t) (topics : Bytes.t list) : (Revision.t * (LogObject.t list)) Lwt_exn.t =
  Lwt_exn.bind (eth_block_number ())
    (fun (to_block : Revision.t) ->
      (*      let (eth_object : EthObject.t) = {from_block=(Some (Block_number start_block)); to_block=(Some (Block_number to_block)); address = (Some contract_address); topics=(Some topics); blockhash=None} in *)
      (*      let (eth_object : EthObject.t) = {from_block=None; to_block=None; address = None; topics=(Some topics); blockhash=None} in *)
      let (eth_object : EthObject.t) = {from_block=(Some (Block_number start_block)); to_block=(Some (Block_number to_block)); address =(Some contract_address); topics=(Some (topics_type_conversion topics)); blockhash=None} in
      Logging.log "retrieve_last_entries. Before call to eth_get_logs";
      Lwt_exn.bind (eth_get_logs eth_object)
        (fun (recLLO : EthListLogObjects.t) ->
          Logging.log "retrieve_last_entries, After call to eth_get_logs";
          Lwt_exn.return (to_block,recLLO)))

let retrieve_relevant_list_logs
      (delay : float) (contract_address : Address.t) (topics : Bytes.t list) : LogObject.t list Lwt_exn.t =
  let rec fct_downloading (start_block : Revision.t) : LogObject.t list Lwt_exn.t =
    Lwt_exn.bind (retrieve_last_entries start_block contract_address topics)
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


let retrieve_relevant_single_logs
      (delay : float) (contract_address : Address.t) (topics : Bytes.t list) : LogObject.t Lwt_exn.t =
  Lwt_exn.bind (retrieve_relevant_list_logs delay contract_address topics)
    (fun (llogs : LogObject.t list) ->
      let (len : int) = List.length llogs in
      if len > 1 then
        bork "The length should be exactly 1"
      else
        Lwt_exn.return (List.hd llogs))
       
      

   
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

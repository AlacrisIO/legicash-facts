open Lib
open Action
open Yojsoning
(* open Persisting*)
open Json_rpc
open Signing
open Types



type mkb_rpc_config =
  { scheme : string
  ; host : string
  ; port : int
  ; topic : string
  ; username : string
  ; committee_size : int
  ; min_interval_insertion_micros : int
  ; total_capacity_mem : int
  ; instant_capacity_mem : int
  ; total_throughput_per_min : int
  ; total_throughput_per_sec : int
  ; retention_time : int
  ; retention_size : int
  ; hash_method : string }
[@@deriving of_yojson]

(*
  GENERAL CODE FOR ACCESSING THE MKB
 *)

let mkb_rpc_config =
  lazy
    (let config_file = Config.get_config_filename "mkb_config.json" in
     match yojson_of_file config_file
           |> mkb_rpc_config_of_yojson with
     | Ok config -> config
     | Error msg -> bork "Error loading Mutual Knowledge Base JSON RPC configuration: %s" msg)

let mkb_net =
  lazy
    (let lazy { scheme; host; port } = mkb_rpc_config in
     Uri.make ~scheme ~host ~port ())

let mkb_topic_user =
  lazy
    (let lazy {topic; username } = mkb_rpc_config in
     (topic, username))


let mkb_mutex = Lwt_mutex.create ()

let mkb_json_rpc
      method_name result_decoder param_encoder ?timeout ?log params =
  Logging.log "ETH json rpc method_name=%s" method_name;
  Lwt_mutex.with_lock mkb_mutex
    (fun () ->
       json_rpc (Lazy.force mkb_net) method_name result_decoder param_encoder ?timeout ?log params)


(*
  The permanent system
*)

module TransactionMutualKnowledge = struct 
  type t = {
      topic : string
    }
  [@@deriving yojson]
end





type request_mkb_update =
  | Submit of (string * TransactionMutualKnowledge.t OrExn.t Lwt.u)

let request_mkb_update_mailbox : request_mkb_update Lwt_mvar.t = Lwt_mvar.create_empty ()

let post_to_mailbox_state_update : string -> TransactionMutualKnowledge.t OrExn.t Lwt.t =
  fun str ->
  simple_client request_mkb_update_mailbox
    (fun ((_x_digest, x_resolver) : (string * TransactionMutualKnowledge.t OrExn.t Lwt.u)) -> Submit (str,x_resolver)) str




let inner_call_mkb () =
  let open Lwt in
  let rec inner_loop : unit -> unit Lwt.t =
    fun () ->
    Lwt_mvar.take request_mkb_update_mailbox
    >>= function
    | Submit ((new_entry, notify_u) : (string * TransactionMutualKnowledge.t OrExn.t Lwt.u)) ->
       let (topic, username) = (Lazy.force mkb_topic_user) in
       inner_loop ()
  in inner_loop ()

(*
   Some data specific requests
*)


module MkbTopicDescription = struct
  type t =
    { topic : string
    ; committee_size : int
    ; min_interval_insertion_micros : int
    ; total_capacity_mem : int
    ; instant_capacity_mem : int
    ; total_throughput_per_min : int
    ; total_throughput_per_sec : int
    ; retention_time : int
    ; retention_size : int
    ; hash_method : string }
  [@@deriving yojson]
end

type mkb_add_registrar =
  { topic : string
  }
[@@deriving of_yojson]

type mkb_send_data =
  { message : string
  }
[@@deriving of_yojson]

let yojson_noargs = fun () -> `Null
let yojson_0args = fun () -> `List []
let yojson_1arg f = fun x -> `List [f x]
let yojson_2args f g = fun (x, y) -> `List [f x; g y]
let yojson_3args f g h = fun (x, y, z) -> `List [f x; g y; h z]
let yojson_4args f g h k = fun (x, y, z, t) -> `List [f x; g y; h z; k t]


let mkb_topic_creation =
  mkb_json_rpc "topic_creation"
    Digest.of_yojson_exn
    (yojson_1arg MkbTopicDescription.to_yojson)

let mkb_add_registrar =
  mkb_json_rpc "add_registrar"
    Digest.of_yojson_exn
    (yojson_2args Address.to_yojson Address.to_yojson)

let mkb_add_account =
  mkb_json_rpc "add_account"
    Digest.of_yojson_exn
    (yojson_3args Address.to_yojson Address.to_yojson Address.to_yojson)

let mkb_send_data =
  mkb_json_rpc "send_data"
    Digest.of_yojson_exn
    (yojson_4args Address.to_yojson Address.to_yojson Address.to_yojson Address.to_yojson)

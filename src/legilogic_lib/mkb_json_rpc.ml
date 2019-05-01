open Lib
open Action
open Yojsoning
open Persisting
open Json_rpc
(* open Signing *)
open Types



type mkb_rpc_config_type =
  { scheme : string
  ; main_host : string
  ; main_port : int
  ; list_neighboring_registrar : string list
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
           |> mkb_rpc_config_type_of_yojson with
     | Ok config -> config
     | Error msg -> bork "Error loading Mutual Knowledge Base JSON RPC configuration: %s" msg)

let mkb_net =
  lazy
    (let lazy { scheme; main_host; main_port } = mkb_rpc_config in
     Uri.make ~scheme ~host:main_host ~port:main_port ())


let mkb_mutex = Lwt_mutex.create ()

let mkb_json_rpc
      method_name result_decoder param_encoder ?timeout ?log params =
  Logging.log "MKB json rpc method_name=%s" method_name;
  Lwt_mutex.with_lock mkb_mutex
    (fun () ->
       json_rpc (Lazy.force mkb_net) method_name result_decoder param_encoder ?timeout ?log params)


(*
   Some request specific data types.
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

module SendDataResult = struct
  type t = { hash : string
           }
  [@@deriving yojson {strict = false}]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end

type mkb_add_registrar =
  { topic : string
  }
[@@deriving of_yojson]

type mkb_send_data_type =
  { message : string
  }
[@@deriving of_yojson]

let yojson_noargs = fun () -> `Null
let yojson_0args = fun () -> `List []
let yojson_1arg f = fun x -> `List [f x]
let yojson_2args f g = fun (x, y) -> `List [f x; g y]
let yojson_3args f g h = fun (x, y, z) -> `List [f x; g y; h z]
let yojson_4args f g h k = fun (x, y, z, t) -> `List [f x; g y; h z; k t]


let get_mkb_topic_description : mkb_rpc_config_type -> MkbTopicDescription.t =
  fun x_mkb_config ->
  { topic=x_mkb_config.topic;
    committee_size=x_mkb_config.committee_size;
    min_interval_insertion_micros=x_mkb_config.min_interval_insertion_micros;
    total_capacity_mem=x_mkb_config.total_capacity_mem;
    instant_capacity_mem=x_mkb_config.instant_capacity_mem;
    total_throughput_per_min=x_mkb_config.total_throughput_per_min;
    total_throughput_per_sec=x_mkb_config.total_throughput_per_sec;
    retention_time=x_mkb_config.retention_time;
    retention_size=x_mkb_config.retention_size;
    hash_method=x_mkb_config.hash_method}

let mkb_topic_creation =
  mkb_json_rpc "topic_creation"
    Digest.of_yojson_exn
    (yojson_1arg MkbTopicDescription.to_yojson)

let mkb_add_registrar =
  mkb_json_rpc "add_registrar"
    Digest.of_yojson_exn
    (yojson_2args StringT.to_yojson StringT.to_yojson)

let mkb_add_account =
  mkb_json_rpc "add_account"
    Digest.of_yojson_exn
    (yojson_2args StringT.to_yojson StringT.to_yojson)

let mkb_send_data : (string * string * string * string) -> SendDataResult.t Lwt_exn.t =
  mkb_json_rpc "send_data"
    SendDataResult.of_yojson_exn
    (yojson_4args StringT.to_yojson StringT.to_yojson StringT.to_yojson StringT.to_yojson)


let rec mkb_send_data_iterate_fail : (string * string * string * string) -> SendDataResult.t Lwt.t =
  fun x ->
  Lwt.bind (mkb_send_data x)
  (function
  | Ok x -> Lwt.return x
  | _ -> mkb_send_data_iterate_fail x)


  
(*
  The permanent system
*)

module TransactionMutualKnowledge = struct
  type t = { topic : string
           ; hash : string
           }
  [@@deriving yojson]
end

type request_mkb_update =
  | Submit of (string * TransactionMutualKnowledge.t Lwt.u)

let request_mkb_update_mailbox : request_mkb_update Lwt_mvar.t = Lwt_mvar.create_empty ()

let post_to_mailbox_state_update : string -> TransactionMutualKnowledge.t Lwt.t =
  fun str ->
  simple_client request_mkb_update_mailbox
    (fun ((_x_digest, x_resolver) : (string * TransactionMutualKnowledge.t Lwt.u)) -> Submit (str,x_resolver)) str




let inner_call_mkb () =
  let open Lwt in
  let str_start : string = "" in
  let hash_ref : string ref = ref str_start in
  let rec inner_loop : unit -> unit Lwt.t =
    fun () ->
    Lwt_mvar.take request_mkb_update_mailbox
    >>= function
    | Submit ((new_entry, notify_u) : (string * TransactionMutualKnowledge.t Lwt.u)) ->
       let mkb_rpc_config_v = (Lazy.force mkb_rpc_config) in
       mkb_send_data_iterate_fail (mkb_rpc_config_v.topic, mkb_rpc_config_v.username, !hash_ref, new_entry)
       >>= (fun x ->
         hash_ref := x.hash;
         Lwt.wakeup_later notify_u {topic=mkb_rpc_config_v.topic; hash=x.hash};
         inner_loop ())
  in inner_loop ()


let mkb_add_neighboring_registrar : string -> string list -> unit Lwt_exn.t =
  fun topic list_reg ->
  let len = List.length list_reg in
  let rec individual_addition : int -> unit Lwt_exn.t =
    fun i ->
    let name_reg = List.nth list_reg i in
    Lwt.bind (mkb_add_registrar (topic, name_reg))
      (function
       | Ok _x -> (if (i == len-1) then
                    Lwt_exn.return ()
                  else
                    individual_addition (i+1))
       | Error e -> Lwt.return (Error e))
  in individual_addition 0

let init_mkb_server () =
  Logging.log "Beginning of init_mkb_server";
  let mkb_rpc_config_v = (Lazy.force mkb_rpc_config) in
  let topic = mkb_rpc_config_v.topic in
  let username = mkb_rpc_config_v.username in
  let list_neighboring_registrar = mkb_rpc_config_v.list_neighboring_registrar in
  let mkb_topic_desc = get_mkb_topic_description mkb_rpc_config_v in
  Lwt.async inner_call_mkb;
  let open Lwt_exn in
  mkb_topic_creation mkb_topic_desc
  >>= fun _ -> mkb_add_neighboring_registrar topic list_neighboring_registrar
  >>= fun _ -> mkb_add_account (topic, username)
  >>= fun _ -> return ()

open Lib
open Action
open Yojsoning
open Json_rpc
(*open Types*)
open Logging
open Storage
open Digesting

type mkb_rpc_config_type =
  { use_mkb : bool
  ; scheme : string
  ; main_host : string
  ; main_port : int
  ; neighboring_registrar_list : string list
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

let username_set = ref false


module StringO = struct
  type t = string
  [@@deriving rlp, yojson]
  let of_yojson_exn yojson =
    let nature_str = yojson |> string_of_yojson in
    nature_str
end

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


module SendKeyValueResult = struct
  type t = { nature : string }
  [@@deriving yojson {strict = false}]
  let of_yojson_exn yojson =
    let nature_str : string = yojson |> YoJson.member "nature" |> StringO.of_yojson_exn in
    let nature_t : t = {nature = nature_str} in
    nature_t
(*  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson; of_yojson}
           end) : (PersistableS with type t := t)) *)
end

module GetKeyValueResult = struct
  type t = { nature : string
           ; value : string }
  [@@deriving yojson {strict = false}]
  let of_yojson_exn yojson =
    let nature_str : string = yojson |> YoJson.member "nature" |> StringO.of_yojson_exn in
    let value_str : string = yojson |> YoJson.member "value" |> StringO.of_yojson_exn in
    let data_t : t = {nature = nature_str; value = value_str} in
    data_t
(*  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson; of_yojson}
           end) : (PersistableS with type t := t)) *)
end

module SendDataResult = struct
  type t = { nature : string
           ; hash : string
           }
  [@@deriving yojson {strict = false}]
  let of_yojson_exn yojson =
    let nature_str = yojson |> YoJson.member "nature" |> StringO.of_yojson_exn in
    let hash_str = yojson |> YoJson.member "hash" |> StringO.of_yojson_exn in
    let data_t : t = {nature = nature_str; hash = hash_str} in
    data_t
(*  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson; of_yojson}
           end) : (PersistableS with type t := t)) *)
end





type mkb_add_registrar =
  { topic : string
  }
[@@deriving of_yojson]

type mkb_send_data_type =
  { message : string
  }
[@@deriving of_yojson]



(*
type mkb_status_info =
  { reply : bool
  ; message : string
  }
[@@deriving of_yojson] *)



let get_mkb_topic_description : mkb_rpc_config_type -> MkbTopicDescription.t =
  fun x_mkb_config ->
  { topic = x_mkb_config.topic;
    committee_size = x_mkb_config.committee_size;
    min_interval_insertion_micros = x_mkb_config.min_interval_insertion_micros;
    total_capacity_mem = x_mkb_config.total_capacity_mem;
    instant_capacity_mem = x_mkb_config.instant_capacity_mem;
    total_throughput_per_min = x_mkb_config.total_throughput_per_min;
    total_throughput_per_sec = x_mkb_config.total_throughput_per_sec;
    retention_time = x_mkb_config.retention_time;
    retention_size = x_mkb_config.retention_size;
    hash_method = x_mkb_config.hash_method}

let mkb_topic_creation =
  mkb_json_rpc "topic_creation"
    StringO.of_yojson_exn
    (yojson_singlearg MkbTopicDescription.to_yojson)

let mkb_add_registrar =
  mkb_json_rpc "add_registrar"
    StringO.of_yojson_exn
    (yojson_2args StringO.to_yojson StringO.to_yojson)

let mkb_add_account =
  mkb_json_rpc "add_account"
    SendDataResult.of_yojson_exn
    (yojson_2args StringO.to_yojson StringO.to_yojson)

let mkb_send_data : (string * string * string * string) -> SendDataResult.t Lwt_exn.t =
  mkb_json_rpc "send_data"
    SendDataResult.of_yojson_exn
    (yojson_4args StringO.to_yojson StringO.to_yojson StringO.to_yojson StringO.to_yojson)

let mkb_get_from_latest : (string * string * string) -> GetKeyValueResult.t Lwt_exn.t =
  mkb_json_rpc "get_from_latest"
    GetKeyValueResult.of_yojson_exn
    (yojson_3args StringO.to_yojson StringO.to_yojson StringO.to_yojson)

let mkb_send_key_value : (string * string * string * string) -> SendKeyValueResult.t Lwt_exn.t =
  mkb_json_rpc "send_key_value"
    SendKeyValueResult.of_yojson_exn
    (yojson_4args StringO.to_yojson StringO.to_yojson StringO.to_yojson StringO.to_yojson)

let mkb_get_key_value : (string * string * string) -> GetKeyValueResult.t Lwt_exn.t =
  mkb_json_rpc "get_key_value"
    GetKeyValueResult.of_yojson_exn
    (yojson_3args StringO.to_yojson StringO.to_yojson StringO.to_yojson)

  
(*
let rec infinite_retry : ('a -> 'b Lwt_exn.t) -> 'a -> 'b Lwt.t =
  fun f x ->
  Lwt.bind (f x)
  (function
  | Ok x_ret -> Lwt.return x_ret
  | _ -> if mkb_json_rpc_log then
           log "Reiterating operation of function f with value x";
         infinite_retry f x)
 *)

let set_mkb_username : string -> unit Lwt_exn.t =
  fun username ->
  let open Lwt_exn in
  let mkb_rpc_config_v = (Lazy.force mkb_rpc_config) in
  if !username_set then
    (username_set := true;
     mkb_add_account (mkb_rpc_config_v.topic, username)
     >>= fun _ -> return ())
  else
    return ()

(*
let rec mkb_send_data_iterate_fail : (string * string * string * string) -> SendDataResult.t Lwt_exn.t =
  fun x ->
  Lwt.bind (mkb_send_data x)
  (function
  | Ok x -> Lwt.return x
  | _ -> if mkb_json_rpc_log then
           log "Reiterating mkb_send_data in case of failure";
         mkb_send_data_iterate_fail x)
 *)

(*
  The permanent system
*)

module TransactionMutualKnowledge = struct
  type t = { topic : string
           ; hash : Digest.t
           }
             (*  [@@deriving yojson]*)
end

module TransactionMkbSend = struct
  type t = { hash : Digest.t }
             (*  [@@deriving yojson]*)
end

module TransactionMkbGet = struct
  type t = { value : string }
  [@@deriving yojson]
end




type request_mkb_update =
  | SubmitSequence of (string * Digest.t * TransactionMutualKnowledge.t OrExn.t Lwt.u)
  | SendKeyValue of (string * string * string * TransactionMkbSend.t OrExn.t Lwt.u)
  | GetKey of (string * string * TransactionMkbGet.t OrExn.t Lwt.u)
  | GetLatest of (string * string * TransactionMkbGet.t OrExn.t Lwt.u)

let request_mkb_update_mailbox : request_mkb_update Lwt_mvar.t = Lwt_mvar.create_empty ()


let post_to_mkb_mailbox : string -> Digest.t -> unit Lwt.t =
  fun username digest ->
  let mkb_rpc_config_v = (Lazy.force mkb_rpc_config) in
  let open Lwt in
  if mkb_rpc_config_v.use_mkb then
    simple_client request_mkb_update_mailbox
      (fun ((_x_digest, x_resolver) : (Digest.t * TransactionMutualKnowledge.t OrExn.t Lwt.u)) ->
        SubmitSequence (username, digest, x_resolver)) digest
    >>= fun _ -> return ()
  else
    return ()
(*    Lwt.return TransactionMutualKnowledge.{topic = ""; hash = Digest.zero}*)

let post_send_key_value_to_mkb_mailbox : string -> string -> string -> unit Lwt_exn.t =
  fun username key value ->
  let open Lwt_exn in
  let fct = simple_client request_mkb_update_mailbox
    (fun ((e_user, e_key, e_value), x_resolver : (string * string * string) * TransactionMkbSend.t OrExn.t Lwt.u) ->
      SendKeyValue (e_user, e_key, e_value, x_resolver)) in
  fct (username,key,value)
  >>= fun _ -> return ()


let post_get_key_to_mkb_mailbox : string -> string -> string Lwt_exn.t =
  fun username key ->
  let open Lwt_exn in
  let fct = simple_client request_mkb_update_mailbox
    (fun ((e_user, e_key), x_resolver : (string * string) * TransactionMkbGet.t OrExn.t Lwt.u) ->
      GetKey (e_user, e_key, x_resolver)) in
  fct (username,key)
  >>= fun x -> return x.value

let post_get_latest_to_mkb_mailbox : string -> string -> string Lwt_exn.t =
  fun username key ->
  let open Lwt_exn in
  let fct = simple_client request_mkb_update_mailbox
    (fun ((e_user, e_key), x_resolver : (string * string) * TransactionMkbGet.t OrExn.t Lwt.u) ->
      GetLatest (e_user, e_key, x_resolver)) in
  fct (username,key)
  >>= fun x -> return x.value

let db_value_of_mkb_digest :  ('a -> 'b) -> Digest.t -> 'd =
  fun unmarshal_string (digest : Digest.t) ->
  let open Lwt_exn in
  let e_key : string = content_addressed_storage_key digest in
  let username = "LCFS0001" in
  post_get_key_to_mkb_mailbox username e_key
  >>= fun x -> return (unmarshal_string x)


let get_value : 'a OrExn.t -> 'a =
  fun a_res ->
  match a_res with
  | Ok x -> x
  | Error _ -> bork "Error getting the value"


let get_transactionmkbget : GetKeyValueResult.t OrExn.t -> TransactionMkbGet.t OrExn.t =
  fun receipt_exn ->
  match receipt_exn with
  | Ok receipt -> Ok {value = receipt.value}
  | Error e -> Error e

let inner_call_mkb () =
  let open Lwt in
  let hash_ref : Digest.t ref = ref Digest.zero in
  let mkb_rpc_config_v = (Lazy.force mkb_rpc_config) in
  (*  let username = "LCFS0001" in *)
  let rec inner_loop : unit -> unit Lwt.t =
    fun () ->
    Lwt_mvar.take request_mkb_update_mailbox
    >>= function
    | SubmitSequence ((username, new_entry, notify_u) : (string * Digest.t * TransactionMutualKnowledge.t OrExn.t Lwt.u)) ->
       set_mkb_username username
       >>= fun _ ->
       mkb_send_data (mkb_rpc_config_v.topic, username,(Digest.to_0x !hash_ref),(Digest.to_0x new_entry))
       >>= fun receipt_exn ->
       let receipt = get_value receipt_exn in
       let new_digest = Digest.of_0x receipt.hash in
       hash_ref := new_digest;
       let ret_val : TransactionMutualKnowledge.t OrExn.t = Ok {topic=mkb_rpc_config_v.topic; hash=new_digest} in
       Lwt.wakeup_later notify_u ret_val;
       inner_loop ()
    | SendKeyValue ((username, key, value, notify_u) : (string * string * string * TransactionMkbSend.t OrExn.t Lwt.u)) ->
       set_mkb_username username
       >>= fun _ ->
       mkb_send_key_value (mkb_rpc_config_v.topic, username, key, value)
       >>= fun receipt_exn ->
       let receipt = get_value receipt_exn in
       let ret_val : TransactionMkbSend.t OrExn.t = Ok {hash = (Digesting.digest_of_string receipt.nature)} in
       Lwt.wakeup_later notify_u ret_val;
       inner_loop ()
    | GetKey (username, key, notify_u) ->
       set_mkb_username username
       >>= fun _ ->
       mkb_get_key_value (mkb_rpc_config_v.topic, username, key)
       >>= fun receipt_exn ->
       let ret_value = get_transactionmkbget receipt_exn in
       Lwt.wakeup_later notify_u ret_value;
       inner_loop ()
    | GetLatest (username, key, notify_u) ->
       set_mkb_username username
       >>= fun _ ->
       mkb_get_from_latest (mkb_rpc_config_v.topic, username, key)
       >>= fun receipt_exn ->
       let ret_value = get_transactionmkbget receipt_exn in
       Lwt.wakeup_later notify_u ret_value;
       inner_loop ()
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

let ensure_mkb_server () =
  if mkb_json_rpc_log then
    log "Beginning of ensure_mkb_server";
  let open Lwt_exn in
  let mkb_rpc_config_v = (Lazy.force mkb_rpc_config) in
  if mkb_rpc_config_v.use_mkb then
    let mkb_rpc_config_v = (Lazy.force mkb_rpc_config) in
    let topic = mkb_rpc_config_v.topic in
    let neighboring_registrar_list = mkb_rpc_config_v.neighboring_registrar_list in
    let mkb_topic_desc = get_mkb_topic_description mkb_rpc_config_v in
    Lwt.async inner_call_mkb;
    mkb_topic_creation mkb_topic_desc
    >>= fun _ -> mkb_add_neighboring_registrar topic neighboring_registrar_list
    >>= fun _ -> if mkb_json_rpc_log then
                   log "The MKB has been successfully set up";
                 return ()
  else
    return ()


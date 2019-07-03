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

let username_todo = ref true

let list_char = ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "a"; "b"; "c"; "d"; "e"; "f"]






let string_to_hexstring : string -> string =
  fun strin ->
  let len = String.length strin in
  let list_pair = List.init len (fun idx ->
                      let echar = String.get strin idx in
                      let ecode = Char.code echar in
                      let ecode_res = ecode mod 16 in
                      let ecode_q = (ecode - ecode_res) / 16 in
                      let echar1 = List.nth list_char ecode_res in
                      let echar2 = List.nth list_char ecode_q in
                      String.concat "" [echar1; echar2]) in
  String.concat "" list_pair


let hexstring_to_string : string -> string =
  fun strin ->
  let len = String.length strin in
  let totstr = "0123456789abcdef" in
  let len2 = len / 2 in
  let seq_char = List.init len2 (fun idx ->
                     let pos1 = 2*idx in
                     let pos2 = 2*idx+1 in
                     let echar1 = String.get strin pos1 in
                     let echar2 = String.get strin pos2 in
                     let val1 = String.rindex totstr echar1 in
                     let val2 = String.rindex totstr echar2 in
                     let full_val = val1 + 16 * val2 in
                     let echar = Char.chr full_val in
                     Char.escaped echar) in
  String.concat "" seq_char


let add_quote : string -> string =
  fun strin ->
  String.concat "" ["\""; strin; "\""]



module StringO = struct
  type t = string
  [@@deriving rlp]
  let of_yojson_exn yojson =
    if mkb_json_rpc_log then
      Logging.log "Beginning of of_yojson_exn";
    let nature_str = yojson |> string_of_yojson in
    if mkb_json_rpc_log then
      Logging.log "of_yojson_exn, nature_str=%s" nature_str;
    nature_str
  let of_yojson_hexadecimal_exn yojson =
    if mkb_json_rpc_log then
      Logging.log "Beginning of of_yojson_hexadecimal_exn";
    let nature_str = yojson |> string_of_yojson |> hexstring_to_string in
    if mkb_json_rpc_log then
      Logging.log "of_yojson_hexadecimal_exn, nature_str=%s" nature_str;
    nature_str
  let to_yojson strin =
    if mkb_json_rpc_log then
      Logging.log "to_yojson, strin=%s" strin;
    let yojson_str = strin |> add_quote |> yojson_of_string in
    if mkb_json_rpc_log then
      Logging.log "to_yojson, after creation of yojson_str";
    yojson_str
  let to_yojson_hexadecimal strin =
    if mkb_json_rpc_log then
      Logging.log "to_yojson_hexadecimal, strin=%s" strin;
    let yojson_str = strin |> string_to_hexstring |> add_quote |> yojson_of_string in
    if mkb_json_rpc_log then
      Logging.log "to_yojson_hexadecimal, after creation of yojson_str";
    yojson_str
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
  mkb_json_rpc "mkb_topic_creation"
    StringO.of_yojson_exn
    (yojson_singlearg MkbTopicDescription.to_yojson)

let mkb_add_registrar =
  mkb_json_rpc "mkb_add_registrar"
    StringO.of_yojson_exn
    (yojson_2args StringO.to_yojson StringO.to_yojson)

let mkb_add_account =
  mkb_json_rpc "mkb_add_account"
    SendDataResult.of_yojson_exn
    (yojson_2args StringO.to_yojson StringO.to_yojson)

let mkb_send_data : (string * string * string * string) -> SendDataResult.t Lwt_exn.t =
  mkb_json_rpc "mkb_send_data"
    SendDataResult.of_yojson_exn
    (yojson_4args StringO.to_yojson StringO.to_yojson StringO.to_yojson_hexadecimal StringO.to_yojson_hexadecimal)

let mkb_get_from_latest : (string * string) -> GetKeyValueResult.t Lwt_exn.t =
  mkb_json_rpc "mkb_get_from_latest"
    GetKeyValueResult.of_yojson_exn
    (yojson_2args StringO.to_yojson StringO.to_yojson)

let mkb_send_key_value : (string * string * string * string) -> StringO.t Lwt_exn.t =
  mkb_json_rpc "mkb_send_key_value"
    StringO.of_yojson_exn
    (yojson_4args StringO.to_yojson StringO.to_yojson StringO.to_yojson_hexadecimal StringO.to_yojson_hexadecimal)

let mkb_get_key_value : (string * string * string) -> GetKeyValueResult.t Lwt_exn.t =
  mkb_json_rpc "mkb_get_key_value"
    GetKeyValueResult.of_yojson_exn
    (yojson_3args StringO.to_yojson StringO.to_yojson StringO.to_yojson_hexadecimal)

let rec infinite_retry : ('a -> 'b Lwt_exn.t) -> 'a -> 'b Lwt.t =
  fun f x ->
  Lwt.bind (f x)
  (function
  | Ok x_ret -> Lwt.return x_ret
  | _ -> if mkb_json_rpc_log then
           log "Reiterating operation of function f with value x";
         infinite_retry f x)

let set_mkb_username : string -> unit Lwt_exn.t =
  fun username ->
  let open Lwt_exn in
  let mkb_rpc_config_v = (Lazy.force mkb_rpc_config) in
  if mkb_json_rpc_log then
    Logging.log "Beginning set_mkb_username username_todo=%B" !username_todo;
  if !username_todo then
    (username_todo := false;
     mkb_add_account (mkb_rpc_config_v.topic, username)
     >>= fun _ -> return ())
  else
    return ()

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
  | SubmitSequence of (string * string * TransactionMutualKnowledge.t OrExn.t Lwt.u)
  | SendKeyValue of (string * string * string * TransactionMkbSend.t OrExn.t Lwt.u)
  | GetKey of (string * string * TransactionMkbGet.t OrExn.t Lwt.u)
  | GetLatest of (string * TransactionMkbGet.t OrExn.t Lwt.u)

let request_mkb_update_mailbox : request_mkb_update Lwt_mvar.t = Lwt_mvar.create_empty ()


let post_to_mkb_mailbox : string -> string -> unit Lwt.t =
  fun username value ->
  let open Lwt in
  let fct = simple_client request_mkb_update_mailbox
              (fun (  ((e_user,e_value), x_resolver) : (string*string) * TransactionMutualKnowledge.t OrExn.t Lwt.u) ->
                SubmitSequence (e_user, e_value, x_resolver)) in
  fct (username, value)
  >>= fun _ -> return ()

let post_send_key_value_to_mkb_mailbox : (string * string * string) -> unit Lwt_exn.t =
  fun (username,key,value) ->
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

let post_get_latest_to_mkb_mailbox : string -> string Lwt_exn.t =
  fun username ->
  let open Lwt_exn in
  let fct = simple_client request_mkb_update_mailbox
    (fun (e_user, x_resolver : string * TransactionMkbGet.t OrExn.t Lwt.u) ->
      GetLatest (e_user, x_resolver)) in
  fct username
  >>= fun x -> return x.value

let db_value_of_mkb_digest :  ('a -> 'b) -> Digest.t -> 'd =
  fun unmarshal_string (digest : Digest.t) ->
  let open Lwt_exn in
  let mkb_rpc_config_v = (Lazy.force mkb_rpc_config) in
  let e_key : string = content_addressed_storage_key digest in
  post_get_key_to_mkb_mailbox mkb_rpc_config_v.username e_key
  >>= fun x -> return (unmarshal_string x)


let get_value : 'a OrExn.t -> 'a =
  fun a_res ->
  match a_res with
  | Ok x -> x
  | Error e -> log "Error getting the value e=%s" (Printexc.to_string e);
     bork "Error getting the value"


let get_transactionmkbget : GetKeyValueResult.t OrExn.t -> TransactionMkbGet.t OrExn.t =
  fun receipt_exn ->
  match receipt_exn with
  | Ok receipt -> Ok {value = receipt.value}
  | Error e -> Error e

let inner_call_mkb () =
  let open Lwt in
  let hash_ref : Digest.t ref = ref Digest.zero in
  let mkb_rpc_config_v = (Lazy.force mkb_rpc_config) in
  let rec inner_loop : unit -> unit Lwt.t =
    fun () ->
    Lwt_mvar.take request_mkb_update_mailbox
    >>= function
    | SubmitSequence ((username, new_entry, notify_u) : (string * string * TransactionMutualKnowledge.t OrExn.t Lwt.u)) ->
       set_mkb_username username
       >>= fun _ ->
       mkb_send_data (mkb_rpc_config_v.topic, username, (Digest.to_0x !hash_ref), new_entry)
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
       let _receipt = get_value receipt_exn in
       let ret_val : TransactionMkbSend.t OrExn.t = Ok {hash = Digest.zero} in
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
    | GetLatest (username, notify_u) ->
       set_mkb_username username
       >>= fun _ ->
       mkb_get_from_latest (mkb_rpc_config_v.topic, username)
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
    Lwt.bind
      (if mkb_json_rpc_log then
         log "Before call to mkb_add_registrar topic=%s name_reg=%s" topic name_reg;
       mkb_add_registrar (topic, name_reg))
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
    if mkb_json_rpc_log then
      log "Before call to mkb_topic_creation";
    mkb_topic_creation mkb_topic_desc
    >>= fun _ ->
    if mkb_json_rpc_log then
      log "Before call to mkb_add_neighboring_registrar";
    mkb_add_neighboring_registrar topic neighboring_registrar_list
    >>= fun _ ->
    if mkb_json_rpc_log then
      log "The MKB has been successfully set up";
    return ()
  else
    return ()


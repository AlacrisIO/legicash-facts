open Lwt
open Cohttp
open Cohttp_lwt_unix

open Legilogic_lib
open Lib
open Yojsoning
open Logging

type ethereum_rpc_call =
  (* DApps methods, use anywhere *)
  | Eth_getBalance
  | Eth_sendTransaction
  | Eth_getTransactionByHash
  | Eth_getTransactionCount
  | Eth_getTransactionReceipt
  (* Geth-specific methods, should only be used in tests *)
  | Personal_importRawKey
  | Personal_listAccounts
  | Personal_newAccount
  | Personal_unlockAccount
[@@deriving show]

(* network parameters for geth or other node *)
let ethereum_net = Uri.make ~scheme:"http" ~host:"localhost" ~port:8545 ()

let json_rpc_version = "2.0"

(* Global state, e.g. to correlate responses and answers in logs. *)
let id_counter = make_counter ()

let send_rpc_call_to_net json =
  let json_str = string_of_yojson json in
  (* For debugging, uncomment this line: *)
  log "Sending rpc call to geth: %s" json_str;
  Client.post
    ~body:(Cohttp_lwt__.Body.of_string json_str)
    ~headers:(Cohttp.Header.add (Cohttp.Header.init ()) "Content-Type" "application/json")
    ethereum_net
  >>= fun (resp, body) ->
  let _ = resp |> Response.status |> Code.code_of_status in
  Cohttp_lwt.Body.to_string body
  >>= fun response_str ->
  (* For debugging, uncomment this line: *)
  log "Receive rpc response from geth: %s" response_str;
  Lwt.return (yojson_of_string response_str)


(* given constructor, build JSON RPC call name *)
let json_rpc_callname call =
  let full_name = show_ethereum_rpc_call call in
  let len = String.length full_name in
  let name =
    try
      let dotndx = String.index full_name '.' in
      String.sub full_name (dotndx + 1) (len - dotndx - 1)
    with Not_found -> full_name
  in
  (* constructor is capitalized, actual name is not *)
  String.uncapitalize_ascii name


(* common case: all parameters are strings *)
let build_json_rpc_call call params : yojson =
  let string_params = List.map (fun s -> `String s) params in
  `Assoc
    [ ("jsonrpc", `String json_rpc_version)
    ; ("method", `String (json_rpc_callname call))
    ; ("params", `List string_params)
    ; ("id", `Int (id_counter ())) ]


(* less common case: some parameters are not strings, so they're type-tagged *)
let build_json_rpc_call_with_tagged_parameters call params =
  `Assoc
    [ ("jsonrpc", `String json_rpc_version)
    ; ("method", `String (json_rpc_callname call))
    ; ("params", `List params)
    ; ("id", `Int (id_counter ())) ]

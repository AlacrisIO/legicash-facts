open Lwt
open Cohttp
open Cohttp_lwt_unix
module TokenAmount = Main_chain.TokenAmount

type ethereum_rpc_call =
  (* DApps methods, use anywhere *)
  | Eth_getBalance
  | Eth_sendTransaction
  | Eth_getTransactionByHash
  | Eth_getTransactionCount
  | Eth_getTransactionReceipt
  (* Geth-specific methods, should only be used in tests *)
  | Personal_listAccounts
  | Personal_newAccount
  | Personal_unlockAccount
[@@deriving show]

(* network parameters for geth or other node *)
let ethereum_net = Uri.make ~scheme:"http" ~host:"localhost" ~port:8080 ()

let json_rpc_version = "2.0"

(* global state *)
let id_counter = ref 1

let send_rpc_call_to_net json =
  let json_str = Yojson.to_string json in
  Client.post
    ~body:(Cohttp_lwt__.Body.of_string json_str)
    ~headers:(Cohttp.Header.add (Cohttp.Header.init ()) "Content-Type" "application/json")
    ethereum_net
  >>= fun (resp, body) ->
  let _ = resp |> Response.status |> Code.code_of_status in
  body |> Cohttp_lwt.Body.to_string >|= Yojson.Basic.from_string


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
let build_json_rpc_call call params : Yojson.json =
  let string_params = List.map (fun s -> `String s) params in
  `Assoc
    [ ("jsonrpc", `String json_rpc_version)
    ; ("method", `String (json_rpc_callname call))
    ; ("params", `List string_params)
    ; ("id", `Int !id_counter) ]


(* less common case: some parameters are not strings, so they're type-tagged *)
let build_json_rpc_call_with_tagged_parameters call params =
  `Assoc
    [ ("jsonrpc", `String json_rpc_version)
    ; ("method", `String (json_rpc_callname call))
    ; ("params", `List params)
    ; ("id", `Int !id_counter) ]

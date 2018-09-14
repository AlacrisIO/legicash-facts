open Legilogic_lib
open Lib
open Hex
open Yojsoning
open Json_rpc
open Signing
open Types

open Main_chain

type block_parameter =
  | Block_number of Revision.t
  | Latest
  | Earliest
  | Pending

type ethereum_rpc_config =
  { scheme : string
  ; host : string
  ; port : int
  }
[@@deriving of_yojson]

let ethereum_rpc_config =
  let config_file = Config.get_config_filename "ethereum_config.json" in
  match yojson_of_file config_file
        |> ethereum_rpc_config_of_yojson with
  | Ok config -> config
  | Error msg -> bork "Error loading Ethereum JSON RPC configuration: %s" msg

(** Network parameters for geth or other node on localhost *)
let ethereum_net =
  let { scheme; host; port } = ethereum_rpc_config in
  Uri.make ~scheme ~host ~port ()

let ethereum_json_rpc
      method_name result_decoder param_encoder ?(timeout=rpc_timeout) ?(log= !rpc_log) params =
  json_rpc ethereum_net method_name result_decoder param_encoder ~timeout ~log params

let yojson_of_string_list = List.map yojson_string >> yojson_list
let yojson_singleton x = `List [x]

(* DApps methods, use anywhere *)

let block_parameter_to_yojson = function
  | Block_number x -> Revision.to_yojson x
  | Latest -> `String "latest"
  | Earliest -> `String "earliest"
  | Pending -> `String "pending"

let transaction_to_yojson Transaction.{tx_header = { sender; gas_limit; gas_price; value }; operation } =
  let operation_parameters = match operation with
    | Operation.TransferTokens recipient ->
      [("to", Address.to_yojson recipient)]
    | Operation.CreateContract code ->
      [("data", `String (unparse_0x_bytes code))]
    | Operation.CallFunction (recipient, data) ->
      [("to", Address.to_yojson recipient);
       ("data", `String (unparse_0x_bytes data))] in
  `Assoc
    (List.append
       [ ("from", Address.to_yojson sender)
       ; ("gas", TokenAmount.to_yojson gas_limit)
       ; ("gasPrice", TokenAmount.to_yojson gas_price)
       ; ("value", TokenAmount.to_yojson value) ]
       operation_parameters)

let eth_accounts =
  ethereum_json_rpc "eth_accounts"
    (list_of_yojson_exn Address.of_yojson_exn)
    (fun () -> `Null)

let eth_estimate_gas =
  ethereum_json_rpc "eth_estimateGas"
    TokenAmount.of_yojson_exn
    (fun (transaction, block_parameter) ->
       `List [transaction_to_yojson transaction; block_parameter_to_yojson block_parameter])

let eth_get_balance =
  ethereum_json_rpc "eth_getBalance"
    TokenAmount.of_yojson_exn
    (fun (address, block_parameter) ->
       `List [Address.to_yojson address; block_parameter_to_yojson block_parameter])

let eth_get_transaction_by_hash =
  ethereum_json_rpc "eth_getTransactionByHash"
    TransactionInformation.of_yojson_exn
    (Digest.to_yojson >> yojson_singleton)

let eth_get_transaction_count =
  ethereum_json_rpc "eth_getTransactionCount"
    Nonce.of_yojson_exn
    (fun (address, block_parameter) ->
       `List [Address.to_yojson address; block_parameter_to_yojson block_parameter])

let eth_get_transaction_receipt =
  ethereum_json_rpc "eth_getTransactionReceipt"
    (option_of_yojson_exn TransactionReceipt.of_yojson_exn)
    (Digest.to_yojson >> yojson_singleton)

let eth_send_transaction =
  ethereum_json_rpc "eth_sendTransaction"
    Digest.of_yojson_exn
    (transaction_to_yojson >> yojson_singleton)

let eth_block_number =
  ethereum_json_rpc "eth_blockNumber"
    Revision.of_yojson_exn
    (konstant @@ `List [])

(* Geth-specific methods, should only be used in tests *)

let personal_import_raw_key =
  ethereum_json_rpc "personal_importRawKey"
    Address.of_yojson_exn
    (fun (private_key, password) ->
       yojson_of_string_list
         [ private_key |> PrivateKey.marshal_string |> Hex.unparse_hex_string
         ; password ])

let personal_list_accounts =
  ethereum_json_rpc "personal_listAccounts"
    (list_of_yojson_exn Address.of_yojson_exn)
    (fun () -> `Null)

let personal_new_account =
  ethereum_json_rpc "personal_newAccount"
    Address.of_yojson_exn
    (fun password -> `List [`String password])

let personal_unlock_account =
  ethereum_json_rpc "personal_unlockAccount"
    YoJson.to_bool
    (fun (address, password, duration) ->
       `List [ Address.to_yojson address
             ; `String password
             ; `Int (Option.defaulting (konstant 5) duration) ])

module Test = struct
  open Action.Lwt_exn
  let%test "eth_latest_block get the current latest block" =
    (* Just checks that the block number is non-negative *)
    run (eth_block_number ~log:false >>> (Revision.sign >> (<=) 0 >> return)) ()
end

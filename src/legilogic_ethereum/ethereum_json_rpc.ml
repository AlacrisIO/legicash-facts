open Legilogic_lib
open Lib
open Yojsoning
open Persisting
open Json_rpc
open Signing
open Types

open Main_chain

type ethereum_rpc_config =
  { scheme : string
  ; host : string
  ; port : int }
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

let yojson_noargs = fun () -> `Null
let yojson_0args = fun () -> `List []
let yojson_1arg f = fun x -> `List [f x]
let yojson_2args f g = fun (x, y) -> `List [f x; g y]


module BlockParameter = struct
  type t =
    | Block_number of Revision.t
    | Latest
    | Earliest
    | Pending
  let to_yojson = function
    | Block_number x -> Revision.to_yojson x
    | Latest -> `String "latest"
    | Earliest -> `String "earliest"
    | Pending -> `String "pending"
  let of_yojson_exn yojson =
    let s = YoJson.to_string yojson in
    if s = "latest" then Latest
    else if s = "earliest" then Earliest
    else if s = "pending" then Pending
    else Block_number (Revision.of_0x_string s)
  let of_yojson = of_yojson_of_of_yojson_exn of_yojson_exn
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end

module TransactionCondition = struct
  type t =
    | Block_number of Revision.t
    | UTC_timestamp of Revision.t
    | Null
  let to_yojson = function
    | Block_number revision -> `Assoc [("block", Revision.to_yojson revision)]
    | UTC_timestamp seconds_since_epoch -> `Assoc [("time", Revision.to_yojson seconds_since_epoch)]
    | Null -> `Null
  let of_yojson_exn = function
    | `Assoc [("block", n)] -> Block_number (Revision.of_yojson_exn n)
    | `Assoc [("time", s)] -> UTC_timestamp (Revision.of_yojson_exn s)
    | `Null -> Null
    | _ -> raise (Yojson.Json_error "Not a transaction condition")
  let of_yojson = of_yojson_of_of_yojson_exn of_yojson_exn
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end

(** Parameters for a transaction as per Ethereum JSON RPC interface *)
module TransactionParameters = struct
  type t =
    { from: Address.t
    ; to_: Address.t option [@key "to"]
    ; gas: TokenAmount.t option
    ; gas_price: TokenAmount.t option [@key "gasPrice"]
    ; value: TokenAmount.t option
    ; data: Yojsoning.Bytes.t option
    ; nonce: Nonce.t option
    ; condition: TransactionCondition.t option }
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end

module TransactionInformation = struct
  type t =
    { block_hash: Digest.t option [@key "blockHash"]
    ; block_number: Revision.t option [@key "blockNumber"]
    ; from: Address.t
    ; gas: TokenAmount.t
    ; gas_price: TokenAmount.t [@key "gasPrice"]
    ; hash: Digest.t
    ; input: Yojsoning.Bytes.t
    ; nonce: Nonce.t
    ; to_: Address.t option [@key "to"]
    ; transaction_index: Revision.t option [@key "transactionIndex"]
    ; value: TokenAmount.t
    ; v: string option
    ; r: string option
    ; s: string option }
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end

module SignedTransaction = struct
  type t =
    { raw: Data.t
    ; tx: TransactionInformation.t }
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end

module LogObject = struct
  [@warning "-39"]
  type t =
    { removed: bool
    ; logIndex: Revision.t option
    ; transactionIndex: Revision.t option
    ; transactionHash: Digest.t option
    ; blockNumber: Revision.t option
    ; blockHash: Digest.t option
    ; address: Address.t
    ; data: Yojsoning.Bytes.t
    ; topics: Digest.t list }
  [@@deriving yojson]
  include (Yojsonable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (YojsonableS with type t := t))
end

module Bloom = struct
  include Yojsoning.Bytes (* TODO: Actually always 256 bytes *)
end

module TransactionReceipt = struct
  [@warning "-39"]
  type t =
    { blockHash: Digest.t
    ; blockNumber: Revision.t
    ; contractAddress: Address.t option
    ; cumulativeGasUsed: TokenAmount.t
    ; from: Address.t
    ; to_: Address.t option [@key "to"]
    ; gasUsed: TokenAmount.t
    ; logs: LogObject.t list
    ; logsBloom: Bloom.t
    ; root: Digest.t option [@default None]
    ; status: TokenAmount.t option [@default None]
    ; transactionHash: Digest.t
    ; transactionIndex: Revision.t }
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end

let transaction_to_parameters Transaction.{tx_header = { sender; gas_limit; gas_price; value }; operation } =
  let (to_, data) = match operation with
    | Operation.TransferTokens recipient -> (Some recipient, None)
    | Operation.CreateContract code -> (None, Some code)
    | Operation.CallFunction (recipient, data) -> (Some recipient, Some data) in
  TransactionParameters.
    { from= sender; to_; gas= Some gas_limit; gas_price = Some gas_price; value = Some value;
      data ; nonce= None; condition= None }

let eth_accounts =
  ethereum_json_rpc "eth_accounts"
    (list_of_yojson_exn Address.of_yojson_exn)
    yojson_noargs

let eth_estimate_gas =
  ethereum_json_rpc "eth_estimateGas"
    TokenAmount.of_yojson_exn
    (yojson_2args TransactionParameters.to_yojson BlockParameter.to_yojson)

let eth_get_balance =
  ethereum_json_rpc "eth_getBalance"
    TokenAmount.of_yojson_exn
    (yojson_2args Address.to_yojson BlockParameter.to_yojson)

let eth_get_transaction_by_hash =
  ethereum_json_rpc "eth_getTransactionByHash"
    TransactionInformation.of_yojson_exn
    (yojson_1arg Digest.to_yojson)

let eth_get_transaction_count =
  ethereum_json_rpc "eth_getTransactionCount"
    Nonce.of_yojson_exn
    (yojson_2args Address.to_yojson BlockParameter.to_yojson)

let eth_get_transaction_receipt =
  ethereum_json_rpc "eth_getTransactionReceipt"
    (option_of_yojson_exn TransactionReceipt.of_yojson_exn)
    (yojson_1arg Digest.to_yojson)

let eth_send_raw_transaction =
  ethereum_json_rpc "eth_sendRawTransaction"
    Digest.of_yojson_exn
    (yojson_1arg Data.to_yojson)

let eth_send_transaction =
  ethereum_json_rpc "eth_sendTransaction"
    Digest.of_yojson_exn
    (yojson_1arg TransactionParameters.to_yojson)

let eth_sign =
  ethereum_json_rpc "eth_sign"
    Data.of_yojson_exn
    (yojson_2args Address.to_yojson Data.to_yojson)

let eth_sign_transaction =
  ethereum_json_rpc "eth_signTransaction"
    SignedTransaction.of_yojson_exn
    (yojson_1arg TransactionParameters.to_yojson)

let eth_block_number =
  ethereum_json_rpc "eth_blockNumber"
    Revision.of_yojson_exn
    yojson_0args

(* Geth-specific methods, should only be used in tests *)

let personal_import_raw_key =
  ethereum_json_rpc "personal_importRawKey"
    Address.of_yojson_exn
    (yojson_2args
       (PrivateKey.marshal_string >> Hex.unparse_hex_string >> StringT.to_yojson)
       StringT.to_yojson)

let personal_list_accounts =
  ethereum_json_rpc "personal_listAccounts"
    (list_of_yojson_exn Address.of_yojson_exn)
    yojson_noargs

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

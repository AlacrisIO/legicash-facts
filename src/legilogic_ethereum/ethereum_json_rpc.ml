open Legilogic_lib
open Lib
open Yojsoning
open Persisting
open Json_rpc
open Signing
open Types

open Ethereum_chain

type ethereum_rpc_config =
  { scheme : string
  ; host : string
  ; port : int }
[@@deriving of_yojson]

let ethereum_rpc_config =
  lazy
    (let config_file = Config.get_config_filename "ethereum_config.json" in
     match yojson_of_file config_file
           |> ethereum_rpc_config_of_yojson with
     | Ok config -> config
     | Error msg -> bork "Error loading Ethereum JSON RPC configuration: %s" msg)

(** Network parameters for geth or other node on localhost *)
let ethereum_net =
  lazy
    (let lazy { scheme; host; port } = ethereum_rpc_config in
     Uri.make ~scheme ~host ~port ())

(* Use a mutex for access to geth, not to overload it and get timeouts.
   TODO: have a pool of a small number of connections to geth rather than just one.
*)
let ethereum_mutex = Lwt_mutex.create ()

let ethereum_json_rpc
      method_name result_decoder param_encoder ?timeout ?log params =
  Lwt_mutex.with_lock ethereum_mutex
    (fun () ->
       json_rpc (Lazy.force ethereum_net) method_name result_decoder param_encoder ?timeout ?log params)

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
    ; to_: Address.t option [@key "to"] [@default None]
    ; gas: TokenAmount.t option [@default None]
    ; gas_price: TokenAmount.t option [@key "gasPrice"] [@default None]
    ; value: TokenAmount.t option [@default None]
    ; data: Yojsoning.Bytes.t option [@default None]
    ; nonce: Nonce.t option [@default None]
    ; condition: TransactionCondition.t option [@default None] }
  [@@deriving yojson {strict = false}]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end

module TransactionInformation = struct
  type t =
    { hash: Digest.t
    ; nonce: Nonce.t
    ; block_hash: Digest.t option [@key "blockHash"] [@default None]
    ; block_number: Revision.t option [@key "blockNumber"] [@default None]
    ; transaction_index: Revision.t option [@key "transactionIndex"] [@default None]
    ; from: Address.t option [@default None]
    ; to_: Address.t option [@key "to"] [@default None]
    ; value: TokenAmount.t
    ; gas_price: TokenAmount.t [@key "gasPrice"]
    ; gas: TokenAmount.t
    ; input: Yojsoning.Bytes.t
    ; v: Quantity.t option [@default None]
    ; standard_v: Quantity.t option [@default None]
    ; r: Quantity.t option [@default None]
    ; raw: Data.t option [@default None]
    ; public_key: PublicKey.t option [@key "publicKey"] [@default None]
    ; network_id: Quantity.t option [@key "networkID"] [@default None]
    ; creates: Digest.t option [@default None]
    ; condition: yojson option [@default None] }
  [@@deriving yojson {strict = false}, show]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end

module SignedTransaction = struct
  type t =
    { raw: Data.t
    ; tx: TransactionInformation.t }
  [@@deriving yojson {strict = false}, show]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end

module LogObject = struct
  [@warning "-39"]
  type t =
    { removed: bool
    ; logIndex: Revision.t option [@default None]
    ; transactionIndex: Revision.t option [@default None]
    ; transactionHash: Digest.t option [@default None]
    ; blockNumber: Revision.t option [@default None]
    ; blockHash: Digest.t option [@default None]
    ; address: Address.t
    ; data: Yojsoning.Bytes.t
    ; topics: Digest.t list }
  [@@deriving yojson {strict = false}]
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
    { block_hash: Digest.t [@key "blockHash"]
    ; block_number: Revision.t [@key "blockNumber"]
    ; contract_address: Address.t option [@key "contractAddress"] [@default None]
    ; cumulative_gas_used: TokenAmount.t [@key "cumulativeGasUsed"]
    ; from: Address.t
    ; to_: Address.t option [@key "to"] [@default None]
    ; gas_used: TokenAmount.t [@key "gasUsed"]
    ; logs: LogObject.t list
    ; logs_bloom: Bloom.t [@key "logsBloom"]
    ; root: Digest.t option [@default None] [@default None]
    ; status: TokenAmount.t option [@default None] [@default None]
    ; transaction_hash: Digest.t [@key "transactionHash"]
    ; transaction_index: Revision.t [@key "transactionIndex"] }
  [@@deriving yojson {strict = false}]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end

let transaction_to_parameters Transaction.{tx_header = { sender; nonce; gas_limit; gas_price; value }; operation } =
  let (to_, data) = match operation with
    | Operation.TransferTokens recipient -> (Some recipient, None)
    | Operation.CreateContract code -> (None, Some code)
    | Operation.CallFunction (recipient, data) -> (Some recipient, Some data) in
  TransactionParameters.
    { from= sender; to_; gas= Some gas_limit; gas_price = Some gas_price; value = Some value;
      data ; nonce= Some nonce; condition= None }

let eth_accounts =
  ethereum_json_rpc "eth_accounts"
    (list_of_yojson_exn Address.of_yojson_exn)
    yojson_noargs

let eth_estimate_gas =
  ethereum_json_rpc "eth_estimateGas"
    TokenAmount.of_yojson_exn
    (yojson_2args TransactionParameters.to_yojson BlockParameter.to_yojson)

let eth_gas_price =
  ethereum_json_rpc "eth_gasPrice"
    TokenAmount.of_yojson_exn
    yojson_noargs
(** Get the current gas price in wei *)

let eth_get_balance =
  ethereum_json_rpc "eth_getBalance"
    TokenAmount.of_yojson_exn
    (yojson_2args Address.to_yojson BlockParameter.to_yojson)

let eth_get_code =
  ethereum_json_rpc "eth_getCode"
    Yojsoning.Bytes.of_yojson_exn
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

let personal_lock_account =
  ethereum_json_rpc "personal_lockAccount"
    YoJson.to_bool
    (yojson_1arg Address.to_yojson)

let personal_new_account =
  ethereum_json_rpc "personal_newAccount"
    Address.of_yojson_exn
    (yojson_1arg StringT.to_yojson)

let personal_sign_transaction =
  ethereum_json_rpc "personal_signTransaction"
    SignedTransaction.of_yojson_exn
    (yojson_2args TransactionParameters.to_yojson StringT.to_yojson)

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

  let%test "parse_signed_signature" =
    let st = "{\"raw\":\"0xf8c90302830f4240940000000000000000000000000000000000000000820404b864cf2c52cb000000000000000000000000f47408143d327e4bc6a87ef4a70a4e0af09b9a1c00000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000000820a96a0f6683d2489560376326818813d4d2aac304feba152111c75d1a192c5b2660493a052660483b5855f5f2ca61c24682869702d3ed0c5b838eb1b7ed36c804221ed43\",\"tx\":{\"nonce\":\"0x3\",\"gasPrice\":\"0x2\",\"gas\":\"0xf4240\",\"to\":\"0x0000000000000000000000000000000000000000\",\"value\":\"0x404\",\"input\":\"0xcf2c52cb000000000000000000000000f47408143d327e4bc6a87ef4a70a4e0af09b9a1c00000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000000\",\"v\":\"0xa96\",\"r\":\"0xf6683d2489560376326818813d4d2aac304feba152111c75d1a192c5b2660493\",\"s\":\"0x52660483b5855f5f2ca61c24682869702d3ed0c5b838eb1b7ed36c804221ed43\",\"hash\":\"0xc34293fefd30282a189cce127a3636e2076b0fdf843bcf10361b0784061db2cf\"}}" |> yojson_of_string |> SignedTransaction.of_yojson_exn in
    String.get st.SignedTransaction.raw 0 = '\xf8'
end

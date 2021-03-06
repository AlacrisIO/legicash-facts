open Legilogic_lib
open Lib
open Action
open Yojsoning
open Persisting
open Json_rpc
open Signing
open Types

open Ethereum_chain
open Ethereum_config

(* Use a mutex for access to geth, not to overload it and get timeouts.
   TODO: have a pool of a small number of connections to geth rather than just one.
*)
let ethereum_mutex = Lwt_mutex.create ()

let ethereum_json_rpc
      method_name result_decoder param_encoder ?timeout
      ?log params =
  if !rpc_log then
    Logging.log "ETH json rpc method_name=%s" method_name;
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
    else Block_number (Revision.of_0x s)
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

  let of_operation sender operation value =
    let (to_, data) = match operation with
      | Operation.TransferTokens recipient -> (Some recipient, None)
      | Operation.CreateContract code -> (None, Some code)
      | Operation.CallFunction (recipient, data) -> (Some recipient, Some data) in
    { from= sender; to_; gas= None; gas_price = None; value = Some value; data ; nonce= None; condition= None }

  let of_pre_transaction sender PreTransaction.{operation; gas_limit; value} =
    {(of_operation sender operation value) with gas = Some gas_limit; value = Some value}

  let of_transaction Transaction.{tx_header = { sender; nonce; gas_limit; gas_price; value }; operation } =
    {(of_operation sender operation value) with
      gas= Some gas_limit; gas_price = Some gas_price; value = Some value; nonce= Some nonce }
end


(*
module TransactionResult = struct
  type t =
    { hash: Digest.t
    ; nonce: Nonce.t
    ; block_hash: Digest.t option [@key "blockHash"] [@default None]
    ; block_number: Revision.t option [@key "blockNumber"] [@default None]
    ; transaction_index: Revision.t option [@key "transactionIndex"] [@default None]
    ; from: Address.t [@default None]
    ; to_: Address.t option [@key "to"] [@default None]
    ; value: TokenAmount.t
    ; gas_price: TokenAmount.t [@key "gasPrice"]
    ; gas: TokenAmount.t
    ; input: Yojsoning.Bytes.t }
  [@@deriving yojson {strict = false}, show]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end
 *)


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
    ; r: UInt256.t option [@default None]
    ; s: UInt256.t option [@default None]
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

module EthObject = struct
  type t =
    { from_block: BlockParameter.t option [@key "fromBlock"]
    ; to_block: BlockParameter.t option [@key "toBlock"]
    ; address : Address.t option [@default None]
    ; topics : Yojsoning.Bytes.t option list option [@default None]
    ; blockhash : Digest.t option [@default None]
    } [@@deriving yojson {strict = false}]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end

module ParitySignedTransaction = struct
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
    ; status: TokenAmount.t
    ; transaction_hash: Digest.t [@key "transactionHash"]
    ; transaction_index: Revision.t [@key "transactionIndex"] }
  [@@deriving yojson {strict = false}]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
  let to_confirmation
        { transaction_hash ; transaction_index ; block_number ; block_hash ; status } =
    if TokenAmount.sign status = 0 then
      Lib.bork "receipt indicates transaction %s failed" (Digest.to_0x transaction_hash)
    else
      Confirmation.{ transaction_hash ; transaction_index ; block_number ; block_hash }
end

module EthListLogObjects = struct
  [@warning "-39"]
  type t = LogObject.t list
  [@@deriving yojson {strict = false}]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end

module TxPoolContent = struct
  type entry =
    { block_hash:        Digest.t          [@key "blockHash"]
    ; block_number:      Revision.t option [@key "blockNumber"]
    ; from:              Address.t         [@key "from"]
    ; gas:               TokenAmount.t     [@key "gas"]
    ; gas_price:         TokenAmount.t     [@key "gasPrice"]
    ; hash:              Digest.t          [@key "hash"]
    ; input:             Yojsoning.Bytes.t [@key "input"]
    ; nonce:             Nonce.t           [@key "nonce"]
    ; to_:               Address.t         [@key "to"]
    ; transaction_index: Revision.t option [@key "transactionIndex"]
    ; value:             TokenAmount.t     [@key "value"]
    } [@@deriving yojson {strict=false; exn=true}]

  type t =
    { pending: (Address.t * (Nonce.t * entry list) list) list
    ; queued:  (Address.t * (Nonce.t * entry list) list) list
    }

  let to_nonces ds = `Assoc (flip List.map ds @@ fun (n, es) ->
    (Nonce.to_string n, `List (List.map entry_to_yojson es)))

  let to_addrs ds = `Assoc (flip List.map ds @@ fun (a, ns) ->
    (Address.to_0x a, to_nonces ns))

  let (=!!) yo e = Yojson.json_error
    @@ "Expected " ^ e ^ " but got: " ^ (Yojson.Safe.pretty_to_string yo)

  let of_nonces ds =
    let f (n, es) = match es with
      | `List es' -> (Nonce.of_string n, List.map entry_of_yojson_exn es')
      | _         -> es =!! "`List of entries"
    in List.map f ds

  let of_addrs ds =
    let f (a, ns) = match ns with
      | `Assoc ns' -> (Address.of_0x a, of_nonces ns')
      | _          -> ns =!! "`Assoc of nonces: `List of entries"
    in List.map f ds

  (* NB Unfortunately we have to perform conversions manually instead of
   * deriving an encoder + decoder because nonces and account addresses are
   * used for some record keys (rather than static labels) *)
  let to_yojson tpc = `Assoc [ ("pending", to_addrs tpc.pending)
                             ; ("queued",  to_addrs tpc.queued) ]

  let of_yojson_exn d =
    match d with
      | `Assoc [("pending", `Assoc p); ("queued",  `Assoc q)]
          -> { pending=of_addrs p; queued=of_addrs q }
      | _ -> d =!! "`Assoc of (pending: `Assoc p) and (queued: `Assoc q)"

  let of_yojson d =
    try Ok (of_yojson_exn d)
    with | Yojson.Json_error e -> Error e
         | ex                  -> Error (Printexc.to_string ex)
end

let eth_accounts =
  ethereum_json_rpc "eth_accounts"
    (list_of_yojson_exn Address.of_yojson_exn)
    yojson_noargs
(** Returns a list of address owned by the client *)


module CallParameters = struct
  type t =
    { from: Address.t
    ; to_: Address.t option [@key "to"] [@default None]
    ; gas: TokenAmount.t option [@default None]
    ; gas_price: TokenAmount.t option [@key "gasPrice"] [@default None]
    ; value: TokenAmount.t option [@default None]
    ; data: Yojsoning.Bytes.t option [@default None] }
  [@@deriving yojson {strict = false}]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))

  let of_operation sender operation =
    let (to_, data) = match operation with
      | Operation.TransferTokens recipient -> (Some recipient, None)
      | Operation.CreateContract code -> (None, Some code)
      | Operation.CallFunction (recipient, data) -> (Some recipient, Some data) in
    { from= sender; to_; gas= None; gas_price = None; value = None; data }

  let of_pre_transaction sender PreTransaction.{operation; gas_limit; value} =
    {(of_operation sender operation) with gas = Some gas_limit; value = Some value}

  let of_transaction Transaction.{tx_header = { sender; gas_limit; gas_price; value }; operation } =
    {(of_operation sender operation) with
      gas= Some gas_limit; gas_price = Some gas_price; value = Some value }
end

let eth_call =
  ethereum_json_rpc "eth_call"
    Data.of_yojson_exn
    (yojson_2args CallParameters.to_yojson BlockParameter.to_yojson)

let eth_chain_id =
  ethereum_json_rpc "eth_chainId"
    (option_of_yojson_exn UInt256.of_yojson_exn)
    yojson_noargs

let this_chain_id = lazy (Lwt_exn.run eth_chain_id ())

let v_of_chain_id =
  Option.defaulting (fun () -> Option.defaulting (konstant UInt256.zero) (Lazy.force this_chain_id))

module SignedTx = struct
  type t =
    { nonce : Revision.t
    ; gas_price : TokenAmount.t [@key "gasPrice"]
    ; gas : TokenAmount.t
    ; to_ : Address.t option [@key "to"] [@default None]
    ; value : TokenAmount.t
    ; input : Data.t
    ; v : UInt256.t option [@default None]
    ; r : UInt256.t option [@default None]
    ; s : UInt256.t option [@default None]
    ; hash : Digest.t }
  [@@deriving yojson {strict = false}, show]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end

module SignedTransaction = struct
  type t =
    { raw: Data.t
    ; tx: SignedTx.t }
  [@@deriving yojson {strict = false}, show]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end

let transaction_data_of_signed_transaction = function
    | SignedTransaction.{tx={nonce; gas_price; gas; to_ ; value ; input ; v ; r ; s }} ->
       SignedTransactionData.{
           nonce
         ; gas_price
         ; gas_limit = gas
         ; to_address = Option.defaulting (konstant Address.zero) to_
         ; value
         ; data = input
         ; v = v_of_chain_id v
         ; r = Option.defaulting (konstant UInt256.zero) r
         ; s = Option.defaulting (konstant UInt256.zero) s } (* TODO: is this the correct? Probably not *)


let eth_estimate_gas =
  ethereum_json_rpc "eth_estimateGas"
    TokenAmount.of_yojson_exn
    (yojson_1arg TransactionParameters.to_yojson)
(** Returns estimate of gas needed for transaction *)

let eth_gas_price =
  ethereum_json_rpc "eth_gasPrice"
    TokenAmount.of_yojson_exn
    yojson_noargs
(** Get the current gas price in wei *)

let eth_get_balance =
  ethereum_json_rpc "eth_getBalance"
    TokenAmount.of_yojson_exn
    (yojson_2args Address.to_yojson BlockParameter.to_yojson)
(** Returns the balance of the account of given address (and block) *)

let eth_get_code =
  ethereum_json_rpc "eth_getCode"
    Yojsoning.Bytes.of_yojson_exn
    (yojson_2args Address.to_yojson BlockParameter.to_yojson)
(** Returns the code of given address (and block) *)

(*
let eth_get_transaction =
  ethereum_json_rpc "eth_getTransaction"
    TransactionResult.of_yojson_exn
    (yojson_1arg Digest.to_yojson) *)
(** Returns a transaction (big object) by the hash code *)

let eth_get_transaction_by_hash =
  ethereum_json_rpc "eth_getTransactionByHash"
    TransactionInformation.of_yojson_exn
    (yojson_1arg Digest.to_yojson)
(** Returns a transaction (big object) by the hash code *)

let eth_get_transaction_count =
  ethereum_json_rpc "eth_getTransactionCount"
    Nonce.of_yojson_exn
    (yojson_2args Address.to_yojson BlockParameter.to_yojson)
(** Returns the number of transaction at address (and transaction) *)

let eth_get_transaction_receipt =
  ethereum_json_rpc "eth_getTransactionReceipt"
    (option_of_yojson_exn TransactionReceipt.of_yojson_exn)
    (yojson_1arg Digest.to_yojson)
(** Returns a receipt of transaction by transaction hash (not available if transaction still pending) *)

let eth_send_raw_transaction =
  ethereum_json_rpc "eth_sendRawTransaction"
    Digest.of_yojson_exn
    (yojson_1arg Data.to_yojson)
(** Create new message call transaction or a contract creation for signed transaction *)

(* Check that it is coherent *)
let eth_get_logs =
  ethereum_json_rpc "eth_getLogs"
    EthListLogObjects.of_yojson_exn
    (yojson_1arg EthObject.to_yojson)
(** Get a list of matchings blocks *)

(* Not used in the code *)
let eth_send_transaction =
  ethereum_json_rpc "eth_sendTransaction"
    Digest.of_yojson_exn
    (yojson_1arg TransactionParameters.to_yojson)
(** Creates new message call transaction or a contract creation if the datafield contains code *)

let eth_sign =
  ethereum_json_rpc "eth_sign"
    Data.of_yojson_exn
    (yojson_2args Address.to_yojson Data.to_yojson)
(** Computes an eth signature *)

let eth_sign_transaction =
  ethereum_json_rpc "eth_signTransaction"
    ParitySignedTransaction.of_yojson_exn
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

(* https://github.com/ethereum/go-ethereum/wiki/Management-APIs#txpool_content *)
let txpool_content =
  ethereum_json_rpc "txpool_content"
    TxPoolContent.of_yojson_exn
    yojson_noargs

module Test = struct
  open Lwt_exn

  let%test "eth_latest_block get the current latest block" =
    (* Just checks that the block number is non-negative *)
    run (eth_block_number ~timeout:1.0 ~log:false >>> (Revision.sign >> (<=) 0 >> return)) ()

  let%test "parse_signed_signature" =
    let st = "{\"raw\":\"0xf8c90302830f4240940000000000000000000000000000000000000000820404b864cf2c52cb000000000000000000000000f47408143d327e4bc6a87ef4a70a4e0af09b9a1c00000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000000820a96a0f6683d2489560376326818813d4d2aac304feba152111c75d1a192c5b2660493a052660483b5855f5f2ca61c24682869702d3ed0c5b838eb1b7ed36c804221ed43\",\"tx\":{\"nonce\":\"0x3\",\"gasPrice\":\"0x2\",\"gas\":\"0xf4240\",\"to\":\"0x0000000000000000000000000000000000000000\",\"value\":\"0x404\",\"input\":\"0xcf2c52cb000000000000000000000000f47408143d327e4bc6a87ef4a70a4e0af09b9a1c00000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000000\",\"v\":\"0xa96\",\"r\":\"0xf6683d2489560376326818813d4d2aac304feba152111c75d1a192c5b2660493\",\"s\":\"0x52660483b5855f5f2ca61c24682869702d3ed0c5b838eb1b7ed36c804221ed43\",\"hash\":\"0xc34293fefd30282a189cce127a3636e2076b0fdf843bcf10361b0784061db2cf\"}}" |> yojson_of_string |> SignedTransaction.of_yojson_exn in
    String.get st.SignedTransaction.raw 0 = '\xf8'

  let%test "txpool_content round-trip decode/encode/decode succeeds" =
    let open Test_txpool_content

    in let a = example_valid_response
      |> Yojson.Safe.from_string
      |> TxPoolContent.of_yojson_exn

    in let b = TxPoolContent.to_yojson a
      |> Yojson.Safe.to_string
      |> Yojson.Safe.from_string
      |> TxPoolContent.of_yojson

    in match b with
      | Ok b' -> a = b'
      | _     -> false

  let%test "txpool_content null `block_hash` field yields failed decode" =
    let open Test_txpool_content

    in let a = example_invalid_response_null_block_hash
      |> Yojson.Safe.from_string
      |> TxPoolContent.of_yojson

    in match a with
      (* We want to assert failure but we'll disregard the text payload since
       * we don't own (nor care too much about) how `ppx_yojson` reports
       * malformed data *)
      | Error _ -> true
      | _       -> false

  let%test "txpool_content missing `pending` + `queued` fields yields failed decode" =
    let invalid = Yojson.Safe.from_string {| { "foo": [1, 2, 3], "bar": "baz" } |}

    in match TxPoolContent.of_yojson invalid with
      | Ok _    -> false
      | Error e ->
        e = "Expected `Assoc of (pending: `Assoc p) and (queued: `Assoc q) but got: "
          ^ (Yojson.Safe.pretty_to_string invalid)

  let%test "txpool_content malformed nonces yield failed decode" =
    let open Test_txpool_content
    in let invalid = Yojson.Safe.from_string
      example_invalid_response_malformed_nonces

    in match TxPoolContent.of_yojson invalid with
      | Ok _    -> false
      | Error e ->
          e = {|Expected `Assoc of nonces: `List of entries but got: [ { "foo": null } ]|}

  let%test "txpool_content malformed nonces list yields failed decode" =
    let open Test_txpool_content
    in let invalid = Yojson.Safe.from_string
      example_invalid_response_malformed_nonces_list

    in match TxPoolContent.of_yojson invalid with
      | Ok _    -> false
      | Error e -> e = {|Expected `List of entries but got: { "foo": null }|}
end

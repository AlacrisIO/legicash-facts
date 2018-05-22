open Legibase
open Lib
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson
open Lens.Infix
module TokenAmount = Main_chain.TokenAmount

let ethereum_net = Uri.make ~scheme:"http" ~host:"localhost" ~port:8080 ()

type ethereum_rpc_call =
  (* DApps methods, use anywhere *)
  | Eth_getBalance
  | Eth_sendTransaction
  | Eth_getTransactionReceipt
  (* Geth-specific methods, should only be used in tests *)
  | Personal_unlockAccount
[@@deriving show]

(* global state *)
let id_counter = ref 1

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

let json_rpc_version = "2.0"

let build_json_rpc_call call params =
  `Assoc
    [ ("jsonrpc", `String json_rpc_version)
    ; ("method", `String (json_rpc_callname call))
    ; ("params", `List params)
    ; ("id", `Int !id_counter) ]

let build_transfer_tokens_json transaction =
  let tx_header = transaction.Main_chain.tx_header in
  let sender = tx_header.sender in
  let recipient =
    match transaction.operation with
    | Main_chain.TransferTokens recipient -> recipient
    | _ -> raise (Internal_error "Expected TransferTokens operation")
  in
  let gas = tx_header.gas_limit in
  let gas_price = tx_header.gas_price in
  let value = tx_header.value in
  let params =
    `Assoc
      [ ("from", `String (Ethereum_util.hex_string_of_string (Address.to_string sender)))
      ; ("to", `String (Ethereum_util.hex_string_of_string (Address.to_string recipient)))
      ; ("gas", `String (Printf.sprintf "0x%Lx" (TokenAmount.to_int64 gas)))
      ; ("gasPrice", `String (Printf.sprintf "0x%Lx" (TokenAmount.to_int64 gas_price)))
      ; ("value", `String (Printf.sprintf "0x%Lx" (TokenAmount.to_int64 value))) ]
  in
  build_json_rpc_call Eth_sendTransaction [params]

let build_create_contract_json transaction =
  let tx_header = transaction.Main_chain.tx_header in
  let sender = tx_header.sender in
  let code =
    match transaction.operation with
    | Main_chain.CreateContract code -> Ethereum_util.hex_string_of_string (Bytes.to_string code)
    | _ -> raise (Internal_error "Expected CreateContract operation")
  in
  let gas = tx_header.gas_limit in
  let gas_price = tx_header.gas_price in
  let contract_endowment = tx_header.value in
  let params =
    `Assoc
      [ ("from", `String (Ethereum_util.hex_string_of_string (Address.to_string sender)))
      ; ("gas", `String (Printf.sprintf "0x%Lx" (TokenAmount.to_int64 gas)))
      ; ("gasPrice", `String (Printf.sprintf "0x%Lx" (TokenAmount.to_int64 gas_price)))
      ; ("value", `String (Printf.sprintf "0x%Lx" (TokenAmount.to_int64 contract_endowment)))
      ; ("data", `String code) ]
  in
  build_json_rpc_call Eth_sendTransaction [params]

let build_call_function_json transaction =
  let tx_header = transaction.Main_chain.tx_header in
  let sender = tx_header.sender in
  let contract_address, call_hash =
    match transaction.operation with
    | Main_chain.CallFunction (contract_address, call_hash) -> (contract_address, call_hash)
    | _ -> raise (Internal_error "Expected CallFunction operation")
  in
  let gas = tx_header.gas_limit in
  let gas_price = tx_header.gas_price in
  let params =
    `Assoc
      [ ("from", `String (Ethereum_util.hex_string_of_string (Address.to_string sender)))
      ; ("to", `String (Ethereum_util.hex_string_of_string (Address.to_string contract_address)))
      ; ("gas", `String (Printf.sprintf "0x%Lx" (TokenAmount.to_int64 gas)))
      ; ("gasPrice", `String (Printf.sprintf "0x%Lx" (TokenAmount.to_int64 gas_price)))
      ; ("data", `String (Ethereum_util.hex_string_of_string (Bytes.to_string call_hash))) ]
  in
  build_json_rpc_call Eth_sendTransaction [params]

let build_transaction_json transaction =
  let open Main_chain in
  match transaction.operation with
  | TransferTokens _ -> build_transfer_tokens_json transaction
  | CreateContract _ -> build_create_contract_json transaction
  | CallFunction _ -> build_call_function_json transaction

let send_rpc_call_to_net json =
  let json_str = Yojson.to_string json in
  Client.post
    ~body:(Cohttp_lwt__.Body.of_string json_str)
    ~headers:(Cohttp.Header.add (Cohttp.Header.init ()) "Content-Type" "application/json")
    ethereum_net
  >>= fun (resp, body) ->
  let _ = resp |> Response.status |> Code.code_of_status in
  body |> Cohttp_lwt.Body.to_string >|= fun s -> Yojson.Basic.from_string s

let send_transaction_to_net transaction =
  let json = build_transaction_json transaction in
  send_rpc_call_to_net json

let send_balance_request_to_net address =
  let params =
    [`String (Ethereum_util.hex_string_of_string (Address.to_string address)); `String "latest"]
  in
  let json = build_json_rpc_call Eth_getBalance params in
  send_rpc_call_to_net json

let get_transaction_receipt transaction_hash =
  let params = [`String transaction_hash] in
  let json = build_json_rpc_call Eth_getTransactionReceipt params in
  send_rpc_call_to_net json

let transaction_executed transaction_hash =
  let rpc_call = get_transaction_receipt transaction_hash in
  let receipt_json = Lwt_main.run rpc_call in
  let keys = Yojson.Basic.Util.keys receipt_json in
  not (List.mem "error" keys) && List.mem "blockHash" keys && List.mem "blockNumber" keys

module Test = struct
  (* for testing only; all test accounts have empty password *)

  let unlock_account address =
    let params =
      [`String (Ethereum_util.hex_string_of_string (Address.to_string address)); `String ""; `Int 5]
    in
    let json = build_json_rpc_call Personal_unlockAccount params in
    send_rpc_call_to_net json

  let json_contains_error json =
    match Yojson.Basic.Util.member "error" json with `Null -> false | _ -> true

  let json_result_to_int json =
    int_of_string (Yojson.Basic.Util.to_string (Yojson.Basic.Util.member "result" json))

  let get_nonce =
    let test_nonce = ref 0 in
    fun () ->
      let nonce = Main_chain.Nonce.of_int !test_nonce in
      incr test_nonce ; nonce

  let%test "transfer-on-Ethereum-testnet" =
    let open Main_chain in
    (* accounts on test net *)
    let sender_hex = "0xebd008d25ace6b456b5b6bda8175d534d4788e9f" in
    let sender_address = Address.of_string (Ethereum_util.string_of_hex_string sender_hex) in
    let recipient_hex = "0xa07275f5125079d7c4c85a116a19292413045b5c" in
    let recipient_address = Address.of_string (Ethereum_util.string_of_hex_string recipient_hex) in
    (* unlock accounts *)
    let unlock_sender_json = Lwt_main.run (unlock_account sender_address) in
    assert (not (json_contains_error unlock_sender_json)) ;
    let unlock_recipient_json = Lwt_main.run (unlock_account recipient_address) in
    assert (not (json_contains_error unlock_recipient_json)) ;
    (* get opening balance *)
    let transfer_amount = 22 in
    let sender_start_balance_json = Lwt_main.run (send_balance_request_to_net sender_address) in
    assert (not (json_contains_error sender_start_balance_json)) ;
    let sender_start_balance = json_result_to_int sender_start_balance_json in
    assert (sender_start_balance >= transfer_amount) ;
    let tx_header =
      { sender= sender_address
      ; nonce= get_nonce ()
      ; gas_price= TokenAmount.of_int 2
      ; gas_limit= TokenAmount.of_int 1000000
      ; value= TokenAmount.of_int transfer_amount }
    in
    let operation = Main_chain.TransferTokens recipient_address in
    let transaction = {tx_header; operation} in
    (* send tokens *)
    let output = Lwt_main.run (send_transaction_to_net transaction) in
    assert (not (json_contains_error output)) ;
    true

  let%test "create-contract-on-Ethereum-testnet" =
    let open Main_chain in
    (* account on test net *)
    let sender_hex = "0xcb6f085e91138ddcd14d6987318c66457e6c2918" in
    let sender_address = Address.of_string (Ethereum_util.string_of_hex_string sender_hex) in
    (* unlock accounts *)
    let unlock_sender_json = Lwt_main.run (unlock_account sender_address) in
    assert (not (json_contains_error unlock_sender_json)) ;
    let tx_header =
      { sender= sender_address
      ; nonce= get_nonce ()
      ; gas_price= TokenAmount.of_int 2
      ; gas_limit= TokenAmount.of_int 1000000
      ; value= TokenAmount.of_int 42 }
    in
    (* a valid contract contains compiled EVM code
     for testing, we just use a buffer with arbitrary contents
   *)
    let operation = Main_chain.CreateContract (Bytes.create 128) in
    let transaction = {tx_header; operation} in
    (* create contract *)
    let output = Lwt_main.run (send_transaction_to_net transaction) in
    assert (not (json_contains_error output)) ;
    true

  let%test "call-contract-on-Ethereum-testnet" =
    let open Main_chain in
    (* account on test net *)
    let sender_hex = "0x5440d0b23296809bdffad4ea481d0fe687954d41" in
    let sender_address = Address.of_string (Ethereum_util.string_of_hex_string sender_hex) in
    (* unlock accounts *)
    let unlock_sender_json = Lwt_main.run (unlock_account sender_address) in
    assert (not (json_contains_error unlock_sender_json)) ;
    (* get opening balance *)
    let tx_header =
      { sender= sender_address
      ; nonce= get_nonce ()
      ; gas_price= TokenAmount.of_int 2
      ; gas_limit= TokenAmount.of_int 1000000
      ; value= TokenAmount.zero }
    in
    (* for CallFunction:

     address should be a valid contract address
     for testing, it's a dummy address

     the bytes are a 4-byte prefix of the Keccak256 hash of the encoding of a method
     signature, followed by the encoding of the method parameters, as described at:

       https://solidity.readthedocs.io/en/develop/abi-spec.html

     This data tells the EVM which method to call, with what arguments, in the contract

     in this test, we just use a dummy hash to represent all of that
   *)
    let hashed = Digest.make "some arbitrary string" in
    let operation =
      Main_chain.CallFunction
        ( Address.of_string
            (Ethereum_util.string_of_hex_string "0x2B1c40cD23AAB27F59f7874A1F454748B004C4D8")
        , Bytes.of_string (Digest.to_string hashed) )
    in
    let transaction = {tx_header; operation} in
    let output = Lwt_main.run (send_transaction_to_net transaction) in
    assert (not (json_contains_error output)) ;
    true
end

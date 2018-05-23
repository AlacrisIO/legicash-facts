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
  | Eth_getTransactionByHash
  | Eth_getTransactionReceipt
  (* Geth-specific methods, should only be used in tests *)
  | Personal_listAccounts
  | Personal_newAccount
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
  body |> Cohttp_lwt.Body.to_string >|= fun s -> Basic.from_string s

let send_transaction_to_net signed_transaction =
  let transaction = signed_transaction.payload in
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

let get_transaction_by_hash transaction_hash =
  let params = [`String transaction_hash] in
  let json = build_json_rpc_call Eth_getTransactionByHash params in
  send_rpc_call_to_net json

let transaction_executed transaction_hash =
  let rpc_call = get_transaction_receipt transaction_hash in
  let receipt_json = Lwt_main.run rpc_call in
  let keys = Basic.Util.keys receipt_json in
  not (List.mem "error" keys)
  && List.mem "result" keys
  &&
  let result_json = Basic.Util.member "result" receipt_json in
  result_json != `Null
  &&
  let result_keys = Basic.Util.keys result_json in
  List.mem "blockHash" result_keys && List.mem "blockNumber" result_keys

let transaction_execution_matches_transaction transaction_hash
    (signed_transaction: Main_chain.transaction_signed) =
  transaction_executed transaction_hash
  &&
  let transaction_json = Lwt_main.run (get_transaction_by_hash transaction_hash) in
  let keys = Basic.Util.keys transaction_json in
  not (List.mem "error" keys)
  &&
  let transaction = signed_transaction.payload in
  let result_json = Basic.Util.member "result" transaction_json in
  (* for all operations, check these fields *)
  let get_result_json key = Basic.Util.to_string (Basic.Util.member key result_json) in
  let actual_sender = get_result_json "from" in
  (*  let actual_nonce = get_result_json "nonce" in *)
  let actual_gas_price = Ethereum_util.token_amount_of_hex_string (get_result_json "gasPrice") in
  let actual_gas = Ethereum_util.token_amount_of_hex_string (get_result_json "gas") in
  let actual_value = Ethereum_util.token_amount_of_hex_string (get_result_json "value") in
  let tx_header = transaction.tx_header in
  let expected_sender = Ethereum_util.hex_string_of_string (Address.to_string tx_header.sender) in
  (* let expected_nonce = Printf.sprintf "0x%Lx" (Main_chain.Nonce.to_int64 tx_header.nonce) in *)
  let expected_gas_limit = tx_header.gas_limit in
  let expected_gas_price = tx_header.gas_price in
  let expected_value = tx_header.value in
  (* can't compare nonces, because geth tracks nonces independently *)
  actual_sender = expected_sender
  && TokenAmount.compare actual_gas expected_gas_limit <= 0
  && TokenAmount.compare actual_gas_price expected_gas_price <= 0
  && TokenAmount.compare actual_value expected_value = 0
  &&
  (* operation-specific checks *)
  match transaction.operation with
  | TransferTokens recipient_address ->
      let actual_to = get_result_json "to" in
      let expected_to = Ethereum_util.hex_string_of_string (Address.to_string recipient_address) in
      actual_to = expected_to
  | CreateContract _ -> true
  | CallFunction (contract_address, _) ->
      let actual_to = get_result_json "to" in
      let expected_to = Ethereum_util.hex_string_of_string (Address.to_string contract_address) in
      actual_to = expected_to

module Test = struct
  open Main_chain

  let alice_keys =
    Keypair.make_keys_from_hex
      "d5:69:84:dc:08:3d:76:97:01:71:4e:eb:1d:4c:47:a4:54:25:5a:3b:bc:3e:9f:44:84:20:8c:52:bd:a3:b6:4e"
      "04:23:a7:cd:9a:03:fa:9c:58:57:e5:14:ae:5a:cb:18:ca:91:e0:7d:69:45:3e:d8:51:36:ea:6a:00:36:10:67:b8:60:a5:b2:0f:11:53:33:3a:ef:2d:1b:a1:3b:1d:7a:52:de:28:69:d1:f6:23:71:bf:81:bf:80:3c:21:c6:7a:ca"

  let list_accounts () =
    let params = [] in
    let json = build_json_rpc_call Personal_listAccounts params in
    send_rpc_call_to_net json

  let new_account () =
    (* all test accounts have empty password *)
    let params = [`String ""] in
    let json = build_json_rpc_call Personal_newAccount params in
    send_rpc_call_to_net json

  let unlock_account address =
    let params =
      [`String (Ethereum_util.hex_string_of_string (Address.to_string address)); `String ""; `Int 5]
    in
    let json = build_json_rpc_call Personal_unlockAccount params in
    send_rpc_call_to_net json

  let json_contains_error json =
    match Basic.Util.member "error" json with `Null -> false | _ -> true

  let json_result_to_int json =
    int_of_string (Basic.Util.to_string (Basic.Util.member "result" json))

  let get_first_account () =
    let accounts_json = Lwt_main.run (list_accounts ()) in
    assert (not (json_contains_error accounts_json)) ;
    let accounts = Basic.Util.to_list (Basic.Util.member "result" accounts_json) in
    assert (not (accounts = [])) ;
    Basic.Util.to_string (List.hd accounts)

  let get_nonce =
    let test_nonce = ref 0 in
    fun () ->
      let nonce = Main_chain.Nonce.of_int !test_nonce in
      incr test_nonce ; nonce

  let wait_for_contract_execution transaction_hash =
    let counter = ref 0 in
    let max_counter = 20 in
    (* wait for transaction to appear in block *)
    while not (transaction_executed transaction_hash && !counter <= max_counter) do
      (* Printf.printf "Waiting for transaction execution...\n%!" ; *)
      Unix.sleepf 0.25 ; incr counter
    done

  let%test "transfer-on-Ethereum-testnet" =
    let sender_account = get_first_account () in
    let sender_address = Address.of_string (Ethereum_util.string_of_hex_string sender_account) in
    let new_account_json = Lwt_main.run (new_account ()) in
    assert (not (json_contains_error new_account_json)) ;
    let new_account = Basic.Util.to_string (Basic.Util.member "result" new_account_json) in
    let recipient_address = Address.of_string (Ethereum_util.string_of_hex_string new_account) in
    (* unlock accounts *)
    let unlock_sender_json = Lwt_main.run (unlock_account sender_address) in
    assert (not (json_contains_error unlock_sender_json)) ;
    let unlock_recipient_json = Lwt_main.run (unlock_account recipient_address) in
    assert (not (json_contains_error unlock_recipient_json)) ;
    (* we don't check opening balance, which may be too large to parse *)
    let transfer_amount = 22 in
    let tx_header =
      { sender= sender_address
      ; nonce= get_nonce ()
      ; gas_price= TokenAmount.of_int 2
      ; gas_limit= TokenAmount.of_int 1000000
      ; value= TokenAmount.of_int transfer_amount }
    in
    let operation = Main_chain.TransferTokens recipient_address in
    let transaction = {tx_header; operation} in
    let signed_transaction = sign alice_keys.private_key transaction in
    (* send tokens *)
    let output = Lwt_main.run (send_transaction_to_net signed_transaction) in
    assert (not (json_contains_error output)) ;
    let result_json = Basic.Util.member "result" output in
    let transaction_hash = Basic.Util.to_string result_json in
    let _ = wait_for_contract_execution transaction_hash in
    transaction_execution_matches_transaction transaction_hash signed_transaction

  let%test "create-contract-on-Ethereum-testnet" =
    let sender_account = get_first_account () in
    let sender_address = Address.of_string (Ethereum_util.string_of_hex_string sender_account) in
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
    let operation = CreateContract (Bytes.create 128) in
    let transaction = {tx_header; operation} in
    let signed_transaction = sign alice_keys.private_key transaction in
    (* create contract *)
    let output = Lwt_main.run (send_transaction_to_net signed_transaction) in
    assert (not (json_contains_error output)) ;
    let result_json = Basic.Util.member "result" output in
    let transaction_hash = Basic.Util.to_string result_json in
    let _ = wait_for_contract_execution transaction_hash in
    let signed_transaction = sign alice_keys.private_key transaction in
    transaction_execution_matches_transaction transaction_hash signed_transaction

  let%test "call-contract-on-Ethereum-testnet" =
    let open Main_chain in
    let sender_account = get_first_account () in
    let sender_address = Address.of_string (Ethereum_util.string_of_hex_string sender_account) in
    let unlock_sender_json = Lwt_main.run (unlock_account sender_address) in
    assert (not (json_contains_error unlock_sender_json)) ;
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
    let signed_transaction = sign alice_keys.private_key transaction in
    let output = Lwt_main.run (send_transaction_to_net signed_transaction) in
    assert (not (json_contains_error output)) ;
    let result_json = Basic.Util.member "result" output in
    let transaction_hash = Basic.Util.to_string result_json in
    let _ = wait_for_contract_execution transaction_hash in
    let signed_transaction = sign alice_keys.private_key transaction in
    transaction_execution_matches_transaction transaction_hash signed_transaction
end

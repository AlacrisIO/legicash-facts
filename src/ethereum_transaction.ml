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
  | Eth_getTransactionCount
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


(* params contain hex strings, but we unhex those strings when running RLP *)
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
      [ ("from", `String (Ethereum_util.hex_string_of_address sender))
      ; ("to", `String (Ethereum_util.hex_string_of_address recipient))
      ; ("gas", `String (Ethereum_util.hex_string_of_token_amount gas))
      ; ("gasPrice", `String (Ethereum_util.hex_string_of_token_amount gas_price))
      ; ("value", `String (Ethereum_util.hex_string_of_token_amount value)) ]
  in
  build_json_rpc_call Eth_sendTransaction [params]


let build_create_contract_json transaction =
  let tx_header = transaction.Main_chain.tx_header in
  if TokenAmount.compare tx_header.value TokenAmount.zero != 0 then
    raise (Internal_error "New contract must have zero value") ;
  let sender = tx_header.sender in
  let code =
    match transaction.operation with
    | Main_chain.CreateContract code -> code
    | _ -> raise (Internal_error "Expected CreateContract operation")
  in
  let gas = tx_header.gas_limit in
  let gas_price = tx_header.gas_price in
  let params =
    `Assoc
      [ ("from", `String (Ethereum_util.hex_string_of_address sender))
      ; ("gas", `String (Ethereum_util.hex_string_of_token_amount gas))
      ; ("gasPrice", `String (Ethereum_util.hex_string_of_token_amount gas_price))
      ; ("data", `String (Ethereum_util.hex_string_of_bytes code)) ]
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
  let value = tx_header.value in
  let params =
    `Assoc
      [ ("from", `String (Ethereum_util.hex_string_of_address sender))
      ; ("to", `String (Ethereum_util.hex_string_of_address contract_address))
      ; ("gas", `String (Ethereum_util.hex_string_of_token_amount gas))
      ; ("gasPrice", `String (Ethereum_util.hex_string_of_token_amount gas_price))
        (* allows transferring value to contract *)
      ; ("value", `String (Ethereum_util.hex_string_of_token_amount value))
      ; ("data", `String (Ethereum_util.hex_string_of_bytes call_hash)) ]
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


let get_transaction_count address =
  let address_hex_string = Ethereum_util.hex_string_of_string (Address.to_string address) in
  let params = [`String address_hex_string; `String "latest"] in
  let json = build_json_rpc_call Eth_getTransactionCount params in
  send_rpc_call_to_net json


let transaction_executed transaction_hash =
  let rpc_call = get_transaction_receipt transaction_hash in
  let receipt_json = Lwt_main.run rpc_call in
  let keys = Basic.Util.keys receipt_json in
  not (List.mem "error" keys) && List.mem "result" keys
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
  let actual_nonce = get_result_json "nonce" in
  let actual_gas_price = Ethereum_util.token_amount_of_hex_string (get_result_json "gasPrice") in
  let actual_gas = Ethereum_util.token_amount_of_hex_string (get_result_json "gas") in
  let actual_value = Ethereum_util.token_amount_of_hex_string (get_result_json "value") in
  let tx_header = transaction.tx_header in
  let expected_sender = Ethereum_util.hex_string_of_string (Address.to_string tx_header.sender) in
  let expected_nonce = Printf.sprintf "0x%Lx" (Main_chain.Nonce.to_int64 tx_header.nonce) in
  let expected_gas_limit = tx_header.gas_limit in
  let expected_gas_price = tx_header.gas_price in
  let expected_value = tx_header.value in
  actual_sender = expected_sender && actual_nonce = expected_nonce
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


(* convert transaction record to rlp_item suitable for encoding *)
let rlp_of_transaction transaction =
  let open Main_chain in
  let open Ethereum_util in
  let open Ethereum_rlp in
  let tx_header = transaction.tx_header in
  (* all items are strings, each character represents 2 digits in hex representation *)
  let nonce = string_of_nonce tx_header.nonce in
  let gas_price = string_of_token_amount tx_header.gas_price in
  let gas_limit = string_of_token_amount tx_header.gas_limit in
  let value = string_of_token_amount tx_header.value in
  let toaddr, data =
    match transaction.operation with
    | TransferTokens to_address -> (Address.to_string to_address, "")
    | CreateContract bytes -> ("", Bytes.to_string bytes)
    | CallFunction (contract_address, call_encoding) ->
        (Address.to_string contract_address, Bytes.to_string call_encoding)
  in
  RlpItems
    [ RlpItem nonce
    ; RlpItem gas_price
    ; RlpItem gas_limit
    ; RlpItem toaddr
    ; RlpItem value
    ; RlpItem data ]


let rlp_of_signed_transaction transaction_rlp ~v ~r ~s =
  let open Ethereum_rlp in
  let open Ethereum_util in
  let signature_items = [RlpItem v; RlpItem r; RlpItem s] in
  match transaction_rlp with
  | RlpItems items -> RlpItems (items @ signature_items)
  | _ -> raise (Internal_error "Expected RlpItems when creating signed transaction RLP")


(* https://medium.com/@codetractio/inside-an-ethereum-transaction-fa94ffca912f
   describes the transaction hashing algorithm
*)
let get_transaction_hash signed_transaction private_key =
  let open Bigarray in
  let transaction = signed_transaction.payload in
  let transaction_rlp = rlp_of_transaction transaction in
  (* step 1: RLP-encode and hash the transaction *)
  let encoded_transaction = Ethereum_rlp.encode transaction_rlp in
  let hashed_transaction = Ethereum_util.hash (Ethereum_rlp.to_string encoded_transaction) in
  (* step 2: sign the hash with a private key, extract pieces of the signature *)
  let hash_buffer = Array1.create Char c_layout (String.length hashed_transaction) in
  let _ =
    for ndx = 0 to Array1.dim hash_buffer - 1 do
      Array1.set hash_buffer ndx hashed_transaction.[ndx]
    done
  in
  let msg =
    match Secp256k1.Sign.msg_of_bytes hash_buffer with
    | Some msg -> msg
    | None -> raise (Internal_error "Couldn't get msg from hash buffer")
  in
  let signature_buffer, recid =
    match Secp256k1.Sign.sign_recoverable ~sk:private_key secp256k1_ctx msg with
    | Ok signature -> Secp256k1.Sign.to_bytes_recid secp256k1_ctx signature
    | Error err -> raise (Internal_error "Could not sign transaction hash")
  in
  let v = String.make 1 (Char.chr (recid + 27)) in
  let string_of_subarray subarray =
    String.init (Array1.dim subarray) (fun ndx -> Array1.get subarray ndx)
  in
  let r = string_of_subarray (Array1.sub signature_buffer 0 32) in
  let s = string_of_subarray (Array1.sub signature_buffer 32 32) in
  (* step 3: RLP-encode and hash the transaction augmented with the signature *)
  let signed_transaction_rlp = rlp_of_signed_transaction transaction_rlp ~v ~r ~s in
  let encoded_signed_transaction = Ethereum_rlp.encode signed_transaction_rlp in
  Ethereum_util.hash (Ethereum_rlp.to_string encoded_signed_transaction)


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


  let get_nonce address =
    let contract_count_json = Lwt_main.run (get_transaction_count address) in
    assert (not (json_contains_error contract_count_json)) ;
    let result = Basic.Util.to_string (Basic.Util.member "result" contract_count_json) in
    Nonce.of_int64 (Int64.of_string result)


  let wait_for_contract_execution transaction_hash =
    let counter = ref 0 in
    let max_counter = 20 in
    (* wait for transaction to appear in block *)
    while not (transaction_executed transaction_hash && !counter <= max_counter) do
      (* Printf.printf "Waiting for transaction execution...\n%!" ; *)
      Unix.sleepf 0.1 ; incr counter
    done


  [%%test
  let "transfer-on-Ethereum-testnet" =
    let sender_account = get_first_account () in
    let sender_address = Ethereum_util.address_of_hex_string sender_account in
    let new_account_json = Lwt_main.run (new_account ()) in
    assert (not (json_contains_error new_account_json)) ;
    let new_account = Basic.Util.to_string (Basic.Util.member "result" new_account_json) in
    let recipient_address = Ethereum_util.address_of_hex_string new_account in
    (* unlock accounts *)
    let unlock_sender_json = Lwt_main.run (unlock_account sender_address) in
    assert (not (json_contains_error unlock_sender_json)) ;
    let unlock_recipient_json = Lwt_main.run (unlock_account recipient_address) in
    assert (not (json_contains_error unlock_recipient_json)) ;
    (* we don't check opening balance, which may be too large to parse *)
    let transfer_amount = 22 in
    let tx_header =
      { sender= sender_address
      ; nonce= get_nonce sender_address
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
    transaction_execution_matches_transaction transaction_hash signed_transaction]

  [%%test
  let "create-contract-on-Ethereum-testnet" =
    let sender_account = get_first_account () in
    let sender_address = Ethereum_util.address_of_hex_string sender_account in
    let unlock_sender_json = Lwt_main.run (unlock_account sender_address) in
    assert (not (json_contains_error unlock_sender_json)) ;
    let tx_header =
      { sender= sender_address
      ; nonce= get_nonce sender_address
      ; gas_price= TokenAmount.of_int 2
      ; gas_limit= TokenAmount.of_int 1000000
      ; value= TokenAmount.zero }
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
    transaction_execution_matches_transaction transaction_hash signed_transaction]

  [%%test
  let "call-contract-on-Ethereum-testnet" =
    let open Main_chain in
    let sender_account = get_first_account () in
    let sender_address = Ethereum_util.address_of_hex_string sender_account in
    let unlock_sender_json = Lwt_main.run (unlock_account sender_address) in
    assert (not (json_contains_error unlock_sender_json)) ;
    let tx_header =
      { sender= sender_address
      ; nonce= get_nonce sender_address
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
    transaction_execution_matches_transaction transaction_hash signed_transaction]

  [%%test
  let "compute-transaction-hash" =
    (* example from https://medium.com/@codetractio/inside-an-ethereum-transaction-fa94ffca912f *)
    let open Bigarray in
    let private_key_hex = "0xc0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0de" in
    let private_key_string = Ethereum_util.string_of_hex_string private_key_hex in
    let private_key_buffer = Array1.create Char c_layout (String.length private_key_string) in
    let _ =
      for ndx = 0 to Array1.dim private_key_buffer - 1 do
        Array1.set private_key_buffer ndx private_key_string.[ndx]
      done
    in
    let private_key =
      match Secp256k1.Key.read_sk secp256k1_ctx private_key_buffer with
      | Ok private_key -> private_key
      | Error err -> raise (Internal_error err)
    in
    let sender_account = get_first_account () in
    let sender_address = Ethereum_util.address_of_hex_string sender_account in
    let tx_header =
      { sender= sender_address (* doesn't matter for transaction hash *)
      ; nonce= Nonce.zero
      ; gas_price= TokenAmount.of_int 20000000000
      ; gas_limit= TokenAmount.of_int 100000
      ; value= TokenAmount.of_int 1000 }
    in
    let operation =
      Main_chain.CallFunction
        ( Ethereum_util.address_of_hex_string "0x687422eea2cb73b5d3e242ba5456b782919afc85"
        , Bytes.of_string (Ethereum_util.string_of_hex_string "0xc0de") )
    in
    let transaction = {tx_header; operation} in
    let signed_transaction = sign private_key transaction in
    let transaction_hash = get_transaction_hash signed_transaction private_key in
    Ethereum_util.hex_string_of_string transaction_hash
    = "0x8b69a0ca303305a92d8d028704d65e4942b7ccc9a99917c8c9e940c9d57a9662"]

  [%%test
  let "hello-solidity" =
    let open Ethereum_abi in
    (* code is result of running "solc --bin hello.sol", and prepending "0x" *)
    let code =
      "0x608060405234801561001057600080fd5b506101a7806100206000396000f300608060405260043610610041576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff16806339a7aa4814610046575b600080fd5b34801561005257600080fd5b5061005b6100d6565b6040518080602001828103825283818151815260200191508051906020019080838360005b8381101561009b578082015181840152602081019050610080565b50505050905090810190601f1680156100c85780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b60607fcde5c32c0a45fd8aa4b65ea8003fc9da9acd5e2c6c24a9fcce6ab79cabbd912260405180806020018281038252600d8152602001807f48656c6c6f2c20776f726c64210000000000000000000000000000000000000081525060200191505060405180910390a16040805190810160405280600881526020017f476f6f64627965210000000000000000000000000000000000000000000000008152509050905600a165627a7a7230582024923934849b0e74a5091ac4b5c65d9b3b93d74726aff49fd5763bc136dac5c60029"
    in
    let code_string = Ethereum_util.string_of_hex_string code in
    let code_bytes = Bytes.of_string code_string in
    (* create a contract using "hello, world" EVM code *)
    let sender_account = get_first_account () in
    let sender_address = Ethereum_util.address_of_hex_string sender_account in
    let unlock_sender_json = Lwt_main.run (unlock_account sender_address) in
    assert (not (json_contains_error unlock_sender_json)) ;
    let tx_header =
      { sender= sender_address
      ; nonce= get_nonce sender_address
      ; gas_price= TokenAmount.of_int 2
      ; gas_limit= TokenAmount.of_int 1000000
      ; value= TokenAmount.zero }
    in
    (* a valid contract contains compiled EVM code
     for testing, we just use a buffer with arbitrary contents
     *)
    let operation = CreateContract code_bytes in
    let transaction = {tx_header; operation} in
    let signed_transaction = sign alice_keys.private_key transaction in
    (* create contract *)
    let output = Lwt_main.run (send_transaction_to_net signed_transaction) in
    assert (not (json_contains_error output)) ;
    let result_json = Basic.Util.member "result" output in
    let transaction_hash = Basic.Util.to_string result_json in
    let _ = wait_for_contract_execution transaction_hash in
    let rpc_call = get_transaction_receipt transaction_hash in
    let receipt_json = Lwt_main.run rpc_call in
    assert (not (json_contains_error receipt_json)) ;
    let receipt_result_json = Basic.Util.member "result" receipt_json in
    let contract_address =
      Basic.Util.to_string (Basic.Util.member "contractAddress" receipt_result_json)
    in
    let signed_transaction = sign alice_keys.private_key transaction in
    assert (transaction_execution_matches_transaction transaction_hash signed_transaction) ;
    (* call the contract we've created *)
    let tx_header1 =
      { sender= sender_address
      ; nonce= get_nonce sender_address
      ; gas_price= TokenAmount.of_int 2
      ; gas_limit= TokenAmount.of_int 1000000
      ; value= TokenAmount.zero }
    in
    let call = {function_name= "printHelloWorld"; parameters= []} in
    let call_bytes = encode_function_call call in
    let operation1 =
      Main_chain.CallFunction (Ethereum_util.address_of_hex_string contract_address, call_bytes)
    in
    let transaction1 = {tx_header= tx_header1; operation= operation1} in
    let signed_transaction1 = sign alice_keys.private_key transaction1 in
    let output1 = Lwt_main.run (send_transaction_to_net signed_transaction1) in
    assert (not (json_contains_error output1)) ;
    let result_json1 = Basic.Util.member "result" output1 in
    let transaction_hash1 = Basic.Util.to_string result_json1 in
    let _ = wait_for_contract_execution transaction_hash1 in
    let rpc_call1 = get_transaction_receipt transaction_hash1 in
    let receipt_json1 = Lwt_main.run rpc_call1 in
    (* verify that we called "printHelloWorld" *)
    let receipt_result1_json = Basic.Util.member "result" receipt_json1 in
    let log_json = List.hd (Basic.Util.to_list (Basic.Util.member "logs" receipt_result1_json)) in
    (* we called the right contract *)
    let log_contract_address = Basic.Util.to_string (Basic.Util.member "address" log_json) in
    assert (log_contract_address = contract_address) ;
    (* we called the right function within the contract *)
    let log_topics = Basic.Util.to_list (Basic.Util.member "topics" log_json) in
    assert (List.length log_topics = 1) ;
    let topic_event = Basic.Util.to_string (List.hd log_topics) in
    let hello_world = (String_value "Hello, world!", String) in
    let signature = make_signature {function_name= "showResult"; parameters= [hello_world]} in
    let signature_hash = Ethereum_util.hex_string_of_string (Ethereum_util.hash signature) in
    assert (topic_event = signature_hash) ;
    (* the log data is the encoding of the parameter passed to the event *)
    let data = Basic.Util.to_string (Basic.Util.member "data" log_json) in
    let hello_encoding =
      let tuple_value, tuple_ty = abi_tuple_of_abi_values [hello_world] in
      Ethereum_util.hex_string_of_bytes (encode_abi_value tuple_value tuple_ty)
    in
    data = hello_encoding]

  [%%test
  let "fallback-with-facilitator-address" =
    (* we call the fallback function in a contract by using the facilitator address as "code" *)
    let open Ethereum_abi in
    (* code is result of running "solc --bin facilitator-fallback.sol", and prepending "0x" *)
    let code =
      "0x608060405234801561001057600080fd5b5060e48061001f6000396000f30060806040527facfada45e09e5bb4c2c456febe99efe38be8bfc67a25cccdbb4c93ec56f661a5605f6000368080601f01602080910402602001604051908101604052809392919081815260200183838082843782019150505050505060aa565b34604051808373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018281526020019250505060405180910390a1005b6000601482015190509190505600a165627a7a723058202091e8d135ab21f370f9e632d1bd31e4a6add8179253a6a45a2a7e56ea7d3e090029"
    in
    let code_string = Ethereum_util.string_of_hex_string code in
    let code_bytes = Bytes.of_string code_string in
    (* create the contract *)
    let sender_account = get_first_account () in
    let sender_address = Ethereum_util.address_of_hex_string sender_account in
    let unlock_sender_json = Lwt_main.run (unlock_account sender_address) in
    assert (not (json_contains_error unlock_sender_json)) ;
    let tx_header =
      { sender= sender_address
      ; nonce= get_nonce sender_address
      ; gas_price= TokenAmount.of_int 42
      ; gas_limit= TokenAmount.of_int 1000000
      ; value= TokenAmount.zero }
    in
    let operation = CreateContract code_bytes in
    let transaction = {tx_header; operation} in
    let signed_transaction = sign alice_keys.private_key transaction in
    let output = Lwt_main.run (send_transaction_to_net signed_transaction) in
    assert (not (json_contains_error output)) ;
    let result_json = Basic.Util.member "result" output in
    let transaction_hash = Basic.Util.to_string result_json in
    let _ = wait_for_contract_execution transaction_hash in
    let rpc_call = get_transaction_receipt transaction_hash in
    let receipt_json = Lwt_main.run rpc_call in
    assert (not (json_contains_error receipt_json)) ;
    let receipt_result_json = Basic.Util.member "result" receipt_json in
    let contract_address =
      Basic.Util.to_string (Basic.Util.member "contractAddress" receipt_result_json)
    in
    (* check balance of new contract *)
    let starting_balance_json =
      Lwt_main.run
        (send_balance_request_to_net (Ethereum_util.address_of_hex_string contract_address))
    in
    assert (not (json_contains_error starting_balance_json)) ;
    let starting_balance =
      int_of_string (Basic.Util.to_string (Basic.Util.member "result" starting_balance_json))
    in
    assert (starting_balance = 0) ;
    let signed_transaction = sign alice_keys.private_key transaction in
    assert (transaction_execution_matches_transaction transaction_hash signed_transaction) ;
    (* call the fallback in the contract we've created *)
    let amount_to_transfer = 93490 in
    let tx_header1 =
      { sender= sender_address
      ; nonce= get_nonce sender_address
      ; gas_price= TokenAmount.of_int 2
      ; gas_limit= TokenAmount.of_int 1000000
      ; value= TokenAmount.of_int amount_to_transfer }
    in
    (* use (dummy) facilitator address as code to trigger fallback *)
    let facilitator_address =
      Ethereum_util.address_of_hex_string "0x9797809415e4b8efea0963e362ff68b9d98f9e00"
    in
    let address_bytes = Ethereum_util.bytes_of_address facilitator_address in
    let operation1 =
      Main_chain.CallFunction (Ethereum_util.address_of_hex_string contract_address, address_bytes)
    in
    let transaction1 = {tx_header= tx_header1; operation= operation1} in
    let signed_transaction1 = sign alice_keys.private_key transaction1 in
    let output1 = Lwt_main.run (send_transaction_to_net signed_transaction1) in
    assert (not (json_contains_error output1)) ;
    let result_json1 = Basic.Util.member "result" output1 in
    let transaction_hash1 = Basic.Util.to_string result_json1 in
    let _ = wait_for_contract_execution transaction_hash1 in
    let rpc_call1 = get_transaction_receipt transaction_hash1 in
    let receipt_json1 = Lwt_main.run rpc_call1 in
    (* verify that we called the fallback *)
    let receipt_result1_json = Basic.Util.member "result" receipt_json1 in
    let logs = Basic.Util.to_list (Basic.Util.member "logs" receipt_result1_json) in
    assert (List.length logs = 1) ;
    let log_json = List.hd logs in
    (* the log is for this contract *)
    let receipt_address = Basic.Util.to_string (Basic.Util.member "address" log_json) in
    assert (receipt_address = contract_address) ;
    (* we saw the expected event *)
    let topics = Basic.Util.to_list (Basic.Util.member "topics" log_json) in
    assert (List.length topics = 1) ;
    let topic = List.hd topics in
    let logged_event = Basic.Util.to_string topic in
    let event_parameters =
      [(Address_value facilitator_address, Address); abi_uint_of_int amount_to_transfer]
    in
    let event_signature =
      Ethereum_util.hex_string_of_string
        (Ethereum_util.hash
           (make_signature {function_name= "logTransfer"; parameters= event_parameters}))
    in
    assert (logged_event = event_signature) ;
    (* the facilitator address is visible as data *)
    let data = Basic.Util.to_string (Basic.Util.member "data" log_json) in
    let logged_encoding =
      let tuple_value, tuple_ty = abi_tuple_of_abi_values event_parameters in
      Ethereum_util.hex_string_of_bytes (encode_abi_value tuple_value tuple_ty)
    in
    assert (logged_encoding = data) ;
    (* confirm contract has received amount transferred *)
    let ending_balance_json =
      Lwt_main.run
        (send_balance_request_to_net (Ethereum_util.address_of_hex_string contract_address))
    in
    assert (not (json_contains_error ending_balance_json)) ;
    let ending_balance =
      int_of_string (Basic.Util.to_string (Basic.Util.member "result" ending_balance_json))
    in
    ending_balance = amount_to_transfer]
end

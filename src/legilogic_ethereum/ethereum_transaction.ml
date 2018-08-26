(* ethereum_transaction.ml -- code for running transactions on Ethereum net via JSON RPC *)

open Legilogic_lib
open Lib
open Hex
open Yojsoning
open Digesting
open Signing
open Types

open Main_chain

(* params contain hex strings, but we unhex those strings when running RLP *)
let build_operation_parameters = function
  | Operation.TransferTokens recipient ->
    [("to", Address.to_yojson recipient)]
  | Operation.CreateContract code ->
    [("data", `String (unparse_0x_bytes code))]
  | Operation.CallFunction (recipient, data) ->
    [("to", Address.to_yojson recipient);
     ("data", `String (unparse_0x_bytes data))]

let build_transaction_parameters transaction =
  let TxHeader.{ sender; gas_limit; gas_price; value } = transaction.Transaction.tx_header in
  `Assoc
    (List.append
       [ ("from", Address.to_yojson sender)
       ; ("gas", TokenAmount.to_yojson gas_limit)
       ; ("gasPrice", TokenAmount.to_yojson gas_price)
       ; ("value", TokenAmount.to_yojson value) ]
       (build_operation_parameters transaction.operation))

let build_transaction_json transaction =
  let params = build_transaction_parameters transaction in
  Ethereum_json_rpc.build_json_rpc_call_with_tagged_parameters Eth_sendTransaction [params]

let send_transaction_to_net signed_transaction =
  let transaction = signed_transaction.payload in
  let json = build_transaction_json transaction in
  Ethereum_json_rpc.send_rpc_call_to_net json

let send_balance_request_to_net address =
  let params = [Address.to_0x_string address; "latest"] in
  let json = Ethereum_json_rpc.build_json_rpc_call Ethereum_json_rpc.Eth_getBalance params in
  Ethereum_json_rpc.send_rpc_call_to_net json


let get_transaction_receipt transaction_hash =
  let params = [transaction_hash] in
  let json =
    Ethereum_json_rpc.build_json_rpc_call Ethereum_json_rpc.Eth_getTransactionReceipt params
  in
  Ethereum_json_rpc.send_rpc_call_to_net json

let get_transaction_by_hash transaction_hash =
  let params = [transaction_hash] in
  let json =
    Ethereum_json_rpc.build_json_rpc_call Ethereum_json_rpc.Eth_getTransactionByHash params
  in
  Ethereum_json_rpc.send_rpc_call_to_net json


let get_transaction_count address =
  let params = [Address.to_0x_string address; "latest"] in
  let json =
    Ethereum_json_rpc.build_json_rpc_call Ethereum_json_rpc.Eth_getTransactionCount params
  in
  Ethereum_json_rpc.send_rpc_call_to_net json


let transaction_executed transaction_hash =
  let open Lwt in
  get_transaction_receipt transaction_hash
  >>= fun receipt_json ->
  let keys = YoJson.keys receipt_json in
  let retval =
    not (List.mem "error" keys) && List.mem "result" keys
    &&
    let result_json = YoJson.member "result" receipt_json in
    result_json != `Null
    &&
    let result_keys = YoJson.keys result_json in
    List.mem "blockHash" result_keys && List.mem "blockNumber" result_keys
  in
  return retval

let transaction_execution_matches_transaction transaction_hash
      (signed_transaction: TransactionSigned.t) =
  let open Lwt in
  transaction_executed transaction_hash
  >>= fun executed ->
  if not executed then
    return false
  else
    get_transaction_by_hash transaction_hash
    >>= fun transaction_json ->
    let keys = YoJson.keys transaction_json in
    let retval =
      not (List.mem "error" keys)
      &&
      let transaction = signed_transaction.payload in
      let result_json = YoJson.member "result" transaction_json in
      (* for all operations, check these fields *)
      let get_result_json key =
        YoJson.to_string (YoJson.member key result_json) in
      let actual_sender = get_result_json "from" in
      let actual_nonce = get_result_json "nonce" in
      let actual_gas_price = TokenAmount.of_0x_string (get_result_json "gasPrice") in
      let actual_gas = TokenAmount.of_0x_string (get_result_json "gas") in
      let actual_value = TokenAmount.of_0x_string (get_result_json "value") in
      let tx_header = transaction.tx_header in
      let expected_sender = Address.to_0x_string tx_header.sender in
      let expected_nonce = "0x" ^ (Main_chain.Nonce.to_hex_string tx_header.nonce) in
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
        let expected_to = Address.to_0x_string recipient_address in
        actual_to = expected_to
      | CreateContract _ -> true
      | CallFunction (contract_address, _) ->
        let actual_to = get_result_json "to" in
        let expected_to = Address.to_0x_string contract_address in
        actual_to = expected_to
    in
    return retval

(* convert transaction record to rlp_item suitable for encoding *)
let rlp_of_transaction transaction =
  let open Main_chain in
  let open Ethereum_rlp in
  let tx_header = transaction.Transaction.tx_header in
  (* all items are strings, each character represents 2 digits in hex representation *)
  let nonce = Nonce.to_big_endian_bits tx_header.nonce in
  let gas_price = TokenAmount.to_big_endian_bits tx_header.gas_price in
  let gas_limit = TokenAmount.to_big_endian_bits tx_header.gas_limit in
  let value = TokenAmount.to_big_endian_bits tx_header.value in
  let toaddr, data =
    match transaction.operation with
    | TransferTokens to_address -> (Address.to_big_endian_bits to_address, "")
    | CreateContract bytes -> ("", Bytes.to_string bytes)
    | CallFunction (contract_address, call_encoding) ->
      (Address.to_big_endian_bits contract_address, Bytes.to_string call_encoding)
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
  let signature_items = [RlpItem v; RlpItem r; RlpItem s] in
  match transaction_rlp with
  | RlpItems items -> RlpItems (items @ signature_items)
  | _ -> bork "Expected RlpItems when creating signed transaction RLP"


(* https://medium.com/@codetractio/inside-an-ethereum-transaction-fa94ffca912f
   describes the transaction hashing algorithm
*)
let get_transaction_hash signed_transaction private_key =
  let transaction = signed_transaction.payload in
  let transaction_rlp = rlp_of_transaction transaction in
  (* step 1: RLP-encode the transaction *)
  let encoded_transaction = transaction_rlp |> Ethereum_rlp.encoded_string in
  (* step 2: Sign its hash with a private key, extract pieces of the signature *)
  let signature = make_signature digest_of_string private_key encoded_transaction in
  let (v, r, s) = signature_vrs signature in
  (* step 3: RLP-encode and hash the transaction augmented with the signature *)
  let signed_transaction_rlp = rlp_of_signed_transaction transaction_rlp ~v ~r ~s in
  let encoded_signed_transaction = Ethereum_rlp.encoded_string signed_transaction_rlp in
  keccak256_string encoded_signed_transaction

module Test = struct
  open Lwt
  open Ethereum_json_rpc
  open Signing.Test
  open Ethereum_util.Test

  let%test "move logs aside" = Logging.log_to_file "test.log"; true

  let assert_json_error_free location json =
    if YoJson.mem "error" json then
      bork "Error at %s from geth: %s" location (string_of_yojson json)

  let json_result_to_int json =
    YoJson.member "result" json |> YoJson.to_string |> int_of_string

  let list_accounts () =
    let params = [] in
    let json = build_json_rpc_call Personal_listAccounts params in
    send_rpc_call_to_net json

  let new_account () =
    (* test accounts have empty password *)
    let params = [""] in
    let json = build_json_rpc_call Personal_newAccount params in
    send_rpc_call_to_net json

  let unlock_account ?(duration=5) address =
    let password = "" in
    let params =
      [`String (Address.to_0x_string address); `String password; `Int duration]
    in
    let json = build_json_rpc_call_with_tagged_parameters Personal_unlockAccount params in
    send_rpc_call_to_net json

  let get_first_account () =
    list_accounts ()
    >>= fun accounts_json ->
    let accounts = YoJson.to_list (YoJson.member "result" accounts_json) in
    assert (not (accounts = [])) ;
    return (List.hd accounts)

  let is_testnet_up () =
    let max_tries = 10 in
    let rec poll_net n =
      if n > max_tries then
        false
      else
        try
          let _ = get_first_account () in
          true
        with Unix.Unix_error(Unix.ECONNREFUSED, "connect", "") -> (
            Unix.sleep 1;
            poll_net (n + 1))
    in
    poll_net 0

  let get_nonce address =
    get_transaction_count address
    >>= fun contract_count_json ->
    assert_json_error_free __LOC__ contract_count_json;
    let result = YoJson.member "result" contract_count_json
                 |> YoJson.to_string
    in
    return (Nonce.of_int64 (Int64.of_string result))

  let wait_for_contract_execution transaction_hash =
    let max_counter = 20 in
    (* wait for transaction to appear in block *)
    let rec loop counter =
      transaction_executed transaction_hash
      >>= fun b ->
      if counter > max_counter then
        bork "Could not verify contract execution"
      else if b then
        return ()
      else (
        Unix.sleepf 0.1 ;
        loop (counter + 1)
      )
    in
    loop 0

  let%test "poll-for-testnet" =
    is_testnet_up () || bork "Could not connect to Ethereum test net"

  let%test "transfer-on-Ethereum-testnet" =
    Lwt_main.run (
      get_first_account ()
      >>= fun account_json ->
      let sender_account = YoJson.to_string account_json in
      let sender_address = Address.of_0x_string sender_account in
      new_account ()
      >>= fun new_account_json ->
      assert_json_error_free __LOC__ new_account_json;
      let new_account = YoJson.to_string (YoJson.member "result" new_account_json) in
      let recipient_address = Address.of_0x_string new_account in
      (* unlock accounts *)
      unlock_account sender_address
      >>= fun unlock_sender_json ->
      assert_json_error_free __LOC__ unlock_sender_json;
      unlock_account recipient_address
      >>= fun unlock_recipient_json ->
      assert_json_error_free __LOC__ unlock_recipient_json;
      (* we don't check opening balance, which may be too large to parse *)
      let transfer_amount = 22 in
      get_nonce sender_address
      >>= fun nonce ->
      let tx_header =
        { TxHeader.sender= sender_address
        ; nonce= nonce
        ; gas_price= TokenAmount.of_int 2
        ; gas_limit= TokenAmount.of_int 1000000
        ; value= TokenAmount.of_int transfer_amount }
      in
      let operation = Operation.TransferTokens recipient_address in
      let transaction = {Transaction.tx_header; Transaction.operation} in
      let signed_transaction = Transaction.signed alice_keys transaction in
      (* send tokens *)
      send_transaction_to_net signed_transaction
      >>= fun output ->
      assert_json_error_free __LOC__ output;
      let result_json = YoJson.member "result" output in
      let transaction_hash = YoJson.to_string result_json in
      wait_for_contract_execution transaction_hash
      >>= fun () ->
      transaction_execution_matches_transaction transaction_hash signed_transaction)

  let%test "create-contract-on-Ethereum-testnet" =
    Lwt_main.run (
      get_first_account ()
      >>= fun first_account ->
      let sender_account =  YoJson.to_string first_account in
      let sender_address = Address.of_0x_string sender_account in
      unlock_account sender_address
      >>= fun unlock_sender_json ->
      assert_json_error_free __LOC__ unlock_sender_json;
      get_nonce sender_address
      >>= fun nonce ->
      let tx_header =
        { TxHeader.sender= sender_address
        ; nonce= nonce
        ; gas_price= TokenAmount.of_int 2
        ; gas_limit= TokenAmount.of_int 1000000
        ; value= TokenAmount.zero }
      in
      (* a valid contract contains compiled EVM code
         for testing, we just use a buffer with arbitrary contents
      *)
      let operation = Operation.CreateContract (Bytes.create 128) in
      let transaction = {Transaction.tx_header; Transaction.operation} in
      let signed_transaction = Transaction.signed alice_keys transaction in
      (* create contract *)
      send_transaction_to_net signed_transaction
      >>= fun output ->
      assert_json_error_free __LOC__ output;
      let result_json = YoJson.member "result" output in
      let transaction_hash = YoJson.to_string result_json in
      wait_for_contract_execution transaction_hash
      >>= fun () ->
      let signed_transaction = Transaction.signed alice_keys transaction in
      transaction_execution_matches_transaction transaction_hash signed_transaction)

  let%test "call-contract-on-Ethereum-testnet" =
    let open Main_chain in
    Lwt_main.run (
      get_first_account ()
      >>= fun sender_account_json ->
      let sender_account = YoJson.to_string sender_account_json in
      let sender_address = Address.of_0x_string sender_account in
      unlock_account sender_address
      >>= fun unlock_sender_json ->
      assert_json_error_free __LOC__ unlock_sender_json;
      get_nonce sender_address
      >>= fun nonce ->
      let tx_header =
        { TxHeader.sender= sender_address
        ; nonce= nonce
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
      let hashed = digest_of_string "some arbitrary string" in
      let operation =
        Operation.CallFunction
          ( Address.of_0x_string "0x2B1c40cD23AAB27F59f7874A1F454748B004C4D8"
          , Bytes.of_string (Digest.to_big_endian_bits hashed) )
      in
      let transaction = {Transaction.tx_header; Transaction.operation} in
      let signed_transaction = Transaction.signed alice_keys transaction in
      send_transaction_to_net signed_transaction
      >>= fun output ->
      assert_json_error_free __LOC__ output;
      let result_json = YoJson.member "result" output in
      let transaction_hash = YoJson.to_string result_json in
      wait_for_contract_execution transaction_hash
      >>= fun () ->
      let signed_transaction = Transaction.signed alice_keys transaction in
      transaction_execution_matches_transaction transaction_hash signed_transaction)

  (* TODO: sign the RLP, not the side-chain style marshaling!
     Go through the entire example and check that we get the correct values bit-for-bit.
  *)
  let%test "compute-transaction-hash" =
    Lwt_main.run (
      (* example from https://medium.com/@codetractio/inside-an-ethereum-transaction-fa94ffca912f *)
      let private_key_hex = "0xc0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0de" in
      let private_key_string = parse_0x_string private_key_hex in
      let private_key = Signing.make_private_key private_key_string in
      get_first_account ()
      >>= fun account_json ->
      let sender_account = YoJson.to_string account_json in
      let sender_address = Address.of_0x_string sender_account in
      let tx_header =
        TxHeader.{ sender= sender_address (* doesn't matter for transaction hash *)
                 ; nonce= Nonce.zero
                 ; gas_price= TokenAmount.of_int 20000000000
                 ; gas_limit= TokenAmount.of_int 100000
                 ; value= TokenAmount.of_int 1000 }
      in
      let operation =
        Operation.CallFunction
          ( Address.of_0x_string "0x687422eea2cb73b5d3e242ba5456b782919afc85"
          , parse_0x_bytes "0xc0de")
      in
      let transaction = {Transaction.tx_header; Transaction.operation} in
      let signed_transaction = Signing.signed Transaction.digest private_key transaction in
      let transaction_hash = get_transaction_hash signed_transaction private_key in
      expect_0x_string "transaction hash"
        "0x472703a1599c7f8ffb221715e2183c8736e38e46a9447f3fb69285407b71aec2"
        transaction_hash;
      return true)

  let%test "hello-solidity" =
    let open Ethereum_abi in
    Lwt_main.run (
      (* code is result of running "solc --bin hello.sol", and prepending "0x" *)
      let code =
        "0x608060405234801561001057600080fd5b506101a7806100206000396000f300608060405260043610610041576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff16806339a7aa4814610046575b600080fd5b34801561005257600080fd5b5061005b6100d6565b6040518080602001828103825283818151815260200191508051906020019080838360005b8381101561009b578082015181840152602081019050610080565b50505050905090810190601f1680156100c85780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b60607fcde5c32c0a45fd8aa4b65ea8003fc9da9acd5e2c6c24a9fcce6ab79cabbd912260405180806020018281038252600d8152602001807f48656c6c6f2c20776f726c64210000000000000000000000000000000000000081525060200191505060405180910390a16040805190810160405280600881526020017f476f6f64627965210000000000000000000000000000000000000000000000008152509050905600a165627a7a7230582024923934849b0e74a5091ac4b5c65d9b3b93d74726aff49fd5763bc136dac5c60029"
      in
      let code_bytes = parse_0x_bytes code in
      (* create a contract using "hello, world" EVM code *)
      get_first_account ()
      >>= fun account_json ->
      let sender_account = YoJson.to_string account_json in
      let sender_address = Address.of_0x_string sender_account in
      unlock_account sender_address
      >>= fun unlock_sender_json ->
      assert_json_error_free __LOC__ unlock_sender_json;
      get_nonce sender_address
      >>= fun nonce ->
      let tx_header =
        { TxHeader.sender= sender_address
        ; nonce= nonce
        ; gas_price= TokenAmount.of_int 2
        ; gas_limit= TokenAmount.of_int 1000000
        ; value= TokenAmount.zero }
      in
      (* a valid contract contains compiled EVM code
         for testing, we just use a buffer with arbitrary contents
      *)
      let operation = Operation.CreateContract code_bytes in
      let transaction = {Transaction.tx_header; Transaction.operation} in
      let signed_transaction = Transaction.signed alice_keys transaction in
      (* create contract *)
      send_transaction_to_net signed_transaction
      >>= fun output ->
      assert_json_error_free __LOC__ output;
      let result_json = YoJson.member "result" output in
      let transaction_hash = YoJson.to_string result_json in
      wait_for_contract_execution transaction_hash
      >>= fun () ->
      get_transaction_receipt transaction_hash
      >>= fun receipt_json ->
      assert_json_error_free __LOC__ receipt_json;
      let receipt_result_json = YoJson.member "result" receipt_json in
      let contract_address =
        YoJson.to_string (YoJson.member "contractAddress" receipt_result_json)
      in
      let signed_transaction = Transaction.signed alice_keys transaction in
      transaction_execution_matches_transaction transaction_hash signed_transaction
      >>= fun matches ->
      assert matches ;
      (* call the contract we've created *)
      get_nonce sender_address
      >>= fun nonce ->
      let tx_header1 =
        { TxHeader.sender= sender_address
        ; nonce= nonce
        ; gas_price= TokenAmount.of_int 2
        ; gas_limit= TokenAmount.of_int 1000000
        ; value= TokenAmount.zero }
      in
      let call = {function_name= "printHelloWorld"; parameters= []} in
      let call_bytes = encode_function_call call in
      let operation1 =
        Operation.CallFunction (Address.of_0x_string contract_address, call_bytes)
      in
      let transaction1 = {Transaction.tx_header= tx_header1; Transaction.operation= operation1} in
      let signed_transaction1 = Transaction.signed alice_keys transaction1 in
      send_transaction_to_net signed_transaction1
      >>= fun output1 ->
      assert_json_error_free __LOC__ output;
      let result_json1 = YoJson.member "result" output1 in
      let transaction_hash1 = YoJson.to_string result_json1 in
      wait_for_contract_execution transaction_hash1
      >>= fun () ->
      get_transaction_receipt transaction_hash1
      >>= fun receipt_json1 ->
      (* verify that we called "printHelloWorld" *)
      let receipt_result1_json = YoJson.member "result" receipt_json1 in
      let log_json = List.hd (YoJson.to_list (YoJson.member "logs" receipt_result1_json)) in
      (* we called the right contract *)
      let log_contract_address = YoJson.to_string (YoJson.member "address" log_json) in
      assert (log_contract_address = contract_address) ;
      (* we called the right function within the contract *)
      let log_topics = YoJson.to_list (YoJson.member "topics" log_json) in
      assert (List.length log_topics = 1) ;
      let topic_event = YoJson.to_string (List.hd log_topics) in
      let hello_world = (String_value "Hello, world!", String) in
      let function_signature = {function_name= "showResult"; parameters= [hello_world]} in
      let function_signature_hash = function_signature |> function_signature_hash |> unparse_0x_string in
      assert (topic_event = function_signature_hash) ;
      (* the log data is the encoding of the parameter passed to the event *)
      let data = YoJson.to_string (YoJson.member "data" log_json) in
      let hello_encoding =
        let tuple_value, tuple_ty = abi_tuple_of_abi_values [hello_world] in
        unparse_0x_bytes (encode_abi_value tuple_value tuple_ty)
      in
      return (data = hello_encoding))

  let%test "fallback-with-facilitator-address" =
    (* we call the fallback function in a contract by using the facilitator address as "code" *)
    let open Ethereum_abi in
    Lwt_main.run (
      (* code is result of running "solc --bin facilitator-fallback.sol", and prepending "0x" *)
      let code =
        "0x608060405234801561001057600080fd5b50610108806100206000396000f300608060405260146000369050141515601657600080fd5b7facfada45e09e5bb4c2c456febe99efe38be8bfc67a25cccdbb4c93ec56f661a560716000368080601f01602080910402602001604051908101604052809392919081815260200183838082843782019150505050505060bc565b34604051808373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018281526020019250505060405180910390a1005b6000602082015190506c01000000000000000000000000810490509190505600a165627a7a7230582098fc57c39988f3dcf9f7168b876b9f491273775ea6b44db8cb9483966fa1adc10029"
      in
      let code_string = parse_0x_string code in
      let code_bytes = Bytes.of_string code_string in
      (* create the contract *)
      get_first_account ()
      >>= fun account_json ->
      let sender_account = YoJson.to_string account_json in
      let sender_address = Address.of_0x_string sender_account in
      unlock_account sender_address
      >>= fun unlock_sender_json ->
      assert_json_error_free __LOC__ unlock_sender_json;
      get_nonce sender_address
      >>= fun nonce ->
      let tx_header =
        { TxHeader.sender= sender_address
        ; nonce= nonce
        ; gas_price= TokenAmount.of_int 42
        ; gas_limit= TokenAmount.of_int 1000000
        ; value= TokenAmount.zero }
      in
      let operation = Operation.CreateContract code_bytes in
      let transaction = {Transaction.tx_header; Transaction.operation} in
      let signed_transaction = Transaction.signed alice_keys transaction in
      send_transaction_to_net signed_transaction
      >>= fun output ->
      assert_json_error_free __LOC__ output;
      let result_json = YoJson.member "result" output in
      let transaction_hash = YoJson.to_string result_json in
      wait_for_contract_execution transaction_hash
      >>= fun () ->
      get_transaction_receipt transaction_hash
      >>= fun receipt_json ->
      assert_json_error_free __LOC__ receipt_json;
      let receipt_result_json = YoJson.member "result" receipt_json in
      let contract_address =
        YoJson.to_string (YoJson.member "contractAddress" receipt_result_json)
      in
      (* check balance of new contract *)
      send_balance_request_to_net (Address.of_0x_string contract_address)
      >>= fun starting_balance_json ->
      assert_json_error_free __LOC__ starting_balance_json;
      let starting_balance =
        int_of_string (YoJson.to_string (YoJson.member "result" starting_balance_json))
      in
      assert (starting_balance = 0) ;
      let signed_transaction = Transaction.signed alice_keys transaction in
      transaction_execution_matches_transaction transaction_hash signed_transaction
      >>= fun matches ->
      assert matches;
      (* call the fallback in the contract we've created *)
      let amount_to_transfer = 93490 in
      get_nonce sender_address
      >>= fun nonce ->
      let tx_header1 =
        { TxHeader.sender= sender_address
        ; nonce= nonce
        ; gas_price= TokenAmount.of_int 2
        ; gas_limit= TokenAmount.of_int 1000000
        ; value= TokenAmount.of_int amount_to_transfer }
      in
      (* use (dummy) facilitator address as code to trigger fallback *)
      let facilitator_address =
        Address.of_0x_string "0x9797809415e4b8efea0963e362ff68b9d98f9e00"
      in
      let address_bytes = Ethereum_util.bytes_of_address facilitator_address in
      (* TODO: This smells fishy; where is the hash of the function being called? *)
      let operation1 =
        Operation.CallFunction (Address.of_0x_string contract_address, address_bytes)
      in
      let transaction1 = {Transaction.tx_header= tx_header1; Transaction.operation= operation1} in
      let signed_transaction1 = Transaction.signed alice_keys transaction1 in
      send_transaction_to_net signed_transaction1
      >>= fun output1 ->
      assert_json_error_free __LOC__ output1;
      let result_json1 = YoJson.member "result" output1 in
      let transaction_hash1 = YoJson.to_string result_json1 in
      wait_for_contract_execution transaction_hash1
      >>= fun () ->
      get_transaction_receipt transaction_hash1
      >>= fun receipt_json1 ->
      (* verify that we called the fallback *)
      let receipt_result1_json = YoJson.member "result" receipt_json1 in
      let logs = YoJson.to_list (YoJson.member "logs" receipt_result1_json) in
      assert (List.length logs = 1) ;
      let log_json = List.hd logs in
      (* the log is for this contract *)
      let receipt_address = YoJson.to_string (YoJson.member "address" log_json) in
      assert (receipt_address = contract_address) ;
      (* we saw the expected event *)
      let topics = YoJson.to_list (YoJson.member "topics" log_json) in
      assert (List.length topics = 1) ;
      let topic = List.hd topics in
      let logged_event = YoJson.to_string topic in
      let event_parameters =
        [(Address_value facilitator_address, Address); abi_uint_of_int amount_to_transfer]
      in
      let event_signature =
        {function_name= "logTransfer"; parameters= event_parameters}
        |> function_signature_hash |> unparse_0x_string
      in
      assert (logged_event = event_signature) ;
      (* the facilitator address is visible as data *)
      let data = YoJson.to_string (YoJson.member "data" log_json) in
      let logged_encoding =
        let tuple_value, tuple_ty = abi_tuple_of_abi_values event_parameters in
        unparse_0x_bytes (encode_abi_value tuple_value tuple_ty)
      in
      assert (logged_encoding = data) ;
      (* confirm contract has received amount transferred *)
      send_balance_request_to_net (Address.of_0x_string contract_address)
      >>= fun ending_balance_json ->
      assert_json_error_free __LOC__ ending_balance_json;
      let ending_balance =
        int_of_string (YoJson.to_string (YoJson.member "result" ending_balance_json))
      in
      assert (ending_balance = amount_to_transfer) ;
      (* now try invalid address, make sure it's not logged *)
      let bogus_address_bytes = parse_0x_bytes "0xFF" in
      let operation2 =
        Operation.CallFunction
          (Address.of_0x_string contract_address, bogus_address_bytes)
      in
      let tx_header2 = tx_header1 in
      let transaction2 = {Transaction.tx_header= tx_header2; Transaction.operation= operation2} in
      let signed_transaction2 = Transaction.signed alice_keys transaction2 in
      send_transaction_to_net signed_transaction2
      >>= fun output2 ->
      assert_json_error_free __LOC__ output2;
      let result_json2 = YoJson.member "result" output2 in
      let transaction_hash2 = YoJson.to_string result_json2 in
      wait_for_contract_execution transaction_hash2
      >>= fun () ->
      get_transaction_receipt transaction_hash2
      >>= fun receipt_json2 ->
      let receipt_result2_json = YoJson.member "result" receipt_json2 in
      let logs2 = YoJson.to_list (YoJson.member "logs" receipt_result2_json) in
      return (List.length logs2 = 0))
end

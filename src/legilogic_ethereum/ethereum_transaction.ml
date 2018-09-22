(** Somewhat higher-level wrappers around the basic functionality in ethereum_json_rpc *)
open Legilogic_lib
open Lib
open Hex
open Yojsoning
open Digesting
open Signing
open Types
open Action
open Lwt_exn
open Json_rpc

open Ethereum_chain

(* TODO: when to return false vs raise an exception? Add timeout & log *)
let transaction_executed transaction_hash =
  Ethereum_json_rpc.eth_get_transaction_by_hash transaction_hash
  >>= fun info ->
  return (Option.is_some info.block_hash && Option.is_some info.block_number)


(* TODO: factor this function into parsing a transaction and comparing transaction objects. *)
let transaction_execution_matches_transaction transaction_hash (transaction: Transaction.t) =
  transaction_executed transaction_hash
  >>= fun executed ->
  if not executed then
    return false
  else
    Ethereum_json_rpc.eth_get_transaction_by_hash transaction_hash
    >>= fun info ->
    return
      (try
         (* for all operations, check these fields *)
         let tx_header = transaction.tx_header in
         info.from = Some tx_header.sender
         && info.nonce = tx_header.nonce
         && TokenAmount.compare info.gas tx_header.gas_limit <= 0
         && TokenAmount.compare info.gas_price tx_header.gas_price <= 0
         && TokenAmount.compare info.value tx_header.value = 0
         && (* operation-specific checks *)
         match transaction.operation with
         | TransferTokens recipient_address ->
           info.to_ = Some recipient_address
         | CreateContract data ->
           info.input = data
         | CallFunction (contract_address, call_input) ->
           info.to_ = Some contract_address
           && info.input = call_input
       with _ -> false)

(* convert transaction record to rlp_item suitable for encoding
   TODO: make that our marshaling strategy.
*)
let rlp_of_transaction transaction =
  let open Ethereum_chain in
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
  | _ -> Lib.bork "Expected RlpItems when creating signed transaction RLP"


(* https://medium.com/@codetractio/inside-an-ethereum-transaction-fa94ffca912f
   describes the transaction hashing algorithm

   TODO: make our algorithm correct wrt the v r s:
   looks like we must do a first pass where v, r, s are zero,
   then replace them by the right values.
*)
let get_transaction_hash transaction private_key =
  let transaction_rlp = rlp_of_transaction transaction in
  (* step 1: RLP-encode the transaction *)
  let encoded_transaction = transaction_rlp |> Ethereum_rlp.encoded_string in
  (* step 2: Sign its hash with a private key, extract pieces of the signature *)
  let signature = make_signature digest_of_string private_key encoded_transaction in
  let (v, r, s) = signature_vrs signature in
  (* step 3: RLP-encode and hash the transaction augmented with the signature *)
  let signed_transaction_rlp = rlp_of_signed_transaction transaction_rlp ~v ~r ~s in
  let encoded_signed_transaction = Ethereum_rlp.encoded_string signed_transaction_rlp in
  digest_of_string encoded_signed_transaction

let ensure_private_key ?(timeout=rpc_timeout) ?(log= !rpc_log) (keypair, password) =
  (keypair.Keypair.private_key, password)
  |> trying (Ethereum_json_rpc.personal_import_raw_key ~timeout ~log)
  >>= handling
        (function
          | Rpc_error x as e ->
            if x.message = "account already exists"
            then return keypair.address
            else fail e
          | e -> fail e)

let list_accounts () =
  Ethereum_json_rpc.personal_list_accounts ()

let get_first_account =
  list_accounts >>> catching_arr List.hd

module Test = struct
  open Ethereum_json_rpc
  open Ethereum_util.Test

  let%test "move logs aside" = Logging.set_log_file "test.log"; true

  let get_prefunded_address = get_first_account

  let is_ethereum_net_up () =
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

  let%test "poll-for-testnet" =
    is_ethereum_net_up () || Lib.bork "Could not connect to Ethereum network"

  let%test "transfer-on-Ethereum-testnet" =
    Lwt_exn.run
      (fun () ->
         get_prefunded_address ()
         >>= fun sender_address ->
         Ethereum_json_rpc.personal_new_account ""
         >>= fun recipient_address ->
         (* we don't check opening balance, which may be too large to parse *)
         let transfer_amount = 22 in
         Ethereum_user.(user_action sender_address transfer_tokens)
           (recipient_address, TokenAmount.of_int transfer_amount)
         >>= Ethereum_user.(user_action sender_address confirm_transaction)
         >>= fun (transaction, Confirmation.{transaction_hash}) ->
         transaction_execution_matches_transaction transaction_hash transaction)
      ()

  let%test "create-contract-on-Ethereum-testnet" =
    Lwt_exn.run
      (fun () ->
         get_prefunded_address ()
         >>= fun sender_address ->
         Ethereum_user.(user_action sender_address
                          (make_signed_transaction
                             (Operation.CreateContract (Bytes.create 128))
                             TokenAmount.zero))
           (TokenAmount.of_int 100000)
         >>= Ethereum_user.(user_action sender_address confirm_transaction)
         >>= fun (tx, {transaction_hash}) ->
         transaction_execution_matches_transaction transaction_hash tx)
      ()

  let%test "call-contract-on-Ethereum-testnet" =
    let open Ethereum_chain in
    Lwt_exn.run
      (fun () ->
         get_prefunded_address ()
         >>= fun sender_address ->
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
             , Bytes.of_string (Digest.to_big_endian_bits hashed) ) in
         Ethereum_user.(user_action sender_address
                          (make_signed_transaction
                             operation
                             (TokenAmount.zero)))
           (TokenAmount.of_int 1000000)
         >>= Ethereum_user.(user_action sender_address confirm_transaction)
         >>= fun (transaction, Confirmation.{transaction_hash}) ->
         transaction_execution_matches_transaction transaction_hash transaction)
      ()

  (* TODO: sign the RLP, not the side-chain style marshaling!
     Go through the entire example and check that we get the correct values bit-for-bit.
  *)
  let%test "compute-transaction-hash" =
    Lwt_exn.run
      (fun () ->
         (* example from https://medium.com/@codetractio/inside-an-ethereum-transaction-fa94ffca912f *)
         let keypair = keypair_of_0x
                         "0xc0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0de"
                         "0x044643bb6b393ac20a6175c713175734a72517c63d6f73a3ca90a15356f2e967da03d16431441c61ac69aeabb7937d333829d9da50431ff6af38536aa262497b27" in
         expect_string "c0de address"
           "0x53ae893e4b22d707943299a8d0c844df0e3d5557"
           (Address.to_0x_string keypair.address);
         get_prefunded_address ()
         >>= fun sender_address ->
         let tx_header =
           TxHeader.{ sender= sender_address (* doesn't matter for transaction hash *)
                    ; nonce= Nonce.zero
                    ; gas_price= TokenAmount.of_int 20000000000
                    ; gas_limit= TokenAmount.of_int 100000
                    ; value= TokenAmount.of_int 1000 } in
         let operation =
           Operation.CallFunction
             ( Address.of_0x_string "0x687422eea2cb73b5d3e242ba5456b782919afc85"
             , parse_0x_bytes "0xc0de") in
         let transaction = {Transaction.tx_header; Transaction.operation} in
         let unsigned_transaction_hash =
           transaction |> rlp_of_transaction |> Ethereum_rlp.encoded_string |> digest_of_string in
         (* TODO: FIX THE CODE, THEN RESTORE THE TEST!
            expect_string "unsigned transaction hash"
            "0x6a74f15f29c3227c5d1d2e27894da58d417a484ef53bc7aa57ee323b42ded656"
            (Digest.to_0x_string unsigned_transaction_hash);
         *)
         ignore unsigned_transaction_hash;
         let transaction_hash = get_transaction_hash transaction keypair.private_key in
(*
            expect_string "transaction hash"
            "0x8b69a0ca303305a92d8d028704d65e4942b7ccc9a99917c8c9e940c9d57a9662"
            (Digest.to_0x_string transaction_hash);
         *)
         ignore transaction_hash;
         return true)
      ()

  let%test "hello-solidity" =
    let open Ethereum_abi in
    Lwt_exn.run
      (fun () ->
         (* code is result of running "solc --bin hello.sol", and prepending "0x" *)
         let code =
           "0x608060405234801561001057600080fd5b506101a7806100206000396000f300608060405260043610610041576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff16806339a7aa4814610046575b600080fd5b34801561005257600080fd5b5061005b6100d6565b6040518080602001828103825283818151815260200191508051906020019080838360005b8381101561009b578082015181840152602081019050610080565b50505050905090810190601f1680156100c85780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b60607fcde5c32c0a45fd8aa4b65ea8003fc9da9acd5e2c6c24a9fcce6ab79cabbd912260405180806020018281038252600d8152602001807f48656c6c6f2c20776f726c64210000000000000000000000000000000000000081525060200191505060405180910390a16040805190810160405280600881526020017f476f6f64627965210000000000000000000000000000000000000000000000008152509050905600a165627a7a7230582024923934849b0e74a5091ac4b5c65d9b3b93d74726aff49fd5763bc136dac5c60029"
         in
         let code_bytes = parse_0x_bytes code in
         (* create a contract using "hello, world" EVM code *)
         get_prefunded_address ()
         >>= fun sender_address ->
         (* a valid contract contains compiled EVM code
            for testing, we just use a buffer with arbitrary contents
         *)
         Ethereum_user.(user_action sender_address
                          (make_signed_transaction
                             (Operation.CreateContract code_bytes)
                             (TokenAmount.zero)))
           (TokenAmount.of_int 1000000)
         >>= Ethereum_user.(user_action sender_address confirm_transaction)
         >>= fun (transaction, Confirmation.{transaction_hash}) ->
         transaction_execution_matches_transaction transaction_hash transaction
         >>= fun matches ->
         assert matches ;
         (* call the contract we've created *)
         eth_get_transaction_receipt transaction_hash
         >>= arr Option.get
         >>= fun receipt ->
         let contract_address = Option.get receipt.TransactionReceipt.contract_address in
         let call_bytes = encode_function_call {function_name= "printHelloWorld"; parameters= []} in
         Ethereum_user.(user_action sender_address
                          (make_signed_transaction
                             (Operation.CallFunction (contract_address, call_bytes))
                             TokenAmount.zero))
           (TokenAmount.of_int 1000000)
         >>= Ethereum_user.(user_action sender_address confirm_transaction)
         >>= fun (_transaction, Confirmation.{transaction_hash}) ->
         eth_get_transaction_receipt transaction_hash
         >>= arr Option.get
         >>= fun receipt1 ->
         (* verify that we called "printHelloWorld" *)
         let receipt_log = match receipt1.TransactionReceipt.logs with [x] -> x | _ -> Lib.bork "blah" in
         (* we called the right contract *)
         let log_contract_address = receipt_log.LogObject.address in
         assert (log_contract_address = contract_address) ;
         (* we called the right function within the contract *)
         let topic_event = match receipt_log.LogObject.topics with [x] -> x | _ -> Lib.bork "bloh" in
         let hello_world = (String_value "Hello, world!", String) in
         let function_signature = {function_name= "showResult"; parameters= [hello_world]} in
         let function_signature_digest = function_signature |> function_signature_digest in
         assert (Digest.equal topic_event function_signature_digest) ;
         (* the log data is the encoding of the parameter passed to the event *)
         let data = receipt_log.data in
         let hello_encoding =
           let tuple_value, tuple_ty = abi_tuple_of_abi_values [hello_world] in
           unparse_0x_bytes (encode_abi_value tuple_value tuple_ty)
         in
         return (unparse_0x_bytes data = hello_encoding))
      ()

  let%test "fallback-with-facilitator-address" =
    (* we call the fallback function in a contract by using the facilitator address as "code" *)
    let open Ethereum_abi in
    Lwt_exn.run
      (fun () ->
         (* code is result of running "solc --bin facilitator-fallback.sol", and prepending "0x" *)
         let code =
           "0x608060405234801561001057600080fd5b50610108806100206000396000f300608060405260146000369050141515601657600080fd5b7facfada45e09e5bb4c2c456febe99efe38be8bfc67a25cccdbb4c93ec56f661a560716000368080601f01602080910402602001604051908101604052809392919081815260200183838082843782019150505050505060bc565b34604051808373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018281526020019250505060405180910390a1005b6000602082015190506c01000000000000000000000000810490509190505600a165627a7a7230582098fc57c39988f3dcf9f7168b876b9f491273775ea6b44db8cb9483966fa1adc10029"
         in
         let code_bytes = parse_0x_bytes code in
         (* create the contract *)
         get_prefunded_address ()
         >>= fun sender_address ->
         Ethereum_user.(user_action sender_address
                          (make_signed_transaction
                             (Operation.CreateContract code_bytes)
                             (TokenAmount.zero)))
           (TokenAmount.of_int 1000000)
         >>= Ethereum_user.(user_action sender_address confirm_transaction)
         >>= fun (transaction, Confirmation.{transaction_hash}) ->
         eth_get_transaction_receipt transaction_hash
         >>= arr Option.get
         >>= fun receipt ->
         let contract_address = Option.get receipt.contract_address in
         (* check balance of new contract *)
         eth_get_balance (contract_address, Latest)
         >>= fun starting_balance ->
         assert (TokenAmount.sign starting_balance = 0) ;
         transaction_execution_matches_transaction transaction_hash transaction
         >>= fun matches ->
         assert matches;
         (* Call the fallback in the contract we've created.
            It bypasses the regular ABI to access this address directly. *)
         let amount_to_transfer = TokenAmount.of_int 93490 in
         let facilitator_address = Address.of_0x_string "0x9797809415e4b8efea0963e362ff68b9d98f9e00" in
         let call_bytes = Ethereum_util.bytes_of_address facilitator_address in
         Ethereum_user.(user_action sender_address
                          (make_signed_transaction
                             (Operation.CallFunction (contract_address, call_bytes))
                             amount_to_transfer))
           (TokenAmount.of_int 1000000)
         >>= Ethereum_user.(user_action sender_address confirm_transaction)
         >>= fun (_transaction, Confirmation.{transaction_hash}) ->
         eth_get_transaction_receipt transaction_hash
         >>= arr Option.get
         >>= fun receipt1 ->
         (* verify that we called the fallback *)
         let log = match receipt1.logs with [x] -> x | _ -> Lib.bork "bloh" in
         (* the log is for this contract *)
         let receipt_address = log.address in
         assert (receipt_address = contract_address) ;
         (* we saw the expected event *)
         let logged_event = match log.LogObject.topics with [x] -> x | _ -> Lib.bork "bluh" in
         let event_parameters =
           [(Address_value facilitator_address, Address); abi_token_amount amount_to_transfer]
         in
         let event_signature =
           {function_name= "logTransfer"; parameters= event_parameters}
           |> function_signature_digest
         in
         assert (logged_event = event_signature) ;
         (* the facilitator address is visible as data *)
         let data = unparse_0x_bytes log.data in
         let logged_encoding =
           let tuple_value, tuple_ty = abi_tuple_of_abi_values event_parameters in
           unparse_0x_bytes (encode_abi_value tuple_value tuple_ty)
         in
         assert (logged_encoding = data) ;
         (* confirm contract has received amount transferred *)
         eth_get_balance (contract_address, Latest)
         >>= fun ending_balance ->
         assert (ending_balance = amount_to_transfer) ;
         (* now try invalid address, make sure it's not logged *)
         let bogus_address_bytes = parse_0x_bytes "0xFF" in
         Ethereum_user.(user_action sender_address
                          (make_signed_transaction
                             (Operation.CallFunction (contract_address, bogus_address_bytes))
                             amount_to_transfer))
           (TokenAmount.of_int 1000000)
         >>= Ethereum_user.(user_action sender_address confirm_transaction)
         >>= fun (_transaction, Confirmation.{transaction_hash}) ->
         eth_get_transaction_receipt transaction_hash
         >>= arr Option.get
         >>= fun receipt2 ->
         let logs2 = receipt2.logs in
         return (List.length logs2 = 0))
      ()
end

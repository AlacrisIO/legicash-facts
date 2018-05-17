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
  (* Geth-specific methods, should only be used in tests *)
  | Personal_unlockAccount
  [@@deriving show]

(* global state *)
let id_counter = ref 1

(* Ethereum uses format 0x followed by hex-digit pairs *)
let rec zero_code = Char.code '0'

and a_code = Char.code 'a'

and big_a_code = Char.code 'A'

and string_of_hex_string hs =
  let len = String.length hs in
  if not (hs.[0] = '0' && hs.[1] = 'x') then
    raise (Internal_error "Hex string does not begin with 0x") ;
  if len mod 2 = 1 then raise (Internal_error "Hex string contains odd number of characters") ;
  let unhex_digit hd =
    match hd with
    | '0'..'9' -> Char.code hd - zero_code
    | 'a'..'f' -> Char.code hd - a_code + 0xa
    | 'A'..'F' -> Char.code hd - big_a_code + 0xa (* be liberal in what we accept *)
    | _ -> raise (Internal_error (Printf.sprintf "Invalid hex digit %c" hd))
  in
  String.init
    ((len - 2) / 2)
    (fun ndx ->
      let ndx2 = 2 + 2 * ndx in
      let hi_nybble = unhex_digit hs.[ndx2] in
      let lo_nybble = unhex_digit hs.[ndx2 + 1] in
      Char.chr (hi_nybble lsl 4 + lo_nybble) )


and hex_string_of_string s =
  let len = String.length s in
  let to_hex_digit byte =
    if byte < 0xa then Char.chr (byte + zero_code) (* 0 - 9 *)
    else if byte >= 0xa && byte <= 0xf then (* a - f *)
      Char.chr (a_code + (byte - 0xa))
    else raise (Internal_error "Not a valid hex digit")
  in
  let get_hex_digit ndx =
    let ndx2 = ndx / 2 in
    let bytes = Char.code s.[ndx2] in
    let byte = if ndx mod 2 = 0 then bytes lsr 4 else bytes mod 0x10 in
    to_hex_digit byte
  in
  let hex_digits = String.init (2 * len) get_hex_digit in
  "0x" ^ hex_digits


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
      [ ("from", `String (hex_string_of_string (Address.to_string sender)))
      ; ("to", `String (hex_string_of_string (Address.to_string recipient)))
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
    | Main_chain.CreateContract code -> hex_string_of_string (Bytes.to_string code)
    | _ -> raise (Internal_error "Expected CreateContract operation")
  in
  let gas = tx_header.gas_limit in
  let gas_price = tx_header.gas_price in
  let params =
    `Assoc
      [ ("from", `String (hex_string_of_string (Address.to_string sender)))
      ; ("gas", `String (Printf.sprintf "0x%Lx" (TokenAmount.to_int64 gas)))
      ; ("gasPrice", `String (Printf.sprintf "0x%Lx" (TokenAmount.to_int64 gas_price)))
      ; ("value", `String (Printf.sprintf "0x%Lx" (TokenAmount.to_int64 TokenAmount.zero)))
        (* ignored *)
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
      [ ("from", `String (hex_string_of_string (Address.to_string sender)))
      ; ("to", `String (hex_string_of_string (Address.to_string contract_address)))
      ; ("gas", `String (Printf.sprintf "0x%Lx" (TokenAmount.to_int64 gas)))
      ; ("gasPrice", `String (Printf.sprintf "0x%Lx" (TokenAmount.to_int64 gas_price)))
      ; ("data", `String (hex_string_of_string (Bytes.to_string call_hash))) ]
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
  let params = [`String (hex_string_of_string (Address.to_string address)); `String "latest"] in
  let json = build_json_rpc_call Eth_getBalance params in
  send_rpc_call_to_net json


(* for testing only; all test accounts have empty password *)
let unlock_account address =
  let params = [`String (hex_string_of_string (Address.to_string address)); `String ""; `Int 5] in
  let json = build_json_rpc_call Personal_unlockAccount params in
  send_rpc_call_to_net json


(* test utilities *)
let json_contains_error json =
  match Yojson.Basic.Util.member "error" json with `Null -> false | _ -> true


let json_result_to_int json =
  int_of_string (Yojson.Basic.Util.to_string (Yojson.Basic.Util.member "result" json))


[%%test
let "transfer-on-Ethereum-testnet" =
  let open Main_chain in
  (* accounts on test net *)
  let sender_hex = "0x44e3a10528a0d5a41c86ee1bf39d60a6705911b9" in
  let sender_address = Address.of_string (string_of_hex_string sender_hex) in
  let recipient_hex = "0xa17a5c51b9d9938da47afade37f3cfab0fd5ce2d" in
  let recipient_address = Address.of_string (string_of_hex_string recipient_hex) in
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
    ; nonce= Nonce.of_int 2
    ; gas_price= TokenAmount.of_int 2
    ; gas_limit= TokenAmount.of_int 1000000
    ; value= TokenAmount.of_int transfer_amount }
  in
  let operation = Main_chain.TransferTokens recipient_address in
  let transaction = {tx_header; operation} in
  (* send tokens *)
  let output = Lwt_main.run (send_transaction_to_net transaction) in
  assert (not (json_contains_error output)) ;
  true]

[%%test
let "create-contract-on-Ethereum-testnet" =
  let open Main_chain in
  (* account on test net *)
  let sender_hex = "0xa17a5c51b9d9938da47afade37f3cfab0fd5ce2d" in
  let sender_address = Address.of_string (string_of_hex_string sender_hex) in
  (* unlock accounts *)
  let unlock_sender_json = Lwt_main.run (unlock_account sender_address) in
  assert (not (json_contains_error unlock_sender_json)) ;
  (* get opening balance *)
  let tx_header =
    { sender= sender_address
    ; nonce= Nonce.of_int 42
    ; gas_price= TokenAmount.of_int 2
    ; gas_limit= TokenAmount.of_int 1000000
    ; value= TokenAmount.zero (* TODO: should this be an option type? *) }
  in
  (* a valid contract contains compiled EVM code
     for testing, we just use a buffer with arbitrary contents
   *)
  let operation = Main_chain.CreateContract (Bytes.create 128) in
  let transaction = {tx_header; operation} in
  (* create contract *)
  let output = Lwt_main.run (send_transaction_to_net transaction) in
  assert (not (json_contains_error output)) ;
  true]

[%%test
let "call-contract-on-Ethereum-testnet" =
  let open Main_chain in
  (* account on test net *)
  let sender_hex = "0x08cb396ebfd4c6ef9c5f3ca77f9bde354762655d" in
  let sender_address = Address.of_string (string_of_hex_string sender_hex) in
  (* unlock accounts *)
  let unlock_sender_json = Lwt_main.run (unlock_account sender_address) in
  assert (not (json_contains_error unlock_sender_json)) ;
  (* get opening balance *)
  let tx_header =
    { sender= sender_address
    ; nonce= Nonce.of_int 53
    ; gas_price= TokenAmount.of_int 2
    ; gas_limit= TokenAmount.of_int 1000000
    ; value= TokenAmount.zero }
  in
  (* for CallFunction:

     address should be a valid contract address
     for testing, it's a dummy address

     the bytes are a 4-byte prefix of the Keccak256 hash of the encoding of a method
     signature, followed by the encoding of the method parameters, as described at:

       https://github.com/ethereum/wiki/wiki/Ethereum-Contract-ABI

     This data tells the EVM which method to call, with what arguments, in the contract

     in this test, we just use a dummy hash to represent all of that
   *)
  let hashed = Digest.make "some arbitrary string" in
  let operation =
    Main_chain.CallFunction
      ( Address.of_string (string_of_hex_string "0x2B1c40cD23AAB27F59f7874A1F454748B004C4D8")
      , Bytes.of_string (Digest.to_string hashed) )
  in
  let transaction = {tx_header; operation} in
  let output = Lwt_main.run (send_transaction_to_net transaction) in
  assert (not (json_contains_error output)) ;
  true]

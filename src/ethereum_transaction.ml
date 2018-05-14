open Legibase
open Lib
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson
module TokenAmount = Main_chain.TokenAmount

let ethereum_net = Uri.make ~scheme:"http" ~host:"localhost" ~port:8080 ()

type ethereum_rpc_call = Eth_sendTransaction [@@deriving show]

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

let id_counter = ref 1

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
    | _ -> raise (Internal_error "Expected TransferTokens transaction")
  in
  let gas = tx_header.gas_limit in
  let gas_price = tx_header.gas_price in
  let value = tx_header.value in
  let _ = Printf.printf "sender addr: %s\n%!" (Address.to_hex_string sender) in
  let params =
    `Assoc
      [ ("from", `String (Address.to_hex_string sender))
      ; ("to", `String (Address.to_hex_string recipient))
      ; ("gas", `String (Format.sprintf "0x%Lx" (TokenAmount.to_int64 gas)))
      ; ("gasPrice", `String (Format.sprintf "0x%Lx" (TokenAmount.to_int64 gas_price)))
      ; ("value", `String (Format.sprintf "0x%Lx" (TokenAmount.to_int64 value)))
      ; ("data", `String "0x00")
      (* TODO: fill in with hash of transaction *)
       ]
  in
  build_json_rpc_call Eth_sendTransaction [params]

let build_create_contract_json transaction = bottom ()

let build_call_function_json transaction = bottom ()

let build_transaction_json transaction_signed =
  let open Main_chain in
  let transaction = transaction_signed.payload in
  match transaction.operation with
  | TransferTokens _ -> build_transfer_tokens_json transaction
  | CreateContract _ -> build_create_contract_json transaction
  | CallFunction _ -> build_call_function_json transaction

let send_transaction_to_net transaction_signed =
  Printf.printf "Building JSON\n%!" ;
  let json = Yojson.to_string (build_transaction_json transaction_signed) in
  Printf.printf "JSON: %s\n%!" json ;
  Client.post
    ~body:(Cohttp_lwt__.Body.of_string json)
    ~headers:(Cohttp.Header.add (Cohttp.Header.init ()) "Content-Type" "application/json")
    ethereum_net
  >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code ;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string) ;
  body |> Cohttp_lwt.Body.to_string >|= fun body -> Printf.printf "Body: %s\n" body ; body

let go () =
  let open Main_chain in
  let sender = Address.of_hex_string "0xa4a32bdac80257a79250df366935e5f49d41e780" in
  let tx_header =
    { sender
    ; nonce= Nonce.of_int 2
    ; gas_price= TokenAmount.of_int 2
    ; gas_limit= TokenAmount.of_int 1000000
    ; value= TokenAmount.of_int 22 }
  in
  let recipient = Address.of_hex_string "0xa4a32bdac80257a79250df366935e5f49d41e780" in
  let operation = Main_chain.TransferTokens recipient in
  let transaction = {tx_header; operation} in
  let alice_keys =
    Keypair.make_keys_from_hex
      "d5:69:84:dc:08:3d:76:97:01:71:4e:eb:1d:4c:47:a4:54:25:5a:3b:bc:3e:9f:44:84:20:8c:52:bd:a3:b6:4e"
      "04:23:a7:cd:9a:03:fa:9c:58:57:e5:14:ae:5a:cb:18:ca:91:e0:7d:69:45:3e:d8:51:36:ea:6a:00:36:10:67:b8:60:a5:b2:0f:11:53:33:3a:ef:2d:1b:a1:3b:1d:7a:52:de:28:69:d1:f6:23:71:bf:81:bf:80:3c:21:c6:7a:ca"
  in
  let signed_transaction = sign alice_keys.private_key transaction in
  Lwt_main.run (send_transaction_to_net signed_transaction) ;
  ()

(*
let body =
  Client.get (Uri.of_string "https://www.reddit.com/")
  >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code ;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string) ;
  body |> Cohttp_lwt.Body.to_string
  >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body) ;
  body

let () =
  let body = Lwt_main.run body in
  print_endline ("Received body\n" ^ body)
 *)

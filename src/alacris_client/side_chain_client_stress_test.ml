open Scgi

open Legilogic_lib
open Signing
open Yojsoning
open Logging
open Types
open Lib
open Action
open Lwter (* TODO: use Lwt_exn *)

(* open Random *)

open Alacris_lib
open Side_chain

open Side_chain_client_lib
open Actions

(* Side_chain also has a Request module *)
module Request = Scgi.Request

let _ = Config.set_application_name "alacris"
let _ = set_log_file "logs/alacris-client.log"



(* TODO: use DepositWanted, WithdrawalWanted, PaymentWanted from side_chain_user, directly ? *)
type deposit_json =
  { address: Address.t
  ; amount: TokenAmount.t
  } [@@deriving yojson]

type withdrawal_json =
  { address: Address.t
  ; amount: TokenAmount.t
  } [@@deriving yojson]

type payment_json =
  { sender: Address.t
  ; recipient: Address.t
  ; amount: TokenAmount.t
  } [@@deriving yojson]

type address_json =
  { address: Address.t
  } [@@deriving yojson]

let handle_lwt_exception exn =
  Logging.log "Got LWT exception: %s"
    (Printexc.to_string exn)

(* port and address must match "scgi_pass" in nginx/conf/scgi.conf *)
let port = 1025
let address = "127.0.0.1"

let return_json id status json =
  let headers =  [`Content_type "application/json"] in
  let body = `String (string_of_yojson json) in
  let response = Response.{status; headers; body} in
  log "[\"RESPONSE\", %d, %s]" id (Response.to_debug_string response);
  Lwt.return response

let ok_json id json = return_json id `Ok json

let bad_request id json = return_json id `Bad_request json

let internal_error id json = return_json id `Internal_server_error json

let bad_request_method id methodz =
  let json = error_json "Invalid HTTP method: %s" (Http_method.to_string methodz) in
  bad_request id json

let invalid_api_call id methodz call =
  let json = error_json "No such %s API call: %s" methodz call in
  bad_request id json

let invalid_get_api_call id call = invalid_api_call id "GET" call
let invalid_post_api_call id call = invalid_api_call id "POST" call

let bad_request_response id msg =
  bad_request id (error_json "%s" msg)

let error_response id msg =
  ok_json id (error_json "%s" msg)

let internal_error_response id msg =
  internal_error id (error_json "%s" msg)

let return_result id json_or_exn =
  match json_or_exn with
  | Ok json -> ok_json id json
  | Error exn -> Printexc.to_string exn
                 |> error_response id

let trent_address = Signing.Test.trent_address


let void_return = 
  let sum = ref 0 in
  for i = 0 to 10 do
    sum := !sum + i
  done

let void_return_ins _ = 
  let sum = ref 0 in
  for i = 0 to 10 do
    sum := !sum + i
  done

let get_keys_filename = "demo-keys-small.json" |> Config.get_config_filename



let random_deposit_oper laddr = 
  let deposit_idx = Random.int (List.length laddr) and deposit_amnt = Random.int 100 in
    let deposit_addr = List.nth laddr deposit_idx and deposit_amnt256 = UInt256.of_int deposit_amnt in 
      void_return_ins (deposit_to ~facilitator:trent_address deposit_addr deposit_amnt256)



let random_withdrawal_oper laddr =
 let withdrawal_idx = Random.int (List.length laddr) and withdrawal_amnt = Random.int 100 in
  let withdrawal_addr = List.nth laddr withdrawal_idx and withdrawal_amnt256 = UInt256.of_int withdrawal_amnt in
   void_return_ins (withdrawal_from ~facilitator:trent_address withdrawal_addr withdrawal_amnt256)

let random_payment_oper laddr =
  let recipient_idx = Random.int (List.length laddr) and sender_idx = Random.int (List.length laddr) and payment_amnt = Random.int 100 in
    let recipient_addr = List.nth laddr recipient_idx and sender_addr = List.nth laddr sender_idx and payment_amnt256 = UInt256.of_int payment_amnt in
      void_return_ins (payment_on ~facilitator:trent_address sender_addr recipient_addr payment_amnt256 "memo")


let random_balance_oper laddr = 
  let queried_idx = Random.int (List.length laddr) in
    let queried_addr = List.nth laddr queried_idx in 
      void_return_ins (get_balance_on ~facilitator:trent_address queried_addr)


let random_recent_transaction_oper = void_return_ins

let random_status_oper laddr = 
  let queried_idx = Random.int (List.length laddr) in
    let queried_addr = List.nth laddr queried_idx in 
      void_return_ins (get_status_on_trent_and_main_chain queried_addr)


let random_single_oper laddr =
    let pos = Random.int 6 in
    match pos with
    | 0 -> random_deposit_oper laddr
    | 1 -> random_withdrawal_oper laddr
    | 2 -> random_payment_oper laddr
    | 3 -> random_balance_oper laddr
    | 4 -> random_recent_transaction_oper laddr
    | _ -> random_status_oper laddr


let get_address (_,keypair) =
  keypair.Keypair.address

let get_list_address : string -> address list = fun file ->
  Yojsoning.yojson_of_file file
  |> decode_keypairs
  |> List.map get_address



let _ =
  let keys_file = get_keys_filename in
   let list_addr = get_list_address keys_file in
     let nb_iteration = 1000 in
       for _ = 0 to nb_iteration do
         random_single_oper list_addr
       done

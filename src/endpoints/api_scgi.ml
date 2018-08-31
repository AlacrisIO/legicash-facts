open Scgi
open Lwt

open Legilogic_lib
open Signing
open Yojsoning
open Logging

open Legicash_lib
open Side_chain

open Endpoints
open Accounts
open Actions

(* Side_chain also has a Request module *)
module Request = Scgi.Request

let _ = log_to_file "logs/legicash.log"

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

let _ =
  let request_counter =
    let counter = ref 0 in
    fun () -> let n = !counter + 1 in counter := n ; n in

  let handle_request request =
    (* Log the request *)
    let id = request_counter () in
    log "[\"REQUEST\", %d, %s]" id (Request.to_debug_string request);
    let uri = Request.path request in (* /api/somemethod *)
    let api_call = String.sub uri 5 (String.length uri - 5) in (* remove /api/ *)
    match Request.meth request with
    | `GET ->
      begin
        match api_call with
          "balances" ->
          get_all_balances_on_trent ()
          >>= ok_json id
        | "tps" ->
          let result_json = get_transaction_rate_on_trent () in
          ok_json id result_json
        | "proof" ->
          (match Request.param request "tx-revision" with
           | Some param ->
             (try
                let result_json = get_proof (int_of_string param) in
                ok_json id result_json
              with
              | Failure msg when msg = "int_of_string" ->
                bad_request_response id ("Invalid tx-revision: " ^ param)
              | exn ->
                internal_error_response id (Printexc.to_string exn))
           | None -> bad_request_response id "Expected one parameter, tx-revision")
        | "thread" ->
          (match Request.param request "id" with
             Some param ->
             (try
                let result_json = apply_main_chain_thread (int_of_string param) in
                ok_json id result_json
              with
              | Failure msg when msg = "int_of_string" ->
                bad_request_response id ("Invalid id: " ^ param)
              | exn ->
                internal_error_response id (Printexc.to_string exn))
           | None ->
             bad_request_response id "Expected one parameter, id")
        | _ -> invalid_get_api_call id api_call
      end
    | `POST ->
      let json = yojson_of_string (Request.contents request) in
      begin
        match api_call with
        | "deposit" ->
          let maybe_deposit = deposit_json_of_yojson json in
          (match maybe_deposit with
           | Ok deposit ->
             (try
                let result_json = deposit_to_trent deposit.address deposit.amount in
                ok_json id result_json
              with
              | Lib.Internal_error msg -> internal_error_response id msg
              | exn -> internal_error_response id (Printexc.to_string exn))
           | Error msg -> error_response id msg)
        | "withdrawal" ->
          let maybe_withdrawal = withdrawal_json_of_yojson json in
          (match maybe_withdrawal with
           | Ok withdrawal ->
             (try
                let result_json = withdrawal_from_trent withdrawal.address withdrawal.amount in
                ok_json id result_json
              with
              | Lib.Internal_error msg -> internal_error_response id msg
              | exn -> internal_error_response id (Printexc.to_string exn))
           | Error msg -> error_response id msg)
        | "payment" ->
          let maybe_payment = payment_json_of_yojson json in
          (match maybe_payment with
           | Ok payment ->
             (try
                payment_on_trent payment.sender payment.recipient payment.amount
                >>= ok_json id
              with
              | Lib.Internal_error msg -> internal_error_response id msg
              | exn -> internal_error_response id (Printexc.to_string exn))
           | Error msg -> error_response id msg)
        | "balance" ->
          let maybe_address = address_json_of_yojson json in
          (match maybe_address with
           | Ok address_record ->
             (try
                let result_json = get_balance_on_trent address_record.address in
                ok_json id result_json
              with
              | Lib.Internal_error msg -> internal_error_response id msg
              | exn -> internal_error_response id (Printexc.to_string exn))
           | Error msg -> error_response id msg)
        | "recent_transactions" ->
          let maybe_limit_string = Request.param request "limit" in
          let invalid_limit = Some (-1) in
          let maybe_limit =
            match maybe_limit_string with
            | Some s ->
              (try
                 let limit = int_of_string s in
                 if limit < 0 then raise (Failure "bad limit");
                 Some limit
               with Failure _ ->
                 (* limit string not parseable as a number, or negative *)
                 invalid_limit)
            | None -> None
          in
          if maybe_limit = invalid_limit then
            bad_request_response id ("Invalid limit value: " ^ (Lib.Option.get maybe_limit_string))
          else
            let maybe_address = address_json_of_yojson json in
            (match maybe_address with
             | Ok address_record ->
               let result_json = get_recent_transactions_on_trent address_record.address maybe_limit in
               ok_json id result_json
             | Error msg -> error_response id msg)
        | "status" ->
          let maybe_address = address_json_of_yojson json in
          (match maybe_address with
           | Ok address_record ->
             (try
                get_status_on_trent_and_main_chain address_record.address
                >>= ok_json id
              with
              | Lib.Internal_error msg -> internal_error_response id msg
              | exn -> internal_error_response id (Printexc.to_string exn))
           | Error msg -> error_response id msg)
        | other_call -> invalid_post_api_call id other_call
      end
    (* neither GET nor POST *)
    | methodz -> bad_request_method id methodz
  in
  let _ = Server.handler_inet address port handle_request in
  (* run forever in Lwt monad *)
  Lwt_main.run (
    prepare_server ()
    >>= fun () ->
    (fst (Lwt.wait ())))

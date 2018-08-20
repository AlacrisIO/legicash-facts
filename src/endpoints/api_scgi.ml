open Legicash_lib
open Endpoints_lib
open Actions
open Scgi
open Lwt
open Yojsoning

type deposit_json =
  { address: string
  ; amount: int
  } [@@deriving yojson]

type withdrawal_json =
  { address: string
  ; amount: int
  } [@@deriving yojson]

type payment_json =
  { sender: string
  ; recipient: string
  ; amount: int
  } [@@deriving yojson]

type address_json =
  { address: string
  } [@@deriving yojson]

(* port and address must match "scgi_pass" in nginx/conf/scgi.conf *)
let port = 1025
let address = "127.0.0.1"

let return_json status json =
  Lwt.return
    { Response.status = status
    ; headers = [`Content_type "application/json"]
    ; body = `String (string_of_yojson json)
    }

let ok_json json = return_json `Ok json

let bad_request json = return_json `Bad_request json

let internal_error json = return_json `Internal_server_error json

let error_json msg = `Assoc [("error",`String msg)]

let bad_request_method methodz =
  let json = error_json ("Invalid HTTP method: " ^ (Http_method.to_string methodz)) in
  bad_request json

let invalid_api_call methodz call =
  let json = error_json ("No such " ^ methodz ^ " API call: " ^ call) in
  bad_request json

let invalid_get_api_call call = invalid_api_call "GET" call
let invalid_post_api_call call = invalid_api_call "POST" call

let bad_request_response msg =
  let json = error_json msg in
  bad_request json

let error_response msg =
  let json = error_json msg in
  ok_json json

let internal_error_response msg =
  Printf.eprintf "INTERNAL: %s\n%!" msg;
  let json = error_json msg in
  internal_error json

let _ =
  let handle_request request =
    let uri = Request.path request in (* /api/somemethod *)
    let api_call = String.sub uri 5 (String.length uri - 5) in (* remove /api/ *)
    match Request.meth request with
    | `GET ->
      begin
        match api_call with
          "balances" ->
          get_all_balances_on_trent ()
          >>= ok_json
        | "tps" ->
          let result_json = get_transaction_rate_on_trent () in
          ok_json result_json
        | "proof" ->
          (match Request.param request "tx-revision" with
           | Some param ->
             (try
                let result_json = get_proof (int_of_string param) in
                ok_json result_json
              with
              | Failure msg when msg = "int_of_string" ->
                bad_request_response ("Invalid tx-revision: " ^ param)
              | exn ->
                internal_error_response (Printexc.to_string exn))
           | None -> bad_request_response ("Expected one parameter, tx-revision"))
        | "thread" ->
          (match Request.param request "id" with
             Some param ->
             (try
                let result_json = apply_main_chain_thread (int_of_string param) in
                ok_json result_json
              with
              | Failure msg when msg = "int_of_string" ->
                bad_request_response ("Invalid id: " ^ param)
              | exn ->
                internal_error_response (Printexc.to_string exn))
           | None ->
             bad_request_response "Expected one parameter, id")
        | _ -> invalid_get_api_call api_call
      end
    | `POST ->
      let json = Yojson.Safe.from_string (Request.contents request) in
      begin
        match api_call with
        | "deposit" ->
          let maybe_deposit = deposit_json_of_yojson json in
          (match maybe_deposit with
           | Ok deposit ->
             (try
                let result_json = deposit_to_trent deposit.address deposit.amount in
                ok_json result_json
              with
              | Lib.Internal_error msg -> internal_error_response msg
              | exn -> internal_error_response (Printexc.to_string exn))
           | Error msg -> error_response msg)
        | "withdrawal" ->
          let maybe_withdrawal = withdrawal_json_of_yojson json in
          (match maybe_withdrawal with
           | Ok withdrawal ->
             (try
                let result_json = withdrawal_from_trent withdrawal.address withdrawal.amount in
                ok_json result_json
              with
              | Lib.Internal_error msg -> internal_error_response msg
              | exn -> internal_error_response (Printexc.to_string exn))
           | Error msg -> error_response msg)
        | "payment" ->
          let maybe_payment = payment_json_of_yojson json in
          (match maybe_payment with
           | Ok payment ->
             (try
                payment_on_trent payment.sender payment.recipient payment.amount
                >>= ok_json
              with
              | Lib.Internal_error msg -> internal_error_response msg
              | exn -> internal_error_response (Printexc.to_string exn))
           | Error msg -> error_response msg)
        | "balance" ->
          let maybe_address = address_json_of_yojson json in
          (match maybe_address with
           | Ok address_record ->
             (try
                let result_json = get_balance_on_trent address_record.address in
                ok_json result_json
              with
              | Lib.Internal_error msg -> internal_error_response msg
              | exn -> internal_error_response (Printexc.to_string exn))
           | Error msg -> error_response msg)
        | other_call -> invalid_post_api_call other_call
      end
    (* neither GET nor POST *)
    | methodz -> bad_request_method methodz
  in
  let _ = Server.handler_inet address port handle_request in
  (* run forever in Lwt monad *)
  Lwt_main.run (fst (Lwt.wait ()))

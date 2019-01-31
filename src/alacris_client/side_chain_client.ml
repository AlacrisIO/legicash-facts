open Scgi

open Legilogic_lib
open Signing
open Yojsoning
open Logging
open Types
open Action
open Lwter (* TODO: use Lwt_exn *)

open Alacris_lib
open Side_chain

open Side_chain_client_lib
open Actions

(* Side_chain also has a Request module *)
module Request = Scgi.Request

let _ = Config.set_application_name "alacris"
(* let _ = set_log_file "logs/alacris-client.log" *)

(* TODO: before we got to production, make sure keys at rest are suitably encrypted *)
let _ =
  let keys_file_ref = ref ("demo-keys-small.json" |> Config.get_config_filename) in
  let args = ref [] in
  Arg.parse_argv Sys.argv
    [("--keys", Set_string keys_file_ref, "file containing keys to the managed accounts")]
    (fun x -> args := x :: !args)
    "side_chain_client.exe";
  register_file_keypairs !keys_file_ref

(* let account_names = nicknames_with_registered_keypair () |> List.sort compare *)


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

let throw_if_err = function
  | Ok d    -> Lwt.return d
  | Error e -> raise e

let trent_address = Signing.Test.trent_address

let _ =
  let request_counter =
    let counter = ref 0 in
    fun () -> let n = !counter + 1 in counter := n ; n in

  (* just log Lwt exceptions *)
  let _ = Lwt.async_exception_hook := handle_lwt_exception in

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
          Logging.log "GET: balances";
          get_all_balances_on_trent ()
          >>= return_result id
        | "tps" ->
          Logging.log "GET: tps";
          get_transaction_rate_on_trent ()
          |> ok_json id
        | "proof" ->
          Logging.log "GET: proof";
          (match Request.param request "tx-revision" with
           | Some param -> (
               try
                 get_proof (Revision.of_string param)
                 >>= return_result id
               with
               | Failure msg when msg = "int_of_string" ->
                 bad_request_response id ("Invalid tx-revision: " ^ param)
               | exn ->
                 internal_error_response id (Printexc.to_string exn))
           | None -> bad_request_response id "Expected one parameter, tx-revision")
        | "thread" ->
          Logging.log "GET: thread";
          (match Request.param request "id" with
             Some param ->
             (try
                apply_main_chain_thread (int_of_string param)
                |> ok_json id
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

      let (=->) deserialized f =
        Logging.log "POST: %s" api_call;
        let err500 m = internal_error_response id m in

        match (deserialized json) with
          | Error msg -> error_response id msg
          | Ok d      ->
              try f d >>= ok_json id
              with | Lib.Internal_error msg -> err500 msg
                   | exn                    -> err500 (Printexc.to_string exn)
      in

      begin
        match api_call with

        | "deposit" -> deposit_json_of_yojson =-> fun d ->
            Lwt.return @@ deposit_to ~operator:trent_address
                                     d.address
                                     d.amount

        | "withdrawal" -> withdrawal_json_of_yojson =-> fun d ->
            Lwt.return @@ withdrawal_from ~operator:trent_address
                                          d.address
                                          d.amount


        | "payment" -> payment_json_of_yojson =-> fun d ->
            Lwt.return @@ payment_on ~operator:trent_address
                                     d.sender
                                     d.recipient
                                     d.amount
                                     "memo"

        | "balance" -> address_json_of_yojson =-> fun d ->
            get_balance_on ~operator:trent_address
                           d.address
                >>= throw_if_err

        | "status" -> address_json_of_yojson =-> fun d ->
            get_status_on_trent_and_main_chain d.address
                >>= throw_if_err

        | "recent_transactions" ->
          Logging.log "POST: recent_transactions";
          let maybe_limit_string = Request.param request "limit" in
          let invalid_limit = Some (Revision.zero) in
          let maybe_limit =
            match maybe_limit_string with
            | Some s ->
              (try
                 let limit = Revision.of_string s in
                 if Revision.compare limit Revision.zero <= 0 then raise (Failure "bad limit");
                 Some limit
               with Failure _ ->
                 (* limit string not parseable as a number, or nonpositive *)
                 invalid_limit)
            | None -> None
          in
          if maybe_limit = invalid_limit then
            bad_request_response id ("Invalid limit value: " ^ (Lib.Option.get maybe_limit_string))
          else
            let maybe_address = address_json_of_yojson json in
            (match maybe_address with
             | Ok address_record -> (
                 get_recent_user_transactions_on_trent address_record.address maybe_limit
                 >>= return_result id)
             | Error msg -> error_response id msg)

        | "sleep" ->
          Logging.log "POST: sleep";
          (try
             Lwt_unix.sleep 1.0 >>= (fun () -> ok_json id (`Int 42))
           with
           | Lib.Internal_error msg -> internal_error_response id msg
           | exn -> internal_error_response id (Printexc.to_string exn))
        | other_call -> invalid_post_api_call id other_call
      end
    (* neither GET nor POST *)
    | methodz -> bad_request_method id methodz
  in
  let _ = Server.handler_inet address port handle_request in
  (* run forever in Lwt monad *)
  Db.run ~db_name:"alacris_client_db"
    (fun () -> fst (Lwt.wait ()))

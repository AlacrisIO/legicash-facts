open Legicash_lib
open Endpoints_lib
open Actions
open Netcgi
open Yojson

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

type error_message = {
  error : string;
} [@@deriving yojson]

exception SCGI_error of string

let amp_re = Str.regexp "&"
let eq_re = Str.regexp "="

let return_result (cgi:cgi) result_json =
  cgi#set_header
    ~status:`Ok
    ~cache:`No_cache ();
  cgi # out_channel # output_string (Safe.to_string result_json);
  cgi # out_channel # commit_work ()

let make_error_json msg = `Assoc [("error",`String msg)]

let failure_response (cgi:cgi) msg =
  cgi#set_header
    ~status:`Not_found
    ~cache:`No_cache ();
  cgi # out_channel # output_string (Basic.to_string (make_error_json msg));
  cgi # out_channel # commit_work ()

let invalid_api_call (cgi:cgi) call =
  cgi#set_header
    ~status:`Not_found
    ~cache:`No_cache ();
  cgi # out_channel # output_string ("No such API call: " ^ call);
  cgi # out_channel # commit_work ()

let bad_request_method (cgi:cgi) =
  cgi#set_header
    ~status:`Bad_request
    ~cache:`No_cache ();
  cgi # out_channel # output_string "Only GET and POST are allowed";
  cgi # out_channel # commit_work ()

let handler (cgi:cgi) =
  let uri = cgi#environment#cgi_property "DOCUMENT_URI" in (* /api/somemethod *)
  (* the Nginx SCGI conf guarantees the uri begins with "/api/" *)
  let api_call = String.sub uri 5 (String.length uri - 5) in
  (* query parameters are "parm1=xxx&parm2=xxx& ..." *)
  let raw_parameters = cgi#environment#cgi_query_string in
  let parameters =
    List.map
      (fun param ->
         match Str.split eq_re param with
         | [k ; v] -> (k, v)
         | _ -> raise (SCGI_error "Not a valid parameter"))
      (Str.split amp_re raw_parameters)
  in
  match cgi#request_method with
  | `GET ->
    (match api_call with
       "balances" ->
       let result_json = get_all_balances_on_trent () in
       return_result cgi result_json
     | "tps" ->
       let result_json = get_transaction_rate_on_trent () in
       return_result cgi result_json
     | "proof" ->
       if parameters != [] && List.tl parameters = [] then
         let (key,value) = List.hd parameters in
         if key = "tx-revision" then
           let tx_revision = int_of_string value in
           let result_json = get_proof tx_revision in
           return_result cgi result_json
         else
           failure_response cgi ("Expected tx-revision parameter, got: " ^ key)
       else
         failure_response cgi ("Expected one parameter, tx-revision")
     | "thread" ->
       if parameters != [] && List.tl parameters = [] then
         let (key,value) = List.hd parameters in
         if key = "id" then
           let id = int_of_string value in
           let result_json = apply_main_chain_thread id in
           return_result cgi result_json
         else
           failure_response cgi ("Expected id parameter, got: " ^ key)
       else
         failure_response cgi ("Expected one parameter, id")
     | other_call -> invalid_api_call cgi other_call)
  | `POST ->
    let body = (cgi#argument "BODY")#value in
    let json = Yojson.Safe.from_string body in
    (match api_call with
     | "deposit" ->
       let maybe_deposit = deposit_json_of_yojson json in
       (match maybe_deposit with
        | Ok deposit ->
          (try
             let result_json = deposit_to_trent deposit.address deposit.amount in
             return_result cgi result_json
           with (Lib.Internal_error msg) -> failure_response cgi msg)
        | Error msg -> failure_response cgi msg)
     | "withdrawal" ->
       let maybe_withdrawal = withdrawal_json_of_yojson json in
       (match maybe_withdrawal with
        | Ok withdrawal ->
          (try
             let result_json = withdrawal_from_trent withdrawal.address withdrawal.amount in
             return_result cgi result_json
           with (Lib.Internal_error msg) -> failure_response cgi msg)
        | Error msg -> failure_response cgi msg)
     | "payment" ->
       let maybe_payment = payment_json_of_yojson json in
       (match maybe_payment with
        | Ok payment ->
          (try
             let result_json = payment_on_trent payment.sender payment.recipient payment.amount in
             return_result cgi result_json
           with (Lib.Internal_error msg) -> failure_response cgi msg)
        | Error msg -> failure_response cgi msg)
     | "balance" ->
       let maybe_address = address_json_of_yojson json in
       (match maybe_address with
        | Ok address_record ->
          let result_json = get_balance_on_trent address_record.address in
          return_result cgi result_json
        | Error msg -> failure_response cgi msg)
     | other_call -> invalid_api_call cgi other_call)
  (* neither GET nor POST *)
  | _ -> bad_request_method cgi (* should be unreachable, because SCGI config disallows it *)


let api_config = { default_config with
                   permitted_input_content_types =
                     "application/json"::default_config.permitted_input_content_types;
                   permitted_http_methods = [`GET; `POST ]
                 }

let _ =
  Netcgi_scgi.run
    ~config:api_config
    (* must match "scgi_pass" in nginx/conf/scgi.conf *)
    ~sockaddr:(ADDR_INET (Unix.inet_addr_of_string "127.0.0.1",1025))
    handler

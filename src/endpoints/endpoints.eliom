(* endpoints.eliom -- Legicash REST API endpoints *)

open Yojson
open Legicash_lib
open Lib
open Lwt

module TokenAmount = Main_chain.TokenAmount

(**** Data types ****)

type json_signature = {
  signature : string
}
[@@deriving yojson]

type deposit_json =
  { address: string
  ; amount: int
  } [@@deriving yojson]

type withdrawal_json =
  { address: string
  ; amount: int
  } [@@deriving yojson]

type balance_json =
  { address: string
  } [@@deriving yojson]

type payment_json =
  { sender: string
  ; recipient: string
  ; amount: int
  } [@@deriving yojson]

type error_message = {
  error : string;
} [@@deriving yojson]

(**** Services ****)

let path = Eliom_service.Path ["api"]

let make_params name = 
  Eliom_parameter.(suffix (suffix_const name))

let make_post_service name =
  Eliom_service.create
    ~path
    ~meth:(Eliom_service.Post (make_params name,Eliom_parameter.raw_post_data))
    ()

let make_get_service name =
    Eliom_service.create
    ~path
    ~meth:(Eliom_service.Get (make_params name))
    ()

let deposit_service = make_post_service "deposit"

let withdrawal_service = make_post_service "withdrawal"

let payment_service = make_post_service "payment"

let balance_service = make_post_service "balance"

let balances_service = make_get_service "balances"

(* a thread's address is api/thread?id=nnn *)
let thread_params = Eliom_parameter.(suffix_prod (suffix_const "thread") (int "id"))

let thread_service =
  Eliom_service.create
    ~path
    ~meth:(Eliom_service.Get thread_params)
    ()

(**** Handler helpers ****)

let json_mime_type = "application/json"

let send_json ~code json =
  Eliom_registration.String.send ~code (json, json_mime_type)

let send_error ~code error =
  let json = Yojson.Safe.to_string (error_message_to_yojson {error}) in
  send_json ~code json

let send_success () =
  Eliom_registration.String.send ~code:200 ("", "")

let check_content_type ~mime_type content_type =
  match content_type with
  | Some ((type_, subtype), _)
    when (type_ ^ "/" ^ subtype) = mime_type -> true
  | _ -> false

let read_raw_content ?(length = 4096) raw_content =
  let content_stream = Ocsigen_stream.get raw_content in
  Ocsigen_stream.string_of_stream length content_stream

let is_json_content content_type =
  check_content_type ~mime_type:json_mime_type content_type

let not_json_content_error () =
  send_error ~code:400 "Content-type is not JSON"

let missing_content_error operation =
  send_error ~code:400 (Format.sprintf "Missing content in %s request" operation)

let bad_signed_json error =
  send_error ~code:400 "Invalid signed JSON"

let bad_response msg =
  send_error ~code:400 msg

(**** Handlers ****)

let deposit_handler () (content_type,raw_content_opt) =
  let open Yojson.Safe in
  if is_json_content content_type then
    try
      match raw_content_opt with
      | None -> missing_content_error "deposit"
      | Some raw_content ->
        read_raw_content raw_content >>= fun json_string ->
        let json = from_string json_string in
        let maybe_deposit = deposit_json_of_yojson json in
        match maybe_deposit with
        | Ok deposit ->
          let result_json = Actions.deposit_to_trent deposit.address deposit.amount in
          send_json ~code:200 (Yojson.Safe.to_string result_json)
        | Error msg -> bad_response msg
    with Internal_error msg -> bad_response msg
       | exn -> bad_response (Printexc.to_string exn)
  else
    not_json_content_error ()

let withdrawal_handler () (content_type,raw_content_opt) =
  let open Yojson.Safe in
  if is_json_content content_type then
    try
      match raw_content_opt with
      | None -> missing_content_error "withdrawal"
      | Some raw_content ->
        read_raw_content raw_content >>= fun json_string ->
        let json = from_string json_string in
        let maybe_withdrawal = withdrawal_json_of_yojson json in
        match maybe_withdrawal with
        | Ok withdrawal ->
          let result_json = Actions.withdrawal_from_trent withdrawal.address withdrawal.amount in
          send_json ~code:200 (Yojson.Safe.to_string result_json)
        | Error msg -> bad_response msg
    with Internal_error msg -> bad_response msg
       | exn -> bad_response (Printexc.to_string exn)
  else
    not_json_content_error ()

let payment_handler () (content_type,raw_content_opt) =
  let open Yojson.Safe in
  if is_json_content content_type then
    try
      match raw_content_opt with
      | None -> missing_content_error "payment"
      | Some raw_content ->
        read_raw_content raw_content >>= fun json_string ->
        let json = from_string json_string in
        let maybe_payment = payment_json_of_yojson json in
        match maybe_payment with
        | Ok payment ->
          let result_json = Actions.payment_on_trent payment.sender payment.recipient payment.amount in
          send_json ~code:200 (Yojson.Safe.to_string result_json)
        | Error msg -> bad_response msg
    with Internal_error msg -> bad_response msg
       | exn -> bad_response (Printexc.to_string exn)
  else
    not_json_content_error ()

let balance_handler () (content_type,raw_content_opt) =
  let open Yojson.Safe in
  if is_json_content content_type then
    try
      match raw_content_opt with
    | None -> missing_content_error "balance"
    | Some raw_content ->
      read_raw_content raw_content >>= fun json_string ->
      let json = from_string json_string in
      let maybe_balance = balance_json_of_yojson json in
      match maybe_balance with
      | Ok balance ->
        let result_json = Actions.get_balance_on_trent balance.address in
        send_json ~code:200 (Yojson.Safe.to_string result_json)
      | Error msg -> bad_response msg
    with Internal_error msg -> bad_response msg
       | exn -> bad_response (Printexc.to_string exn)
  else
    not_json_content_error ()

let balances_handler balances () =
  try
    let result_json = Actions.get_all_balances_on_trent () in
    send_json ~code:200 (Yojson.Safe.to_string result_json)
    with Internal_error msg -> bad_response msg

let thread_handler (thread,id) () =
  try
    let result_json = Actions.apply_main_chain_thread id in
    send_json ~code:200 (Yojson.Safe.to_string result_json)
  with Internal_error msg -> bad_response msg

(* Register services *)

(* the types of POST and GET endpoint differ, keep in separate lists *)

let post_endpoints =
  [ (deposit_service, deposit_handler)
  ; (withdrawal_service, withdrawal_handler)
  ; (payment_service, payment_handler)
  ; (balance_service, balance_handler)
  ]

let get_endpoints =
  [ (balances_service, balances_handler) ]

let _ =
  let open Eliom_registration.Any in
  let _ = List.iter (fun (service,handler) -> register service handler) post_endpoints in
  let _ = List.iter (fun (service,handler) -> register service handler) get_endpoints in
  (* GET endpoint with a query parameter, so has its own type *)
  let _ = register thread_service thread_handler in
  ()

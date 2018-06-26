(* endpoints.eliom -- Legicash REST API endpoints *)

[%%shared
    open Eliom_lib
]

open Lwt

(**** Data types ****)

type json_signature = {
  signature : string
}
[@@deriving yojson]

type signed_json =
  { payload : string
  ; protected: string
  ; header : json_signature
  } [@@deriving yojson]

type error = {
  error_message : string;
} [@@deriving yojson]

(**** Services ****)

let path = Eliom_service.Path ["api"]

let deposit_params =
  Eliom_parameter.(suffix (suffix_const "deposit"))

let deposit_service =
  Eliom_service.create
    ~path
    ~meth:(Eliom_service.Post (deposit_params,Eliom_parameter.raw_post_data))
    ()

let transfer_params =
  Eliom_parameter.(suffix (suffix_const "transfer"))

let transfer_service =
  Eliom_service.create
    ~path
    ~meth:(Eliom_service.Post (transfer_params,Eliom_parameter.raw_post_data))
    ()

let balances_params =
  Eliom_parameter.(suffix (suffix_const "balances"))

let balances_service =
  Eliom_service.create
    ~path
    ~meth:(Eliom_service.Get balances_params)
    ()

(**** Handler helpers ****)

let json_mime_type = "application/json"

let send_json ~code json =
  Eliom_registration.String.send ~code (json, json_mime_type)

let send_error ~code error_message =
  let json = Yojson.Safe.to_string (error_to_yojson {error_message}) in
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

(**** Handlers ****)

let deposit_handler deposit content =
  send_json ~code:200 "deposit"

let transfer_handler transfer content =
  send_json ~code:200 "transfer"

let balances_handler balances () =
  send_json ~code:200 "balances"

(* Register services *)

let _ =
  let _ = Eliom_registration.Any.register deposit_service deposit_handler in
  let _ = Eliom_registration.Any.register transfer_service transfer_handler in
  let _ = Eliom_registration.Any.register balances_service balances_handler in
  ()

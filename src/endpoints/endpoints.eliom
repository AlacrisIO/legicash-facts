(* endpoints.eliom -- Legicash REST API endpoints *)

[%%shared
    open Eliom_lib
]

open Lwt
open Legibase
open Crypto

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
  error : string;
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

let send_error ~code error =
  let json = Yojson.Safe.to_string (error_to_yojson {error}) in
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

(**** Handlers ****)

let deposit_handler () (content_type,raw_content_opt) =
  if is_json_content content_type then
    match raw_content_opt with
    | None -> missing_content_error "deposit"
    | Some raw_content ->
      read_raw_content raw_content >>= fun json_string ->
      let json = Yojson.Safe.from_string json_string in
      let maybe_signed_deposit = signed_json_of_yojson json in
      match maybe_signed_deposit with
      | Ok signed_deposit ->
        let request_json_string = B64.decode signed_deposit.payload in
        let request_json = Yojson.Safe.from_string request_json_string in
        send_json ~code:200 (Yojson.Safe.to_string json)
      | Error _ -> bad_signed_json ()
  else
    not_json_content_error ()

let transfer_handler () (content_type,raw_content_opt) =
  if is_json_content content_type then
    match raw_content_opt with
    | None -> missing_content_error "transfer"
    | Some raw_content ->
      read_raw_content raw_content >>= fun json_string ->
      let json = Yojson.Safe.from_string json_string in
      let maybe_signed_transfer = signed_json_of_yojson json in
      match maybe_signed_transfer with
      | Ok signed_transfer ->
        let request_json_string = B64.decode signed_transfer.payload in
        let request_json = Yojson.Safe.from_string request_json_string in
        send_json ~code:200 (Yojson.Safe.to_string json)
      | Error _ -> bad_signed_json ()
  else
    not_json_content_error ()

let number_of_accounts = 26 (* TODO : dummy value *)
let address_to_account_tbl = Hashtbl.create number_of_accounts

let dump_facilitator_accounts facilitator_state =
  let open Yojson in
  let open Side_chain in
  let open Main_chain in
  let account_map = facilitator_state.current.accounts in
  let account_bindings = AddressMap.bindings account_map in
  let accounts =
    List.map
      (fun (address, account_state) ->
         (Hashtbl.find address_to_account_tbl address, TokenAmount.to_int64 account_state.balance)
      )
      account_bindings
  in
  let sorted_accounts = List.sort (fun (name1, _) (name2, _) -> compare name1 name2) accounts in 
  let make_balance_json (account,balance) =
    `Assoc
      [("tag",`String "account_balance")
      ; ("account",`String account)
      ; ("balance",`Int (Int64.to_int balance))
      ]
  in
  let balances = List.map make_balance_json sorted_accounts in
  `Assoc [("account_balances",`List balances)]

let balances_handler balances () =
    

send_json ~code:200 "balances"

(* Register services *)

let _ =
  let _ = Eliom_registration.Any.register deposit_service deposit_handler in
  let _ = Eliom_registration.Any.register transfer_service transfer_handler in
  let _ = Eliom_registration.Any.register balances_service balances_handler in
  ()

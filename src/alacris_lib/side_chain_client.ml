(* side_chain_client.ml -- send queries and transactions to side chain via TCP/IP *)

open Side_chain

open Legilogic_lib
open Yojsoning
open Marshaling
open Action
open Lwt_exn
open Signing

type facilitator_config =
  { nickname : string
  ; address : Address.t
  } [@@deriving of_yojson]

type side_chain_client_config =
  { host : string
  ; port : int
  ; facilitator : facilitator_config
  } [@@deriving of_yojson]

let config =
  lazy
    ("side_chain_client_config.json"
     |> Config.get_config_filename
     |> yojson_of_file
     |> side_chain_client_config_of_yojson
     |> function
     | Ok config -> config
     | Error msg -> Lib.bork "Error loading side chain client configuration: %s" msg)

let sockaddr =
  lazy (match config with lazy {host;port} -> Unix.(ADDR_INET (inet_addr_of_string host, port)))

let facilitator_address =
  lazy (match config with lazy {facilitator={address}} -> address)

let decode_response unmarshaler =
  unmarshaler |> Tag.unmarshal_result_or_exn |> unmarshal_string_of_unmarshal |> Lwter.arr

(* Queries return JSON *)
let post_query_to_server (request : Query.t) =
  match request with
  | `AdminQuery _
  | `UserQuery _ ->
    with_connection (Lazy.force sockaddr)
      (fun (in_channel,out_channel) ->
         Query.marshal_string request
         |> write_string_to_lwt_io_channel out_channel
         >>= fun () ->
         read_string_from_lwt_io_channel in_channel
         >>= decode_response yojson_marshaling.unmarshal)

let post_query_hook = ref post_query_to_server

let post_user_query_request (request : UserQueryRequest.t) =
  `UserQuery request |> !post_query_hook

let post_admin_query_request (request : AdminQueryRequest.t) =
  `AdminQuery request |> !post_query_hook

(* Transaction's return TransactionCommitment's *)
let post_user_transaction_request_to_server (request : UserTransactionRequest.t signed) =
  let (external_request : ExternalRequest.t) = `UserTransaction request in
  with_connection (Lazy.force sockaddr)
    (fun (in_channel, out_channel) ->
       ExternalRequest.marshal_string external_request
       |> write_string_to_lwt_io_channel out_channel
       >>= fun () -> read_string_from_lwt_io_channel in_channel
       >>= decode_response TransactionCommitment.unmarshal)

let post_user_transaction_request_hook = ref post_user_transaction_request_to_server

let post_user_transaction_request request =
  request |> !post_user_transaction_request_hook

module Test = struct
  let post_query_hook = post_query_hook
  let post_user_transaction_request_hook = post_user_transaction_request_hook
end

(* side_chain_client.ml -- send queries and transactions to side chain via TCP/IP *)

open Side_chain

open Legilogic_lib
open Yojsoning
open Marshaling
open Action
open Lwt_exn
open Signing

type side_chain_client_config =
  { host : string
  ; port : int }
[@@deriving of_yojson]

let config =
  "side_chain_client_config.json"
  |> Config.get_config_filename
  |> yojson_of_file
  |> side_chain_client_config_of_yojson
  |> function
  | Ok config -> config
  | Error msg -> Lib.bork "Error loading side chain client configuration: %s" msg

let sockaddr = Unix.(ADDR_INET (inet_addr_of_string config.host,config.port))

let decode_response unmarshaler =
  unmarshaler |> Tag.unmarshal_result_or_exn |> unmarshal_string_of_unmarshal |> Lwt_monad.arr

(* queries return JSON *)
let post_query_request_to_side_chain_ (request : ExternalRequest.t) =
  match request with
  | `AdminQuery _
  | `UserQuery _ ->
    with_connection sockaddr
      (fun (in_channel,out_channel) ->
         ExternalRequest.marshal_string request
         |> write_string_to_lwt_io_channel out_channel
         >>= fun () ->
         read_string_from_lwt_io_channel in_channel
         >>= decode_response yojson_marshaling.unmarshal)
  | _ -> bork "post_query_to_side_chain, not a query"

let post_user_query_request_to_side_chain (request : UserQueryRequest.t) =
  `UserQuery request |> post_query_request_to_side_chain_

let post_admin_query_request_to_side_chain (request : AdminQueryRequest.t) =
  `AdminQuery request |> post_query_request_to_side_chain_

(* transactions return Transactions *)
let post_user_transaction_request_to_side_chain (request : UserTransactionRequest.t signed) =
  let (external_request : ExternalRequest.t) = `UserTransaction request in
  with_connection sockaddr
    (fun (in_channel, out_channel) ->
       ExternalRequest.marshal_string external_request
       |> write_string_to_lwt_io_channel out_channel
       >>= fun () -> read_string_from_lwt_io_channel in_channel
       >>= decode_response TransactionCommitment.unmarshal)

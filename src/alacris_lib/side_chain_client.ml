(* side_chain_client.ml -- send queries and transactions to side chain via TCP/IP *)

open Side_chain

open Legilogic_lib
open Yojsoning
open Marshaling
open Action
open Lwt_exn
open Signing

type operator_config =
  { nickname : string
  ; address : Address.t
  } [@@deriving of_yojson]

type side_chain_client_config =
  { host : string
  ; port : int
  ; operator : operator_config
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


let sockaddr = lazy (match config with lazy {host;port} ->
    Unix.ADDR_INET (Get_ip_address.inet_addr_from_ip_or_host host, port))


let operator_address =
  lazy (match config with lazy {operator={address}} -> address)


let decode_response (unmarshaler : 'a unmarshaler) : (string, 'a or_exn) Lwter.arr =
  unmarshaler |> Tag.unmarshal_result_or_exn |> unmarshal_string_of_unmarshal |> Lwter.arr


(* Queries return JSON *)
let post_query_to_server (request : Query.t) : yojson OrExn.t Lwt.t =
  (*  let etime = Unix.gettimeofday() in
  Logging.log "side_chain_client : post_query_to_server etime=%f" etime; *)
  match request with
  | `AdminQuery _
  | `UserQuery _ ->
     with_connection (Lazy.force sockaddr)
      (fun (in_channel,out_channel) ->
         Query.marshal_string request
         |> fun x ->
            (* Logging.log "Before write_string_to_lwt_io_channel, post_query_to_server, x=%s" x; *)
            write_string_to_lwt_io_channel out_channel x
         >>= fun () ->
         read_string_from_lwt_io_channel in_channel
         >>= fun x ->
         decode_response yojson_marshaling.unmarshal x)

let post_query_hook = ref post_query_to_server

let post_user_query_request (request : UserQueryRequest.t) : yojson OrExn.t Lwt.t =
  `UserQuery request |> !post_query_hook

(*
let post_admin_query_request (request : AdminQueryRequest.t) =
  `AdminQuery request |> !post_query_hook
 *)


let post_user_transaction_request_to_server : UserTransactionRequest.t signed -> TransactionCommitment.t Lwt_exn.t =
  fun signed_request ->
  let (external_request : ExternalRequest.t) = `UserTransaction signed_request in
  with_connection (Lazy.force sockaddr)
    (fun (in_channel, out_channel) ->
      let (eval : string) = ExternalRequest.marshal_string external_request in
      eval
      |> fun x ->
         (* Logging.log "Before write_string_to_lwt_io_channel, post_user_transaction_request_to_server x=%s" x; *)
         write_string_to_lwt_io_channel out_channel x
      >>= fun () ->
      read_string_from_lwt_io_channel in_channel
      >>= fun x ->
      decode_response TransactionCommitment.unmarshal x)

let post_user_transaction_request_hook = ref post_user_transaction_request_to_server


let post_user_transaction_request (signed_request : UserTransactionRequest.t signed) =
  signed_request |> !post_user_transaction_request_hook

module Test = struct
  let post_query_hook = post_query_hook
  let post_user_transaction_request_hook = post_user_transaction_request_hook
end

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
  Logging.log "side_chain_client : decode_response";
  unmarshaler |> Tag.unmarshal_result_or_exn |> unmarshal_string_of_unmarshal |> Lwter.arr
  
  
(* Queries return JSON *)
let post_query_to_server (request : Query.t) : yojson OrExn.t Lwt.t =
  Logging.log "side_chain_client : post_query_to_server";
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

let post_user_query_request (request : UserQueryRequest.t) : yojson OrExn.t Lwt.t =
  `UserQuery request |> !post_query_hook

(*
let post_admin_query_request (request : AdminQueryRequest.t) =
  `AdminQuery request |> !post_query_hook
 *)


let fct_decode_response (x : int) : bytes -> TransactionCommitment.t * int =
  Logging.log "Passing by fct_decode_response x=%i" x;
  TransactionCommitment.unmarshal x

  
(* Transaction's return TransactionCommitment's *)
let post_user_transaction_request_to_server (request : UserTransactionRequest.t signed) : TransactionCommitment.t OrExn.t Lwt.t =
  Logging.log "side_chain_client : post_user_transaction_request_to_server";
  let (external_request : ExternalRequest.t) = `UserTransaction request in
  with_connection (Lazy.force sockaddr)
    (fun (in_channel, out_channel) ->
      Logging.log "side_chain_client : fun (in_channel, out_channel)";
      let (eval : string) = ExternalRequest.marshal_string external_request in
      Logging.log "Returning marshaled value";
      eval
      |> write_string_to_lwt_io_channel out_channel
      >>= fun () ->
      Logging.log "Before read_string_from_lwt_io_channel in_channel";
      read_string_from_lwt_io_channel in_channel
      >>= decode_response fct_decode_response)

let post_user_transaction_request_hook = ref post_user_transaction_request_to_server

                                       
let post_user_transaction_request (request : UserTransactionRequest.t signed) =
  Logging.log "post_user_transaction_request";
  request |> !post_user_transaction_request_hook

module Test = struct
  let post_query_hook = post_query_hook
  let post_user_transaction_request_hook = post_user_transaction_request_hook
end

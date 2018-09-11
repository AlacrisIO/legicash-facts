(* side_chain_client.ml -- send queries and transactions to side chain via TCP/IP *)

open Lwt_io

open Side_chain

open Legilogic_lib
open Action
open Lwt_exn
open Signing

let port = 8095 (* TODO: configuration item *)

let sockaddr = Unix.(ADDR_INET (inet_addr_of_string "127.0.0.1",port))

(* queries return JSON *)
let post_query_request_to_side_chain_ (request : ExternalRequest.t) =
  match request with
  | `AdminQuery _
  | `UserQuery _ ->
    of_lwt open_connection sockaddr
    >>= fun (in_channel,out_channel) ->
    ExternalRequest.marshal_string request
    |> write_string_to_lwt_io_channel out_channel
    >>= fun () -> read_string_from_lwt_io_channel in_channel
    >>= fun s ->
    Printf.eprintf "RESPONSE STRING: \"%s\"\n%!" s;
    Yojson.Safe.from_string s |> return
  | _ -> Lib.bork "post_query_to_side_chain, not a query"

let post_user_query_request_to_side_chain (request : UserQueryRequest.t) =
  `UserQuery request |> post_query_request_to_side_chain_

let post_admin_query_request_to_side_chain (request : AdminQueryRequest.t) =
  `AdminQuery request |> post_query_request_to_side_chain_

(* transactions return Transactions *)
let post_user_transaction_request_to_side_chain (request : UserTransactionRequest.t signed) =
  let (external_request : ExternalRequest.t) = `UserTransaction request in
  of_lwt open_connection sockaddr
  >>= fun (in_channel,out_channel) ->
  ExternalRequest.marshal_string external_request
  |> Lwt_stream.of_string
  |> of_lwt (write_chars out_channel)
  >>= fun () -> read_chars in_channel |> of_lwt Lwt_stream.to_string
  >>= fun s -> Transaction.unmarshal_string s |> return

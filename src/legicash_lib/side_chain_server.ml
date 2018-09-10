(* side_chain_server -- TCP/IP server to receive client requests *)

open Lwt_io

open Legilogic_lib
open Action
open Lwt_exn

open Legicash_lib

open Side_chain_facilitator
open Side_chain

let sock = Lwt_unix.(socket PF_INET SOCK_STREAM 0)

let port = 8095 (* TODO: configuration item *)

let sockaddr = Unix.(ADDR_INET (inet_addr_any,port))

let process_request_exn _client_address (in_channel,out_channel) =
  read_chars in_channel |> of_lwt Lwt_stream.to_string
  >>= fun marshaled ->
  match ExternalRequest.unmarshal_string marshaled with
  | `UserQuery request ->
    trying post_user_query_request request
    >>= handling fail (* TODO: something better *)
    >>= fun json ->
    Yojson.Safe.Util.to_string json
    |> Lwt_stream.of_string
    |> of_lwt (write_chars out_channel)
  | `UserTransaction request ->
    trying post_user_transaction_request (request,true)
    >>= handling fail
    >>= fun transaction ->
    Transaction.marshal_string transaction
    |> Lwt_stream.of_string
    |> of_lwt (write_chars out_channel)
  | `AdminQuery request ->
    trying post_admin_query_request request
    >>= handling fail
    >>= fun json ->
    Yojson.Safe.Util.to_string json
    |> Lwt_stream.of_string
    |> of_lwt (write_chars out_channel)

(* squeeze Lwt_exn into Lwt *)
let process_request client_address channels =
  run_lwt (process_request_exn client_address) channels

let _ =
  let _server =
    establish_server_with_client_address
      ~fd:sock
      sockaddr
      process_request
  in
  Printf.printf "Side chain server started\n%!";
  Lwt_main.run (fst (Lwt.wait ()))

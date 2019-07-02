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
  ; contract_address : string
  ; code_hash : string
  ; creation_hash : string
  ; creation_block : int
  ; operator : operator_config
  } [@@deriving of_yojson]


let side_chain_client_ref = ref None

let rec get_config_client_iter : string -> side_chain_client_config Lwt.t =
  fun full_filename ->
  let open Lwt in
  let test = Sys.file_exists full_filename in
  if test then
    (let config_client = full_filename |> yojson_of_file |> side_chain_client_config_of_yojson in
     match config_client with
     | Ok x -> Lwt.return x
     | Error msg -> Lib.bork "Error loading side chain client configuration: %s" msg)
  else
    (Lwt_unix.sleep 1.0
     >>= fun () -> get_config_client_iter full_filename)



let get_config_client : unit -> side_chain_client_config Lwt.t =
  fun () ->
  let open Lwt in
  match !side_chain_client_ref with
  | Some x -> Lwt.return x
  | None -> (let full_filename = Config.get_config_filename "side_chain_client_config.json" in
             get_config_client_iter full_filename
             >>= fun side_chain_client_config ->
             side_chain_client_ref := Some side_chain_client_config;
             Lwt.return side_chain_client_config)



let get_sockaddr () =
  let open Lwt in
  get_config_client ()
  >>= fun config ->
  return (Unix.ADDR_INET (Get_ip_address.inet_addr_from_ip_or_host config.host, config.port))


let quadruple_contract_info_for_client () =
  let open Lwt in
  get_config_client ()
  >>= fun config ->
  let open Types in
  let contract_address_i : Address.t = Address.of_0x config.contract_address in
  let code_hash_i : Digest.t = Digest.of_0x config.code_hash in
  let creation_hash_i : Digest.t = Digest.of_0x config.creation_hash in
  let creation_block_i = Revision.of_int config.creation_block in
  let e_quad : Operator_contract.quadruple_contract = {contract_address=contract_address_i; code_hash=code_hash_i; creation_hash=creation_hash_i; creation_block=creation_block_i} in
  return e_quad


let get_operator_address_client () =
  let open Lwt in
  get_config_client ()
  >>= fun config ->
  Lwt.return config.operator.address



let decode_response (unmarshaler : 'a unmarshaler) : (string, 'a or_exn) Lwter.arr =
  unmarshaler |> Tag.unmarshal_result_or_exn |> unmarshal_string_of_unmarshal |> Lwter.arr


(* Queries return JSON *)
let post_query_to_server (request : Query.t) : yojson OrExn.t Lwt.t =
  (*  let etime = Unix.gettimeofday() in
  Logging.log "side_chain_client : post_query_to_server etime=%f" etime; *)
  match request with
  | `AdminQuery _
  | `UserQuery _ ->
     (Lwt_exn.of_lwt get_sockaddr) ()
     >>= fun sockaddr ->
     with_connection sockaddr
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


let post_user_transaction_request_to_server (request : UserTransactionRequest.t signed) : TransactionCommitment.t OrExn.t Lwt.t =
  let (external_request : ExternalRequest.t) = `UserTransaction request in
  (Lwt_exn.of_lwt get_sockaddr) ()
  >>= fun sockaddr ->
  with_connection sockaddr
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


let post_user_transaction_request (request : UserTransactionRequest.t signed) =
  request |> !post_user_transaction_request_hook

module Test = struct
  let post_query_hook = post_query_hook
  let post_user_transaction_request_hook = post_user_transaction_request_hook
end

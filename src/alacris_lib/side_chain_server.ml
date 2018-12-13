(* side_chain_server -- TCP/IP server to receive client requests *)

open Lwt_io

open Legilogic_lib
open Action
open Lwt_exn
open Marshaling
open Types
open Signing

open Alacris_lib
open Side_chain
open Side_chain_operator

let _ =
  Config.set_application_name "alacris"

type side_chain_server_config =
  { port : int; }
[@@deriving of_yojson]

let _init_random =
  Random.self_init

(** TODO: encrypt the damn file! *)
type operator_keys_config =
  { nickname : string
  ; keypair : Keypair.t }
[@@deriving of_yojson]

let operator_address =
  "operator_keys.json"
  |> Config.get_config_filename
  |> Yojsoning.yojson_of_file
  |> operator_keys_config_of_yojson
  |> OrString.get
  |> fun { nickname; keypair } ->
  let address = keypair.address in
  Logging.log "Using operator keypair %S %s" nickname (Address.to_0x address);
  register_keypair nickname keypair;
  address

let config =
  "side_chain_server_config.json"
  |> Config.get_config_filename
  |> Yojsoning.yojson_of_file
  |> side_chain_server_config_of_yojson
  |> function
  | Ok config -> config
  | Error msg -> Lib.bork "Error loading side chain server configuration: %s" msg

let sockaddr = Unix.(ADDR_INET (inet_addr_any, config.port))

(* TODO: pass request id, so we can send a JSON RPC style reply? *)
(* TODO: have some try ... finally construct handle the closing of the channels *)
let process_request_exn _client_address (in_channel,out_channel) =
  let encode_response marshaler =
    marshaler |> Tag.marshal_result_or_exn |> marshal_string_of_marshal |> arr in
  read_string_from_lwt_io_channel in_channel
  >>= trying (catching_arr ExternalRequest.unmarshal_string)
  >>= (function
    | Ok (`UserQuery request) ->
      (post_user_query_request request |> Lwt.bind) (encode_response yojson_marshaling.marshal)
    | Ok (`UserTransaction request) ->
      (post_user_transaction_request request |> Lwt.bind) (encode_response TransactionCommitment.marshal)
    | Ok (`AdminQuery request) ->
      (post_admin_query_request request |> Lwt.bind) (encode_response yojson_marshaling.marshal)
    | Error e ->
      Error e |> encode_response Unit.marshal)
  (* TODO: We need to always close, and thus exit the Lwt_exn monad and properly handle the Result
     (e.g. by turning it into a yojson that fulfills the JSON RPC interface) before we close.
  *)
  >>= catching (write_string_to_lwt_io_channel out_channel)
  >>= fun () -> catching_lwt close in_channel
  >>= fun () -> catching_lwt close out_channel

(* squeeze Lwt_exn into Lwt *)
let process_request client_address channels =
  run_lwt (trying (catching (process_request_exn client_address))
           >>> handling (fun e ->
             Logging.log "Exception while processing server request: %s" (Printexc.to_string e);
             return ()))
    channels

let load_operator_state address =
  Logging.log "Loading the side_chain state...";
  Db.check_connection ();
  trying (catching_arr OperatorState.load) address
  >>= handling
        (function
          | Operator_not_found _ ->
            Logging.log "Side chain not found, generating a new demo side chain";
            let initial_state = initial_operator_state address in
            let open Lwt in
            OperatorState.save initial_state
            >>= Db.commit
            >>= fun () ->
            Lwt_exn.return initial_state
          | e -> fail e)
  >>= fun operator_state ->
  Logging.log "Done loading side chain state";
  return operator_state

let _ =
  Logging.log "*** STARTING SIDE CHAIN SERVER, PLEASE WAIT ***";
  Lwt_exn.run
    (fun () ->
       of_lwt Db.open_connection "alacris_server_db"
       >>= fun () ->
       Side_chain_action.ensure_side_chain_contract_created operator_address
       >>= fun contract_address ->
       assert (contract_address = Operator_contract.get_contract_address ());
       Logging.log "Using contract %s"
         (Address.to_0x contract_address);
       load_operator_state operator_address
       >>= fun _operator_state ->
       let%lwt _server = establish_server_with_client_address sockaddr process_request in
       start_operator operator_address
       >>= fun () ->
       Logging.log "*** SIDE CHAIN SERVER STARTED ***";
       (* NEVER RETURN *)
       of_lwt (fun () -> Lwt.wait () |> fst) ())
    ()

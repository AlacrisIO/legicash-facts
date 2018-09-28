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
open Side_chain_facilitator

let _ =
  Config.set_application_name "alacris"

type side_chain_server_config =
  { port : int; }
[@@deriving of_yojson]

let _init_random =
  Random.self_init

(** TODO: encrypt the damn file! *)
type facilitator_keys_config =
  { nickname : string
  ; keypair : Keypair.t
  ; password : string }
[@@deriving of_yojson]

let facilitator_address =
  "facilitator_keys.json"
  |> Config.get_config_filename
  |> Yojsoning.yojson_of_file
  |> facilitator_keys_config_of_yojson
  |> OrString.get
  |> fun { nickname; keypair; password } ->
  let address = keypair.address in
  Logging.log "Using facilitator keypair %S %s" nickname (Address.to_0x_string address);
  register_keypair nickname keypair;
  register_password address password;
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

let new_facilitator_state address =
  let keypair = keypair_of_address address in
  let fee_schedule = initial_fee_schedule in (* TODO: support different fee schedules *)
  let current =
    State.
      { facilitator_revision= Revision.of_int 0
      ; spending_limit= TokenAmount.of_int 100000000 (* TODO: start 0 and do facilitator deposit *)
      ; accounts= AccountMap.empty
      ; transactions= TransactionMap.empty
      ; main_chain_transactions_posted= Merkle_trie.DigestSet.empty } in
  let committed = SignedState.make keypair current in
  FacilitatorState.{ keypair; committed; current; fee_schedule }

let load_facilitator_state address =
  Logging.log "Loading the side_chain state...";
  Db.check_connection ();
  trying (catching_arr FacilitatorState.load) address
  >>= handling
        (function
          | Facilitator_not_found _ ->
            Logging.log "Side chain not found, generating a new demo side chain";
            let initial_state = new_facilitator_state address in
            let open Lwt in
            FacilitatorState.save initial_state
            >>= Db.commit
            >>= fun () ->
            Lwt_exn.return initial_state
          | e -> fail e)
  >>= fun facilitator_state ->
  Logging.log "Done loading side chain state";
  return facilitator_state

let _ =
  Logging.log "*** STARTING SIDE CHAIN SERVER, PLEASE WAIT ***";
  Lwt_exn.run
    (fun () ->
       of_lwt Db.open_connection "alacris-server"
       >>= fun () ->
       Side_chain_action.ensure_side_chain_contract_created facilitator_address
       >>= fun () ->
       Logging.log "Using contract %s"
         (Address.to_0x_string @@ Facilitator_contract.get_contract_address ());
       load_facilitator_state facilitator_address
       >>= fun _facilitator_state ->
       let%lwt _server = establish_server_with_client_address sockaddr process_request in
       start_facilitator facilitator_address
       >>= fun () ->
       Logging.log "*** SIDE CHAIN SERVER STARTED ***";
       (* NEVER RETURN *)
       of_lwt (fun () -> Lwt.wait () |> fst) ())
    ()

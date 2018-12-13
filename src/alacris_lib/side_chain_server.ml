(* side_chain_server -- TCP/IP server to receive client requests *)

open Legilogic_lib
open Action
open Lwt_exn
open Marshaling
open Types
open Signing

open Alacris_lib
open Legilogic_ethereum
open Side_chain
open Side_chain_facilitator
open Side_chain_server_config
   
let _ =
  Config.set_application_name "alacris"

let _init_random =
  Random.self_init

(** TODO: encrypt the damn file! *)
type facilitator_keys_config =
  { nickname : string
  ; keypair : Keypair.t }
[@@deriving of_yojson]

let facilitator_address =
  "facilitator_keys.json"
  |> Config.get_config_filename
  |> Yojsoning.yojson_of_file
  |> facilitator_keys_config_of_yojson
  |> OrString.get
  |> fun { nickname; keypair } ->
  let address = keypair.address in
  Logging.log "Using facilitator keypair %S %s" nickname (Address.to_0x address);
  register_keypair nickname keypair;
  address


(* let minNbBlockConfirm = lazy (match config with lazy {minimal_number_block_for_confirmation={nb}} -> nb) *)
(* let minNbBlockConfirm = config.minimal_number_block_for_confirmation*)
                      
(* TODO: pass request id, so we can send a JSON RPC style reply? *)
(* TODO: have some try ... finally construct handle the closing of the channels *)
let process_request_exn _client_address (in_channel,out_channel) =
  Logging.log "process_request_exn, running 1";
  let encode_response marshaler =
    marshaler |> Tag.marshal_result_or_exn |> marshal_string_of_marshal |> arr in
  read_string_from_lwt_io_channel in_channel
  >>= trying (catching_arr ExternalRequest.unmarshal_string)
  >>= (function
    | Ok (`UserQuery request) ->
      (facil_post_user_query_request request |> Lwt.bind) (encode_response yojson_marshaling.marshal)
    | Ok (`UserTransaction request) ->
      (facil_post_user_transaction_request request |> Lwt.bind) (encode_response TransactionCommitment.marshal)
    | Ok (`AdminQuery request) ->
      (facil_post_admin_query_request request |> Lwt.bind) (encode_response yojson_marshaling.marshal)
    | Error e ->
      Error e |> encode_response Unit.marshal)
  (* TODO: We need to always close, and thus exit the Lwt_exn monad and properly handle the Result
     (e.g. by turning it into a yojson that fulfills the JSON RPC interface) before we close.
  *)
  >>= catching (write_string_to_lwt_io_channel out_channel)
  >>= fun () -> catching_lwt Lwt_io.close in_channel
  >>= fun () -> catching_lwt Lwt_io.close out_channel

(* squeeze Lwt_exn into Lwt *)
let process_request client_address channels =
  Logging.log "process_request, running 1";
  run_lwt (trying (catching (process_request_exn client_address))
           >>> handling (fun e ->
             Logging.log "Exception while processing server request: %s" (Printexc.to_string e);
             return ()))
    channels

let load_facilitator_state address =
  Logging.log "Loading the side_chain state...";
  Db.check_connection ();
  trying (catching_arr FacilitatorState.load) address
  >>= handling
        (function
          | Facilitator_not_found _ ->
            Logging.log "Side chain not found, generating a new demo side chain";
            let initial_state = initial_facilitator_state address in
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
       of_lwt Db.open_connection "alacris_server_db"
       >>= fun () ->
       Side_chain_action.ensure_side_chain_contract_created facilitator_address
       >>= fun contract_address ->
       assert (contract_address = Facilitator_contract.get_contract_address ());
       Logging.log "Using contract %s"
         (Address.to_0x contract_address);
       load_facilitator_state facilitator_address
       >>= fun _facilitator_state ->
       let%lwt _server = Lwt_io.establish_server_with_client_address Side_chain_server_config.sockaddr process_request in
       start_facilitator facilitator_address
       >>= fun () ->
       Logging.log "*** SIDE CHAIN SERVER STARTED ***";
       (* NEVER RETURN *)
       of_lwt (fun () -> Lwt.wait () |> fst) ())
    ()

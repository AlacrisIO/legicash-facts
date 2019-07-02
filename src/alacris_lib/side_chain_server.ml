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
open Side_chain_operator
open Side_chain_server_config

let _ =
  Config.set_application_name "alacris"

let _init_random =
  Random.self_init

let side_chain_server_log = false


(* TODO: pass request id, so we can send a JSON RPC style reply? *)
(* TODO: have some try ... finally construct handle the closing of the channels *)
let process_request_exn _client_address (in_channel,out_channel) =
  if side_chain_server_log then
    Logging.log "process_request_exn, running 1";
  let (iter : int ref) = ref 0 in
  let encode_response marshaler =
    iter := !iter + 1;
    marshaler |> Tag.marshal_result_or_exn |> marshal_string_of_marshal |> arr in
  read_string_from_lwt_io_channel in_channel
  >>= fun x ->
  (*  Logging.log "After read_string_from_lwt_io_channel x=%s" x;*)
  if side_chain_server_log then
    Logging.log "After read_string_from_lwt_io_channel x=(omit)";
  trying (catching_arr ExternalRequest.unmarshal_string) x
  >>= (function
    | Ok (`UserQuery request) ->
       if side_chain_server_log then
         Logging.log "process_request_exn : UserQuery";
       (oper_post_user_query_request request |> Lwt.bind) (encode_response yojson_marshaling.marshal)
    | Ok (`UserTransaction request) ->
       if side_chain_server_log then
         Logging.log "process_request_exn : UserTransaction";
      (oper_post_user_transaction_request request |> Lwt.bind) (encode_response TransactionCommitment.marshal)
    | Ok (`AdminQuery request) ->
       if side_chain_server_log then
         Logging.log "process_request_exn : AdminQuery";
      (oper_post_admin_query_request request |> Lwt.bind) (encode_response yojson_marshaling.marshal)
    | Error e ->
       if side_chain_server_log then
         Logging.log "process_request_exn : Error case";
       Error e |> encode_response Unit.marshal)
  (* TODO: We need to always close, and thus exit the Lwt_exn monad and properly handle the Result
     (e.g. by turning it into a yojson that fulfills the JSON RPC interface) before we close.
  *)
  >>= fun x ->
  if side_chain_server_log then
    Logging.log "Before writing to write_string_to_lwt_io_channel x=(omit)";
  catching (write_string_to_lwt_io_channel out_channel) x
  >>= fun () -> catching_lwt Lwt_io.close in_channel
  >>= fun () -> catching_lwt Lwt_io.close out_channel

(* squeeze Lwt_exn into Lwt *)
let process_request client_address channels =
  if side_chain_server_log then
    Logging.log "process_request, running 1";
  run_lwt (trying (catching (process_request_exn client_address))
           >>> handling (fun e ->
                   if side_chain_server_log then
                     Logging.log "Exception while processing server request: %s" (Printexc.to_string e);
                   return ()))
    channels

let load_operator_state address =
  if side_chain_server_log then
    Logging.log "Loading the side_chain state...";
  Db.check_connection ();
  trying (catching_arr OperatorState.load) address
  >>= handling
        (function
         | Operator_not_found _ ->
            if side_chain_server_log then
              Logging.log "Side chain not found, generating a new demo side chain";
            let initial_state = initial_operator_state address in
            let open Lwt in
            OperatorState.save initial_state
            >>= Db.commit
            >>= fun () ->
            Lwt_exn.return initial_state
         | e -> fail e)
  >>= fun operator_state ->
  if side_chain_server_log then
    Logging.log "Done loading side chain state";
  return operator_state

let sockaddr = Unix.(ADDR_INET (inet_addr_any, Side_chain_server_config.config.port))


let _ =
  Lwt_exn.run
    (fun () ->
      if side_chain_server_log then
        Logging.log "Beginning of side_chain_server";
      Mkb_json_rpc.init_mkb_server ()
      (*      >>= fun () -> State_update.start_state_update_operator () *)
      >>= fun () -> Side_chain_operator.start_state_update_periodic_operator ()
      >>= fun () ->
      if side_chain_server_log then
        Logging.log "Before the Db.open_connection";
      of_lwt Db.open_connection "alacris_server_db"
      >>= fun () ->
      if side_chain_server_log then
        Logging.log "Side_chain_server_config.operator_address=%s" (Address.to_0x Side_chain_server_config.operator_address);
      Side_chain_action.get_contract_address_for_server ()
      >>= fun contract_address ->
      assert (contract_address = Operator_contract.get_contract_address ());
      if side_chain_server_log then
        Logging.log "Using contract %s" (Address.to_0x contract_address);
      load_operator_state Side_chain_server_config.operator_address
      >>= fun _operator_state ->
      let%lwt _server = Lwt_io.establish_server_with_client_address sockaddr process_request in
      start_operator Side_chain_server_config.operator_address
      >>= fun () ->
      if side_chain_server_log then
        Logging.log "*** SIDE CHAIN SERVER STARTED ***";
      (* NEVER RETURN *)
      of_lwt (fun () -> Lwt.wait () |> fst) ())
    ()

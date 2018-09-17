(* side_chain_server -- TCP/IP server to receive client requests *)

open Lwt_io

open Legilogic_lib
open Action
open Lwt_exn
open Marshaling
open Types

open Legicash_lib
open Side_chain_facilitator
open Side_chain

type side_chain_server_config =
  { port : int }
[@@deriving of_yojson]

let config =
  let config_file = Config.get_config_filename "side_chain_server_config.json" in
  match Yojsoning.yojson_of_file config_file
        |> side_chain_server_config_of_yojson with
  | Ok config -> config
  | Error msg -> Lib.bork "Error loading side chain server configuration: %s" msg

let sockaddr = Unix.(ADDR_INET (inet_addr_any,config.port))

(* TODO: pass request id, so we can send a JSON RPC style reply? *)
let process_request_exn _client_address (in_channel,out_channel) =
  let encode_response marshaler =
    marshaler |> Tag.marshal_result_or_exn |> marshal_string_of_marshal |> arr in
  read_string_from_lwt_io_channel in_channel
  >>= trying (catching ExternalRequest.unmarshal_string)
  >>= (function
    | Ok (`UserQuery request) ->
      (post_user_query_request request |> Lwt.bind) (encode_response yojson_marshaling.marshal)
    | Ok (`UserTransaction request) ->
      (post_user_transaction_request request |> Lwt.bind) (encode_response Transaction.marshal)
    | Ok (`AdminQuery request) ->
      (post_admin_query_request request |> Lwt.bind) (encode_response yojson_marshaling.marshal)
    | Error e ->
      Error e |> encode_response Unit.marshal)
  (* TODO: We need to always close, and thus exit the Lwt_exn monad and properly handle the Result
     (e.g. by turning it into a yojson that fulfills the JSON RPC interface) before we close.
  *)
  >>= write_string_to_lwt_io_channel out_channel
  >>= fun () -> catching_lwt close in_channel
  >>= fun () -> catching_lwt close out_channel

(* squeeze Lwt_exn into Lwt *)
let process_request client_address channels =
  run_lwt (process_request_exn client_address) channels

(* TODO: add a wait for facilitator to initialize state with a deposit *)

let new_facilitator_state facilitator_keypair =
  let fee_schedule = Side_chain.Test.trent_fee_schedule in
  let (confirmed_state : Side_chain.State.t) =
    { facilitator_revision= Revision.of_int 0
    ; spending_limit= TokenAmount.of_int 100000000
    ; accounts= AccountMap.empty
    ; transactions= TransactionMap.empty
    ; main_chain_transactions_posted= Merkle_trie.DigestSet.empty } in
  FacilitatorState.
    { keypair= facilitator_keypair
    ; current= confirmed_state
    ; fee_schedule= fee_schedule }

let load_facilitator_state (keys : Signing.Keypair.t) =
  Logging.log "Loading the side_chain state...";
  Db.check_connection ();
  let facilitator_state =
    try
      Side_chain.FacilitatorState.load keys.address
      |> fun facilitator_state ->
      Logging.log "Done loading side chain state";
      facilitator_state
    with (* TODO: add mechanism for creating valid side chain on load failure *)
      Facilitator_not_found _ ->
      Logging.log "Side chain not found, generating a new demo side chain";
      new_facilitator_state keys
  in
  of_lwt FacilitatorState.save facilitator_state
  >>= of_lwt Db.commit

open Signing.Test

let _ =
  Logging.log "*** STARTING SIDE CHAIN SERVER, PLEASE WAIT ***";
  let _server =
    establish_server_with_client_address
      sockaddr
      process_request
  in
  let start_facilitator () =
    of_lwt (fun () -> Db.open_connection ~db_name:Legibase.db_name) ()
    >>= fun () -> Logging.log "Installing facilitator contract..."; return ()
    >>= trying (Side_chain_action.Test.load_contract >>> fun () -> Logging.log "done"; return ())
    >>= handling
          (fun _ -> printf "failed, installing contract...%!"
            >>= Side_chain_action.Test.install_contract
            >>= fun () -> Logging.log "done"; return ())
    >>= fun () -> load_facilitator_state trent_keys
    >>= fun () -> start_facilitator trent_address
    >>= fun () -> Logging.log "*** SIDE CHAIN SERVER STARTED ***"; return ()
    >>= of_lwt (fun () -> Lwt.wait () |> fst)
  in
  Lwt_exn.run start_facilitator ()

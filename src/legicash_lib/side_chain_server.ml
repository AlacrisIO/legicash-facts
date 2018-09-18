(* side_chain_server -- TCP/IP server to receive client requests *)

open Lwt_io

open Legilogic_lib
open Action
open Lwt_exn
open Marshaling
open Types
open Signing

open Legicash_lib
open Side_chain_facilitator
open Side_chain

type side_chain_server_config =
  { port : int }
[@@deriving of_yojson]

let _init_random =
  Random.self_init

let facilitator_address =
  "facilitator_keys.json"
  |> Config.get_config_filename
  |> Yojsoning.yojson_of_file
  |> decode_keypairs
  |> function
  | [(name, keypair)] ->
    Logging.log "Using facilitator keypair %S %s" name (Address.to_0x_string keypair.address);
    register_keypair name keypair;
    keypair.address
  | _ -> Lib.bork "Wrong number of keys in the facilitator_keys.json file"

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
  >>= write_string_to_lwt_io_channel out_channel
  >>= fun () -> catching_lwt close in_channel
  >>= fun () -> catching_lwt close out_channel

(* squeeze Lwt_exn into Lwt *)
let process_request client_address channels =
  run_lwt (process_request_exn client_address) channels

let new_facilitator_state address =
  let keypair = keypair_of_address address in
  let fee_schedule = Side_chain.Test.trent_fee_schedule in (* TODO: have some different fee schedule *)
  let current =
    Side_chain.State.
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
  trying (catching_arr Side_chain.FacilitatorState.load) address
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
       of_lwt (fun () -> Db.open_connection ~db_name:Legibase.db_name) ()
       >>= fun () ->
       Logging.log "Checking for facilitator contract...";
       trying (Side_chain_action.load_contract >>> fun () -> Logging.log "found"; return ()) ()
       >>= handling
             (fun _ ->
                Logging.log "Not found, creating the contract...";
                Side_chain_action.install_contract facilitator_address
                >>= fun () -> Logging.log "done"; return ())
       >>= fun () ->
       load_facilitator_state facilitator_address
       >>= fun _facilitator_state ->
       let%lwt _server = establish_server_with_client_address sockaddr process_request in
       start_facilitator facilitator_address
       >>= fun () ->
       Logging.log "*** SIDE CHAIN SERVER STARTED ***";
       (* NEVER RETURN *)
       of_lwt (fun () -> Lwt.wait () |> fst) ())
    ()

(* side_chain_server -- TCP/IP server to receive client requests *)

open Lwt_io

open Legilogic_lib
open Action
open Lwt_exn
open Types
open Yojsoning

open Legicash_lib

open Side_chain_facilitator
open Side_chain

let port = 8095 (* TODO: configuration item *)

let sockaddr = Unix.(ADDR_INET (inet_addr_any,port))

let process_request_exn _client_address (in_channel,out_channel) =
  read_string_from_lwt_io_channel in_channel
  >>= fun marshaled ->
  match ExternalRequest.unmarshal_string marshaled with
  | `UserQuery request ->
    trying post_user_query_request request
    >>= handling fail
    >>= fun json ->
    YoJson.to_string json
    |> write_string_to_lwt_io_channel out_channel
  | `UserTransaction request ->
    trying post_user_transaction_request (request,false) (* TODO: should the wait flag be in the request? *)
    >>= handling fail
    >>= fun transaction ->
    Transaction.marshal_string transaction
    |> write_string_to_lwt_io_channel out_channel
  | `AdminQuery request ->
    trying post_admin_query_request request
    >>= handling fail
    >>= fun json ->
    YoJson.to_string json
    |> write_string_to_lwt_io_channel out_channel

(* squeeze Lwt_exn into Lwt *)
let process_request client_address channels =
  run_lwt (process_request_exn client_address) channels

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
  Printf.printf "Loading the facilitator state...%!";
  Db.check_connection ();
  let facilitator_state =
    try
      Side_chain.FacilitatorState.load keys.address
      |> fun x -> Printf.printf "done\n%!"; x
    with
      Facilitator_not_found _ ->
      Printf.printf "not found, generating a new demo facilitator\n%!";
      new_facilitator_state keys
  in
  of_lwt FacilitatorState.save facilitator_state
  >>= of_lwt Db.commit

open Signing.Test

let _ =
  Printf.printf "Starting side chain Facilitator\n%!";
  let _server =
    establish_server_with_client_address
      sockaddr
      process_request
  in
  Lwt_main.run (
    of_lwt (fun () -> Db.open_connection ~db_name:Legibase.db_name) ()
    >>= fun () -> load_facilitator_state trent_keys
    >>= (fun () -> start_facilitator trent_address)
    >>= fun () ->
    Printf.printf "Side chain server started\n%!";
    fst (Lwt.wait ()))

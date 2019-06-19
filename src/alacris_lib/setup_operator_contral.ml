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

let setup_operator_contract_log = false


let _ =
  Lwt_exn.run
    (fun () ->
      if setup_operator_contract_log then
        Logging.log "Before setting up the operator_contract";
      Side_chain_action.ensure_side_chain_contract_created Side_chain_server_config.operator_address
      >>= fun () ->
      if setup_operator_contract_log then
        Logging.log "After the setup of the operator contract";
      of_lwt (fun () -> Lwt.wait () |> fst) ())
    ()

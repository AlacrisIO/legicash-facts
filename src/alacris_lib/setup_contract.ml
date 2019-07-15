(* side_chain_server -- TCP/IP server to receive client requests *)

open Legilogic_lib
open Action
open Signing
open Lwt_exn

open Alacris_lib
open Legilogic_ethereum
open Side_chain_server_config

let setup_contract_log = true

let _ =
  Lwt_exn.run
    (fun () ->
      if setup_contract_log then
        Logging.log "Before the Db.open_connection";
      of_lwt Db.open_connection "alacris_server_db"
      >>= fun () ->
      if setup_contract_log then
        Logging.log "Setup_contract operator_address=%s" (Address.to_0x Side_chain_server_config.operator_address);
      Side_chain_action.ensure_side_chain_contract Side_chain_server_config.operator_address) ()

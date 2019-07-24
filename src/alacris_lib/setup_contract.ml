(* side_chain_server -- TCP/IP server to receive client requests *)

open Legilogic_lib
open Action
open Signing
open Lwt_exn

open Alacris_lib
open Side_chain_server_config
open Operator_contract

let setup_contract_log = true

let _ =
  (*Logging.set_log_channel stderr;*)
  run
    (fun () ->
      let config_file = Config.get_config_filename "contract_config.json" in
      if Sys.file_exists config_file then
        bork "Contract configuration file %S already exists. Not overwriting." config_file
      else return () >>= fun _ ->
      if setup_contract_log then
        Logging.log "Before the Db.open_connection";
      of_lwt Db.open_connection "alacris_server_db" >>= fun () ->
      if setup_contract_log then
        Logging.log "Setup_contract operator_address=%s" (Address.to_0x Side_chain_server_config.operator_address);
      create_side_chain_contract Side_chain_server_config.operator_address
      >>= register_side_chain_contract) ()

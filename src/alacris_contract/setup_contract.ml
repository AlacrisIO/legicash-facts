(* side_chain_server -- TCP/IP server to receive client requests *)

open Legilogic_lib
open Action
open Signing

open Alacris_lib
open Legilogic_ethereum
open Side_chain_server_config

let side_chain_server_log = false


let _ =
  Lwt_exn.run
    (fun () ->
      if side_chain_server_log then
        Logging.log "Side_chain_server_config.operator_address=%s" (Address.to_0x Side_chain_server_config.operator_address);
      Side_chain_action.ensure_side_chain_contract_created Side_chain_server_config.operator_address) ()

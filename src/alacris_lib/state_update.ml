open Legilogic_lib
open Signing
open Action
open Lwt_exn

open Types

open Legilogic_ethereum
open Side_chain
open Operator_contract
open Digesting
open Side_chain_server_config


type digest_entry =
  { revision : Revision.t
  ; oper_digest : Digest.t}


let init_state : unit -> digest_entry =
  fun () -> {revision = Revision.of_int 0; oper_digest = null_digest}


let the_digest_entry_ref : (digest_entry ref) = ref (init_state ())

(* Alert to take care of:
   ---lack of gas
   ---transaction not passed
 *)
let push_state_digest_exn (digest : Digest.t) : Digest.t Lwt_exn.t =
  Logging.log "push_state_digest_exn : beginning of function";
  let (operation : Ethereum_chain.Operation.t) = make_state_update_call digest in
  let (gas_limit_val : TokenAmount.t option) = None in (* Some kind of arbitrary choice *)
  let (value : TokenAmount.t) = TokenAmount.zero in
  let (oper_addr : Address.t) = Side_chain_server_config.operator_address in
  Logging.log "push_state_digest_exn : before make_pre_transaction";
  Ethereum_user.make_pre_transaction ~sender:oper_addr operation ?gas_limit:gas_limit_val value
  (*  >>= handling (fun e -> Logging.log "Error caught in push_state_digest_exn"; return ())*)
  >>= fun x ->
  Logging.log "push_state_digest_exn : before confirm_pre_transaction";
  Ethereum_user.confirm_pre_transaction oper_addr x
  >>= fun (_tx, confirmation) ->
  Logging.log "push_state_digest_exn : before eth_get_transaction_receipt";
  Ethereum_json_rpc.eth_get_transaction_receipt confirmation.transaction_hash
  >>= fun x ->
  Logging.log "push_state_digest_exn : after eth_get_transaction_receipt";
  match x with
  | None -> bork "No tx receipt for contract creation"
  | Some _receipt -> return digest

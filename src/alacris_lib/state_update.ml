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
let push_state_digest_exn (digest : Digest.t) (value : TokenAmount.t) : Digest.t Lwt_exn.t =
  Logging.log "push_state_digest_exn : beginning of function value=%s" (TokenAmount.to_string value);
  let (operation : Ethereum_chain.Operation.t) = make_state_update_call digest in
  let (gas_limit_val : TokenAmount.t option) = None in (* Some kind of arbitrary choice *)
  let (oper_addr : Address.t) = Side_chain_server_config.operator_address in
  Logging.log "push_state_digest_exn : before make_pre_transaction";
  Ethereum_user.make_pre_transaction ~sender:oper_addr operation ?gas_limit:gas_limit_val value
  >>= fun x ->
  Logging.log "push_state_digest_exn : before confirm_pre_transaction";
  Ethereum_user.confirm_pre_transaction oper_addr x
  >>= fun (_, _, {transaction_hash}) ->
  return transaction_hash

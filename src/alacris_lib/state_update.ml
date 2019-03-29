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


let print_contract_account_value_unit (estr : string) : unit Lwt_exn.t =
  let (oper_addr : Address.t) = Side_chain_server_config.operator_address in
  let (contr_addr : Address.t) = get_contract_address () in
  Logging.log "oper_addr=%s" (Address.to_string oper_addr);
  Logging.log "contr_addr=%s" (Address.to_string contr_addr);
  Lwt_exn.bind (Ethereum_json_rpc.eth_get_balance (contr_addr, Latest))
    (fun x-> Logging.log "PCAV stage=%s value=%s" estr (TokenAmount.to_string x);
             Lwt_exn.return ())

let print_contract_account_value_ret (estr : string) (x : 'a) : 'a Lwt_exn.t =
  let (oper_addr : Address.t) = Side_chain_server_config.operator_address in
  Lwt_exn.bind (Ethereum_json_rpc.eth_get_balance (oper_addr, Latest))
    (fun x-> Logging.log "PCAV stage=%s value=%s" estr (TokenAmount.to_string x);
             Lwt_exn.return x)
  


  
                                              
(* Alert to take care of:
   ---lack of gas
   ---transaction not passed
 *)
let post_state_update (digest : Digest.t) : Digest.t Lwt_exn.t =
  Logging.log "post_state_update : beginning of function";
  let (operation : Ethereum_chain.Operation.t) = make_state_update_call digest in
  let (gas_limit_val : TokenAmount.t option) = None in (* Some kind of arbitrary choice *)
  let (value : TokenAmount.t) = TokenAmount.zero in
  let (oper_addr : Address.t) = Side_chain_server_config.operator_address in
  Logging.log "post_state_update : before make_pre_transaction";
  print_contract_account_value_unit "from post_state_update"
  >>= fun () -> Ethereum_user.make_pre_transaction ~sender:oper_addr operation ?gas_limit:gas_limit_val value
  >>= fun x ->
  Logging.log "post_state_update : before confirm_pre_transaction";
  Ethereum_user.confirm_pre_transaction oper_addr x
  >>= fun (_tx, confirmation) ->
  Logging.log "post_state_update : before eth_get_transaction_receipt";
  Ethereum_json_rpc.eth_get_transaction_receipt confirmation.transaction_hash
  >>= fun x ->
  Logging.log "post_state_update : after eth_get_transaction_receipt";
  match x with
  | None -> bork "No tx receipt for contract creation"
  | Some _receipt -> return digest

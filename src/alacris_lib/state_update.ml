open Legilogic_lib
open Signing
open Action
open Lwt_exn

open Types

open Legilogic_ethereum
open Side_chain
open Operator_contract
open Digesting
open Ethereum_json_rpc
open Side_chain_server_config


type digest_entry =
  { revision : Revision.t
  ; oper_digest : Digest.t}


let init_state : unit -> digest_entry =
  fun () -> {revision = Revision.of_int 0; oper_digest = null_digest}


let the_digest_entry_ref : (digest_entry ref) = ref (init_state ())


let print_contract_account_value : string -> unit Lwt_exn.t =
  fun estr ->
  let (oper_addr : Address.t) = Side_chain_server_config.operator_address in
  let (contr_addr : Address.t) = get_contract_address () in
  Logging.log "oper_addr=%s" (Address.to_string oper_addr);
  Logging.log "contr_addr=%s" (Address.to_string contr_addr);
  Lwt_exn.bind (Ethereum_json_rpc.eth_get_balance (contr_addr, Latest))
    (fun x-> Logging.log "PCAV stage=%s value=%s" estr (TokenAmount.to_string x);
             Lwt_exn.return ())



let print_status_receipt : TransactionReceipt.t -> string =
  fun tr ->
  match tr.status with
  | None -> "-1"
  | Some x -> (TokenAmount.to_string x)


let post_operation_general_kernel : Ethereum_chain.Operation.t -> TokenAmount.t -> TransactionReceipt.t Lwt_exn.t =
  fun operation value ->
  Logging.log "post_operation_kernel : beginning of function";
  let (gas_limit_val : TokenAmount.t option) = None in (* Some kind of arbitrary choice *)
  let (oper_addr : Address.t) = Side_chain_server_config.operator_address in
  Logging.log "post_operation_general_kernel : before make_pre_transaction";
  print_contract_account_value "from post_operation_general_kernel"
  >>= fun () -> Ethereum_user.make_pre_transaction ~sender:oper_addr operation ?gas_limit:gas_limit_val value
  >>= fun x ->
  Logging.log "post_operation_general_kernel : before confirm_pre_transaction";
  Ethereum_user.confirm_pre_transaction oper_addr x
  >>= fun (_tx, confirmation) ->
  Logging.log "post_operation_general_kernel : before eth_get_transaction_receipt";
  Logging.log "post_operation_general_kernel : confirmation.transaction_hash=%s" (Digest.to_string confirmation.transaction_hash);
  Ethereum_json_rpc.eth_get_transaction_receipt confirmation.transaction_hash
  >>= fun x ->
  Logging.log "post_operation_general_kernel : after eth_get_transaction_receipt";
  match x with
  | None -> bork "post_operation_general_kernel : No tx receipt for contract creation"
  | Some receipt ->
     Logging.log "post_operation_general_kernel : Ok receipt, transaction_hash=%s" (Digest.to_string receipt.transaction_hash);
     Logging.log "transaction status=%s" (print_status_receipt receipt);
     return receipt


let post_operation_general : Ethereum_chain.Operation.t -> TokenAmount.t -> TransactionReceipt.t Lwt_exn.t =
  fun operation value ->
  let rec fct_submit : unit -> TransactionReceipt.t Lwt_exn.t =
    fun () ->
    Lwt.bind (post_operation_general_kernel operation value)
      (function
       | Error _error -> Logging.log "post_operation_general, Error case";
                         Lwt_exn.bind (Ethereum_watch.sleep_delay_exn 1.0) (fun () -> fct_submit ())
       | Ok ereceipt ->
          (let str = print_status_receipt ereceipt in
           let str_succ = "1" in
           if String.equal str str_succ then
             Lwt_exn.return ereceipt
           else
             Lwt_exn.bind (Ethereum_watch.sleep_delay_exn 1.0) (fun () -> fct_submit ())
          )
      ) in
  fct_submit ()

let post_state_update : Digest.t -> TransactionReceipt.t Lwt_exn.t =
  fun digest ->
  let (operation : Ethereum_chain.Operation.t) = make_state_update_call digest in
  let (value : TokenAmount.t) = TokenAmount.zero in
  post_operation_general operation value


(* Alert to take care of:
   ---lack of gas
   ---transaction not passed
 *)
(*
let post_state_update_kernel digest =
  Logging.log "post_state_update : beginning of function";
  let (operation : Ethereum_chain.Operation.t) = make_state_update_call digest in
  let (gas_limit_val : TokenAmount.t option) = None in (* Some kind of arbitrary choice *)
  let (value : TokenAmount.t) = TokenAmount.zero in
  let (oper_addr : Address.t) = Side_chain_server_config.operator_address in
  Logging.log "post_state_update : before make_pre_transaction";
  print_contract_account_value "from post_state_update"
  >>= fun () -> Ethereum_user.make_pre_transaction ~sender:oper_addr operation ?gas_limit:gas_limit_val value
  >>= fun x ->
  Logging.log "post_state_update : before confirm_pre_transaction";
  Ethereum_user.confirm_pre_transaction oper_addr x
  >>= fun (_tx, confirmation) ->
  Logging.log "post_state_update : before eth_get_transaction_receipt";
  Logging.log "post_state_update : confirmation.transaction_hash=%s" (Digest.to_string confirmation.transaction_hash);
  Ethereum_json_rpc.eth_get_transaction_receipt confirmation.transaction_hash
  >>= fun x ->
  Logging.log "post_state_update : after eth_get_transaction_receipt";
  match x with
  | None -> bork "post_state_update : No tx receipt for contract creation"
  | Some receipt ->
     Logging.log "post_state_update : Ok receipt, transaction_hash=%s" (Digest.to_string receipt.transaction_hash);
     Logging.log "transaction status=%s" (print_status_receipt receipt);
     return receipt



let post_state_update digest =
  let rec fct_submit : unit -> TransactionReceipt.t Lwt_exn.t =
    fun () ->
    Lwt_exn.bind (post_state_update_kernel digest)
    (fun ereceipt ->
      let str = print_status_receipt ereceipt in
      let str_succ = "1" in
      if String.equal str str_succ then
        Lwt_exn.return ereceipt
      else
        fct_submit ()
    ) in
  fct_submit ()
 *)

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
  ; digest : Digest.t}

type request_state_update =
  | Submit of (Digest.t * TransactionReceipt.t OrExn.t Lwt.u)
  | GetLastRevision of Revision.t Lwt.u
  | GetLastCommit of Digest.t Lwt.u

let request_state_update_mailbox : request_state_update Lwt_mvar.t = Lwt_mvar.create_empty ()

let post_to_mailbox_state_update : Digest.t -> TransactionReceipt.t OrExn.t Lwt.t =
  fun digest ->
  simple_client request_state_update_mailbox
    (fun ((_x_digest, x_resolver) : (Digest.t * TransactionReceipt.t OrExn.t Lwt.u)) -> Submit (digest,x_resolver)) digest


let retrieve_last_posted_state : unit -> Digest.t Lwt.t =
  fun () ->
  simple_client request_state_update_mailbox
    (fun ((_x_unit, x_resolv) : (unit * Digest.t Lwt.u)) -> GetLastCommit x_resolv) ()

let retrieve_last_revision : unit -> Revision.t Lwt.t =
  fun () ->
  simple_client request_state_update_mailbox
    (fun ((_x_unit, x_resolv) : (unit * Revision.t Lwt.u)) -> GetLastRevision x_resolv) ()






let init_state : unit -> digest_entry =
  fun () -> {revision = Revision.of_int 0; digest = null_digest}


let the_digest_entry_ref : (digest_entry ref) = ref (init_state ())











let print_contract_account_value : string -> unit Lwt_exn.t =
  fun estr ->
  let (oper_addr : Address.t) = Side_chain_server_config.operator_address in
  let (contr_addr : Address.t) = get_contract_address () in
  Logging.log "oper_addr=%s" (Address.to_0x oper_addr);
  Logging.log "contr_addr=%s" (Address.to_0x contr_addr);
  Lwt_exn.bind (Ethereum_json_rpc.eth_get_balance (contr_addr, Latest))
    (fun x-> Logging.log "PCAV stage=%s value=%s" estr (TokenAmount.to_string x);
             Lwt_exn.return ())



let print_status_receipt : TransactionReceipt.t -> string =
  fun tr -> (TokenAmount.to_string tr.status)





let post_operation_general_kernel : Ethereum_chain.Operation.t -> Address.t -> TokenAmount.t -> TransactionReceipt.t Lwt_exn.t =
  fun operation sender value ->
  Logging.log "post_operation_kernel : beginning of function";
  let (gas_limit_val : TokenAmount.t option) = None in (* Some kind of arbitrary choice *)
  Logging.log "post_operation_general_kernel : before make_pre_transaction";
  Ethereum_user.make_pre_transaction ~sender operation ?gas_limit:gas_limit_val value
  >>= fun x_pretrans ->
  Ethereum_user.add_ongoing_transaction ~user:sender (Wanted x_pretrans)
  >>= fun (tracker_key, _, _) ->
  let (_, promise, _) = Ethereum_user.TransactionTracker.get () tracker_key in
  (Lwt.bind promise (function
  | Ethereum_user.FinalTransactionStatus.Failed (_, error) ->
     fail error (* bork "Cannot match this" *)
  | Ethereum_user.FinalTransactionStatus.Confirmed (_transaction, _signed, receipt) ->
     Logging.log "post_operation_general_kernel : Ok receipt, transaction_hash=%s" (Digest.to_0x receipt.transaction_hash);
     Logging.log "transaction status=%s" (print_status_receipt receipt);
     Lwt_exn.return receipt))


let post_operation_general : Ethereum_chain.Operation.t -> Address.t -> TokenAmount.t -> TransactionReceipt.t Lwt_exn.t =
  fun operation sender value ->
  let rec fct_submit : unit -> TransactionReceipt.t Lwt_exn.t =
    fun () ->
    Lwt.bind (post_operation_general_kernel operation sender value)
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
  Logging.log "post_state_update digest=%s" (Digest.to_0x digest);
  let (operation : Ethereum_chain.Operation.t) = make_state_update_call digest in
  let (value : TokenAmount.t) = TokenAmount.zero in
  let (oper_addr : Address.t) = Side_chain_server_config.operator_address in
  post_operation_general operation oper_addr value


(*
let inner_state_update_periodic_loop () =
  let open Lwt in
  let rec inner_loop : unit -> unit Lwt.t =
    fun () ->
    Side_chain_operator.retrieve_validated_rev_digest
    >>= fun x -> post_state_update
    >>= fun _ -> sleep_delay_exn Side_chain_server_config.period_state_update_f
    >>= fun () -> inner_loop ()
  in inner_loop () *)



let inner_state_update_request_loop () =
  let open Lwt in
  let digest_entry_ref : digest_entry ref = ref {revision=Revision.zero; digest=Digest.zero} in
  let rec inner_loop : unit -> unit Lwt.t =
    fun () ->
    Lwt_mvar.take request_state_update_mailbox
    >>= function
    | GetLastRevision (rev_u : Revision.t Lwt.u) ->
       Logging.log "State_update : GetLastRevision";
       Lwt.wakeup_later rev_u !digest_entry_ref.revision;
       inner_loop ()
    | GetLastCommit (digest_u : Digest.t Lwt.u) ->
       Logging.log "State_update : GetLastRevision";
       Lwt.wakeup_later digest_u !digest_entry_ref.digest;
       inner_loop ()
    | Submit ((new_digest, notify_u) : (Digest.t * TransactionReceipt.t OrExn.t Lwt.u)) ->
       let new_rev = Revision.add !digest_entry_ref.revision Revision.one in
       let new_digest_entry = {revision=new_rev; digest=new_digest} in
       digest_entry_ref := new_digest_entry;
       post_state_update new_digest
       >>= fun ereceipt ->
       Lwt.wakeup_later notify_u ereceipt;
       inner_loop ()
  in inner_loop ()


let start_state_update_operator () =
  Logging.log "Beginning of start_state_update_operator";
  Lwt.async inner_state_update_request_loop;
  Lwt_exn.return ()


(* Alert to take care of:
   ---lack of gas
   ---transaction not passed
 *)

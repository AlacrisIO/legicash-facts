open Legilogic_lib
open Signing
open Action
open Lwt_exn
open Logging
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



let print_status_receipt : TransactionReceipt.t -> string =
  fun tr -> (TokenAmount.to_string tr.status)


let post_operation_general_kernel : Ethereum_chain.Operation.t -> Address.t -> TokenAmount.t -> TransactionReceipt.t Lwt_exn.t =
  fun operation sender value ->
  let gas_limit_val = None in (* Some kind of arbitrary choice *)
  if state_update_log then
    log "post_operation_general_kernel : before make_pre_transaction";
  Ethereum_user.make_pre_transaction ~sender operation ?gas_limit:gas_limit_val value
  >>= fun x_pretrans ->
  Ethereum_user.add_ongoing_transaction ~user:sender (Wanted x_pretrans)
  >>= fun (tracker_key, _, _) ->
  let (_, promise, _) = Ethereum_user.TransactionTracker.get () tracker_key in
  (Lwt.bind promise (function
  | Ethereum_user.FinalTransactionStatus.Failed (_, error) ->
     fail error (* bork "Cannot match this" *)
  | Ethereum_user.FinalTransactionStatus.Confirmed (_transaction, _signed, receipt) ->
     if state_update_log then
       log "transaction status=%s" (print_status_receipt receipt);
     Lwt_exn.return receipt))


let post_operation_general : Ethereum_chain.Operation.t -> Address.t -> TokenAmount.t -> TransactionReceipt.t Lwt_exn.t =
  fun operation sender value ->
  let rec submit_operation : unit -> TransactionReceipt.t Lwt_exn.t =
    fun () ->
    Lwt.bind (post_operation_general_kernel operation sender value)
      (function
       | Error _error -> if state_update_log then
                           log "post_operation_general, Error case";
                         Lwt_exn.bind (Ethereum_watch.sleep_delay_exn 1.0) (fun () -> submit_operation ())
       | Ok ereceipt ->
          (let str = print_status_receipt ereceipt in
           let str_succ = "1" in
           if String.equal str str_succ then
             Lwt_exn.return ereceipt
           else
             Lwt_exn.bind (Ethereum_watch.sleep_delay_exn 1.0) (fun () -> submit_operation ())
          )
      ) in
  submit_operation ()


let post_state_update : Revision.t -> Digest.t -> TransactionReceipt.t Lwt_exn.t =
  fun operator_revision digest ->
  if state_update_log then
    log "post_state_update operator_revision=%s digest=%s" (Revision.to_string operator_revision)  (Digest.to_0x digest);
  let operation = make_state_update_call digest operator_revision in
  let value = TokenAmount.zero in
  let oper_addr = Side_chain_server_config.operator_address in
  post_operation_general operation oper_addr value


let inner_state_update_request_loop () =
  let open Lwt in
  let digest_entry_ref : digest_entry ref = ref {revision=Revision.zero; digest=Digest.zero} in
  let rec inner_loop : unit -> unit Lwt.t =
    fun () ->
    Lwt_mvar.take request_state_update_mailbox
    >>= function
    | GetLastRevision (rev_u : Revision.t Lwt.u) ->
       Lwt.wakeup_later rev_u !digest_entry_ref.revision;
       inner_loop ()
    | GetLastCommit (digest_u : Digest.t Lwt.u) ->
       Lwt.wakeup_later digest_u !digest_entry_ref.digest;
       inner_loop ()
    | Submit ((new_digest, notify_u) : (Digest.t * TransactionReceipt.t OrExn.t Lwt.u)) ->
       let new_rev = Revision.add !digest_entry_ref.revision Revision.one in
       let new_digest_entry = {revision=new_rev; digest=new_digest} in
       digest_entry_ref := new_digest_entry;
       let revision : Revision.t = Revision.of_int 742 in
       (* TODO: Clarify this. The value 742 is here for fun so that we know what problem happen.
        In reality, we need the state revision *)
       post_state_update revision new_digest
       >>= fun ereceipt ->
       Lwt.wakeup_later notify_u ereceipt;
       inner_loop ()
  in inner_loop ()


let start_state_update_operator () =
  if state_update_log then
    log "Beginning of start_state_update_operator";
  Lwt.async inner_state_update_request_loop;
  Lwt_exn.return ()


(* Alert to take care of:
   ---lack of gas
   ---transaction not passed
 *)

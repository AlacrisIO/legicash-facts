open Legilogic_lib
open Action

open Types

open Legilogic_ethereum
open Operator_contract
open Digesting
open Ethereum_chain
open Ethereum_json_rpc
open Side_chain_server_config

let state_update_log = true

type digest_entry =
  { revision : Revision.t
  ; digest : Digest.t}

type request_state_update =
  | Submit of (Digest.t * TransactionReceipt.t OrExn.t Lwt.u)
  | GetLastRevision of Revision.t Lwt.u
  | GetLastCommit of Digest.t Lwt.u

let request_state_update_mailbox : request_state_update Lwt_mvar.t = Lwt_mvar.create_empty ()


let last_hash = ref Digest.zero

let post_state_update : Revision.t -> Digest.t -> TransactionReceipt.t Lwt_exn.t =
  fun operator_revision operator_digest ->
  let open Lwt_exn in
  if state_update_log then
    Logging.log "post_state_update beginning";
  let my_operation = ref (make_state_update_call operator_digest operator_revision) in
  if (String.equal (Digest.to_string !last_hash) (Digest.to_string operator_digest)) then
    (my_operation := make_null_operation operator_digest operator_revision;
     if state_update_log then
       Logging.log "Putting a null operation"
    );
  last_hash := operator_digest;
  if state_update_log then
    Logging.log "post_state_update operator_revision=%s digest=%s" (Revision.to_string operator_revision) (Digest.to_0x operator_digest);
  let oper_addr = Side_chain_server_config.operator_address in
  Ethereum_user.post_operation ~operation:!my_operation ~sender:oper_addr ~value_send:TokenAmount.zero
  >>= fun x ->
  if state_update_log then
    Logging.log "After the post_operation of post_state_update";
  return x



let post_state_update_nocheck : Revision.t -> Digest.t -> TransactionReceipt.t Lwt_exn.t =
  fun operator_revision operator_digest ->
  let open Lwt_exn in
  if state_update_log then
    Logging.log "post_state_update_nocheck beginning";
  let operation = make_state_update_call_nocheck operator_digest operator_revision in
  if state_update_log then
    Logging.log "post_state_update_nocheck operator_revision=%s digest=%s" (Revision.to_string operator_revision) (Digest.to_0x operator_digest);
  let oper_addr = Side_chain_server_config.operator_address in
  Ethereum_user.post_operation ~operation:operation ~sender:oper_addr ~value_send:TokenAmount.zero
  >>= fun x ->
  if state_update_log then
    Logging.log "After the post_operation of post_state_update_nocheck";
  return x

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
    Logging.log "Beginning of start_state_update_operator";
  Lwt.async inner_state_update_request_loop;
  Lwt_exn.return ()


(* Alert to take care of:
   ---lack of gas
   ---transaction not passed
 *)

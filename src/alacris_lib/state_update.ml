open Legilogic_lib
open Action
open Signing
open Types

open Legilogic_ethereum
open Operator_contract
open Digesting
open Ethereum_chain

let state_update_log = true


let last_hash = ref Digest.zero
let first_nontrivial_hash = ref Digest.zero


let post_state_update : operator:Address.t -> operator_revision:Revision.t -> operator_digest:Digest.t -> unit Lwt_exn.t =
  fun ~operator ~operator_revision ~operator_digest ->
  let open Lwt_exn in
  if state_update_log then
    Logging.log "post_state_update operator_revision=%s digest=%s" (Revision.to_string operator_revision) (Digest.to_0x operator_digest);
  if (String.equal (Digest.to_string !last_hash) (Digest.to_string operator_digest)) then
    (if state_update_log then
       Logging.log "Same hash as before. No need to do anything";
     return ()
    )
  else
    (if state_update_log then
       Logging.log "New hash, doing a state_update";
     last_hash := operator_digest;
     get_contract_address ()
     >>= fun contract_address ->
     let operation = make_state_update_call ~contract_address ~operator_digest ~operator_revision in
     Ethereum_user.post_operation ~operation:operation ~sender:operator ~value_send:TokenAmount.zero
     >>= fun _ ->
     if state_update_log then
       Logging.log "After the post_operation of post_state_update";
     return ()
    )


let post_state_update_nocheck : operator:Address.t -> operator_revision:Revision.t -> operator_digest:Digest.t -> unit Lwt_exn.t =
  fun ~operator ~operator_revision ~operator_digest ->
  let open Lwt_exn in
  if state_update_log then
    Logging.log "post_state_update beginning";
  let null_oper = ref false in
  if (String.equal (Digest.to_string !last_hash) (Digest.to_string operator_digest)) then
    (if state_update_log then
       Logging.log "previous hash identical to this one. Nothing to be done";
     null_oper := true
    );
  if (String.equal (Digest.to_string !last_hash) (Digest.to_string Digest.zero)) then
    (if state_update_log then
       Logging.log "previous hash is zero. So first commit. Nothing to be done";
     first_nontrivial_hash := operator_digest;
     null_oper := true
    );
  if (String.equal (Digest.to_string !first_nontrivial_hash) (Digest.to_string operator_digest)) then
    (if state_update_log then
       Logging.log "operator_digest is the same as first_nontrivial_hash. Nothing to be done";
     null_oper := true
    );
  last_hash := operator_digest;
  if !null_oper then
    return ()
  else
    (let operation = make_state_update_call operator_digest operator_revision in
     if state_update_log then
       Logging.log "post_state_update operator_revision=%s digest=%s" (Revision.to_string operator_revision) (Digest.to_0x operator_digest);
     Ethereum_user.post_operation ~operation:operation ~sender:operator ~value_send:TokenAmount.zero
     >>= fun _ ->
     if state_update_log then
       Logging.log "After the post_operation of post_state_update";
     return ()
    )



(* Alert to take care of:
   ---lack of gas
   ---transaction not passed
 *)

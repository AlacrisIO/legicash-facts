open Legilogic_lib
open Action
open Signing
open Types

open Legilogic_ethereum
open Operator_contract
open Digesting
open Side_chain
open Side_chain_operator
open Side_chain_server_config

let state_update_log = true

let last_hash = ref Digest.zero
let first_nontrivial_hash = ref Digest.zero

let small_money_transfer_recipient_sender : recipient:Address.t -> sender:Address.t -> unit Lwt_exn.t =
  fun ~recipient ~sender ->
  if state_update_log then
    Logging.log "small_money_transfer_recipient_sender";
  let open Lwt_exn in
  let transfer_amount = TokenAmount.of_string "500000000000" in
  let pre_transaction = Ethereum_user.transfer_tokens ~recipient transfer_amount in
  Ethereum_user.post_pretransaction pre_transaction sender
  >>= fun _ -> return ()

let small_money_transfer : int -> unit Lwt_exn.t =
  fun pos ->
  if state_update_log then
    Logging.log "small_money_transfer. Doing pos=%i" pos;
  let heckle_address = Signing.Test.heckle_address in
  let jeckle_address = Signing.Test.jeckle_address in
  if (pos == 0) then
    small_money_transfer_recipient_sender ~recipient:heckle_address ~sender:jeckle_address
  else
    small_money_transfer_recipient_sender ~recipient:jeckle_address ~sender:heckle_address

let small_transfer_status_ref = ref 0

let post_state_update : operator:Address.t -> confirmed_state_update:StateUpdate.t -> unit Lwt_exn.t =
  fun ~operator ~confirmed_state_update ->
  let open Lwt_exn in
  if state_update_log then
    Logging.log "post_state_update confirmed_state_update.revision=%s digest=%s" (Revision.to_string confirmed_state_update.revision) (Digest.to_0x confirmed_state_update.state);
  if (String.equal (Digest.to_string !last_hash) (Digest.to_string confirmed_state_update.state)) then
    (if state_update_log then
       Logging.log "Same hash as before. No need to do state_update. Instead doing small_money_transfer";
     small_transfer_status_ref := 1 - !small_transfer_status_ref;
     small_money_transfer !small_transfer_status_ref
    )
  else
    (if state_update_log then
       Logging.log "New hash, doing a state_update";
     last_hash := confirmed_state_update.state;
     get_contract_address ()
     >>= fun contract_address ->
     let operation = make_state_update_call ~contract_address ~confirmed_state_update in
     Ethereum_user.post_operation ~operation:operation ~sender:operator ~value_send:TokenAmount.zero
     >>= fun _ ->
     if state_update_log then
       Logging.log "After the post_operation of post_state_update";
     return ()
    )


(* TODO for a state_update_deadline_in_blocks somewhere *)
let rec inner_state_update_periodic_loop : Address.t -> unit Lwt_exn.t =
  fun operator ->
  let open Lwt_exn in
  retrieve_validated_state_update ()
  >>= fun confirmed_state_update -> post_state_update ~operator ~confirmed_state_update
  >>= fun () -> sleep_delay_exn Side_chain_server_config.state_update_period_in_seconds_f
  >>= fun () -> inner_state_update_periodic_loop operator

let start_state_update_periodic_daemon address =
  register_keypair "heckle" Signing.Test.heckle_keys;
  register_keypair "jeckle" Signing.Test.jeckle_keys;
  if state_update_log then
    Logging.log "Beginning of start_state_update_periodic_operator wait=%f" Side_chain_server_config.state_update_period_in_seconds_f;
  Lwt.async (fun () -> inner_state_update_periodic_loop address);
  Lwt_exn.return ()


module Test = struct

  let post_state_update_for_test : operator:Address.t -> confirmed_state_update:StateUpdate.t -> unit Lwt_exn.t =
    fun ~operator ~confirmed_state_update ->
    let open Lwt_exn in
    if state_update_log then
      Logging.log "post_state_update beginning";
    let null_oper = ref false in
    if (String.equal (Digest.to_string !last_hash) (Digest.to_string confirmed_state_update.state)) then
      (if state_update_log then
         Logging.log "previous hash identical to this one. Nothing to be done";
       null_oper := true
      );
    if (String.equal (Digest.to_string !last_hash) (Digest.to_string Digest.zero)) then
      (if state_update_log then
         Logging.log "previous hash is zero. So first commit. Nothing to be done";
       first_nontrivial_hash := confirmed_state_update.state;
       null_oper := true
      );
    if (String.equal (Digest.to_string !first_nontrivial_hash) (Digest.to_string confirmed_state_update.state)) then
      (if state_update_log then
         Logging.log "confirmed_state_update.state is the same as first_nontrivial_hash. Nothing to be done";
       null_oper := true
      );
    last_hash := confirmed_state_update.state;
    if !null_oper then
      return ()
    else
      (get_contract_address ()
       >>= fun contract_address ->
       let operation = make_state_update_call ~contract_address ~confirmed_state_update in
       if state_update_log then
         Logging.log "post_state_update revision=%s digest=%s" (Revision.to_string confirmed_state_update.revision) (Digest.to_0x confirmed_state_update.state);
       Ethereum_user.post_operation ~operation:operation ~sender:operator ~value_send:TokenAmount.zero
       >>= fun _ ->
       if state_update_log then
         Logging.log "After the post_operation of post_state_update";
       return ()
      )

  let rec inner_state_update_for_test_periodic_loop : Address.t -> unit Lwt_exn.t =
    fun operator ->
    let open Lwt_exn in
    retrieve_validated_state_update ()
    >>= fun confirmed_state_update -> post_state_update_for_test ~operator ~confirmed_state_update
    >>= fun () -> sleep_delay_exn Side_chain_server_config.state_update_period_in_seconds_f
    >>= fun () -> inner_state_update_for_test_periodic_loop operator

  let start_state_update_for_test_periodic_daemon address =
    if state_update_log then
      Logging.log "Beginning of start_state_update_for_test_periodic_operator wait=%f" Side_chain_server_config.state_update_period_in_seconds_f;
    Lwt.async (fun () -> inner_state_update_for_test_periodic_loop address);
    Lwt_exn.return ()

end

(* Alert to take care of:
   ---lack of gas
   ---transaction not passed
 *)

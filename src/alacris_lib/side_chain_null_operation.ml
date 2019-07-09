open Legilogic_lib
open Action
open Signing

open Legilogic_ethereum
open Side_chain_server_config
open Side_chain

let side_chain_null_operation_log = true


let small_money_transfer_first_second : recipient:Address.t -> sender:Address.t -> unit Lwt_exn.t =
  fun ~recipient ~sender ->
  if side_chain_null_operation_log then
    Logging.log "small_money_transfer_first_second";
  let open Lwt_exn in
  let transfer_amount = TokenAmount.of_string "500000000000" in
  let pre_transaction = Ethereum_user.transfer_tokens ~recipient transfer_amount in
  Ethereum_user.post_pretransaction pre_transaction sender
  >>= fun _ -> return ()


let small_money_transfer : int -> unit Lwt_exn.t =
  fun pos ->
  if side_chain_null_operation_log then
    Logging.log "small_money_transfer. Doing pos=%i" pos;
  let alice_address = Signing.Test.alice_address in
  let bob_address = Signing.Test.bob_address in
  if (pos == 0) then
    small_money_transfer_first_second ~recipient:alice_address ~sender:bob_address
  else
    small_money_transfer_first_second ~recipient:bob_address ~sender:alice_address


let rec infinite_loop_transfer : int -> unit Lwt_exn.t =
  fun pos ->
  let open Lwt_exn in
  small_money_transfer pos
  >>= fun () -> sleep_delay_exn 5.0
  >>= fun () -> infinite_loop_transfer (1 - pos)



let start_null_operation_thread () =
  if side_chain_null_operation_log then
    Logging.log "Beginning of the null_operation_thread";
  if Side_chain_server_config.run_small_activity then
    Lwt.async (fun () -> infinite_loop_transfer 0);
  Lwt_exn.return ()


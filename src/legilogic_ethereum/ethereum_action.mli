open Legilogic_lib
open Signing
open Types
open Action

open Main_chain

val block_depth_for_confirmation : Revision.t
(** How many additional blocks should one wait for before to consider a transaction confirmed
    after it was included in the blockchain? *)

val issue_transaction : (Operation.t * TokenAmount.t * TokenAmount.t, Transaction.t) UserAsyncAction.arr

val transfer_tokens : (Address.t * TokenAmount.t, Transaction.t) UserAsyncAction.arr
(** Transfer tokens from one address to another on the main chain; asynchronous *)

val send_transaction : (Transaction.t, Digest.t) Lwt_exn.arr
(** Send a transaction, return its hash *)

val wait_for_confirmation : (Digest.t, Confirmation.t) Lwt_exn.arr
(** Wait until a transaction (identified by its hash) has been confirmed by the main chain *)

val main_chain_block_notification_stream :
  ?delay:float             (* Wait this long between polls of geth *)
  -> ?start_block:Revision.t (* Don't report until this block number has passed. *)
  -> ?get_block: (?timeout:float -> ?log:bool -> (unit, Revision.t) Lwt_exn.arr) (* Testing affordance. Don't use *)
  -> unit
  -> Revision.t AsyncStream.t Lwt.t (* Stream of block numbers *)
(** [main_chain_block_notification_stream () start delay] is an asynchronous
    stream of notifications that a new block has been observed, based on polling
    geth every [delay] seconds, and starting with block [start]*)

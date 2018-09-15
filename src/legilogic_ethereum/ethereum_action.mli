open Legilogic_lib
open Signing

open Main_chain

val issue_transaction : (Operation.t * TokenAmount.t * TokenAmount.t, Transaction.t) UserAsyncAction.arr

val transfer_tokens : (Address.t * TokenAmount.t, Transaction.t) UserAsyncAction.arr
(** transfer tokens from one address to another on the main chain; asynchronous *)

val wait_for_confirmation : (Transaction.t, Confirmation.t) UserAsyncAction.arr
(** wait until a transaction has been confirmed by the main chain; asynchronous *)

val main_chain_block_notification_stream
  : unit
  -> ?delay:float             (* Wait this long between polls of geth *)
  -> start_block : int option (* Don't report until this block number has passed. *)
  -> Types.Revision.t Action.EventStream.t (* Stream of block numbers *)
(** [main_chain_block_notification_stream () start delay] is an asynchronous
    stream of notifications that a new block has been observed, based on polling
    geth every [delay] seconds, and starting with block [start]*)

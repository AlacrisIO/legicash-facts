open Legilogic_lib
open Signing

open Main_chain

val issue_transaction : (Operation.t * TokenAmount.t * TokenAmount.t, TransactionSigned.t) UserAsyncAction.arr

val transfer_tokens : (Address.t * TokenAmount.t, TransactionSigned.t) UserAsyncAction.arr
(** transfer tokens from one address to another on the main chain; asynchronous *)

val wait_for_confirmation : (TransactionSigned.t, Confirmation.t) UserAsyncAction.arr
(** wait until a transaction has been confirmed by the main chain; asynchronous *)

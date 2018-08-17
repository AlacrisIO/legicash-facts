open Db
open Main_chain

val transfer_tokens : (Address.t * TokenAmount.t, TransactionSigned.t) UserAsyncAction.arr
(** transfer tokens from one address to another on the main chain; asynchronous *)

val deposit : (Address.t * TokenAmount.t, TransactionSigned.t) UserAsyncAction.arr
(** deposit user tokens to facilitator on the main chain; asynchronous *)

val wait_for_confirmation : (TransactionSigned.t, Confirmation.t) UserAsyncAction.arr
(** wait until a transaction has been confirmed by the main chain; asynchronous *)

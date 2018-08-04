open Db
open Main_chain

val transfer_tokens : (Address.t * TokenAmount.t, TransactionSigned.t) user_async_action
(** transfer tokens from one address to another on the main chain; asynchronous *)

val deposit : (Address.t * TokenAmount.t, TransactionSigned.t) user_async_action
(** deposit user tokens to facilitator on the main chain; asynchronous *)

val wait_for_confirmation : (TransactionSigned.t, Confirmation.t) user_async_action
(** wait until a transaction has been confirmed by the main chain; asynchronous *)

open Crypto
open Main_chain

val transfer_tokens : (Address.t * TokenAmount.t, TransactionSigned.t) user_async_action
(** transfer tokens from one address to another on the main chain; asynchronous *)

val wait_for_confirmation : (TransactionSigned.t, Confirmation.t) user_async_action
(** wait until a transaction has been confirmed by the main chain; asynchronous *)

val withdraw_tokens : Side_chain.facilitator_state -> Address.t -> TokenAmount.t -> Digest.t Lwt.t
(** call contract on main chain, which will transfer tokens from facilitator to user; result is
    main chain transaction hash *)

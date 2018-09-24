open Legilogic_lib
open Yojsoning
open Persisting
open Signing
open Types
open Action

open Ethereum_chain
open Ethereum_json_rpc

module FinalTransactionStatus : sig
  type t =
    [ `Confirmed of Transaction.t * Confirmation.t
    | `Invalidated of Transaction.t * yojson ]
  include PersistableS with type t := t
end

module TransactionStatus : sig
  type t =
    [ FinalTransactionStatus.t
    | `Signed of Transaction.t * SignedTransaction.t
    | `Sent of Transaction.t * SignedTransaction.t * Digest.t ]
  include PersistableS with type t := t
  val transaction : t -> Transaction.t
end

module TransactionTracker : sig
  type t =
    { address : Address.t
    ; revision : Revision.t
    ; promise : FinalTransactionStatus.t Lwt.t
    ; get : unit -> TransactionStatus.t }
  val make : Address.t -> Revision.t -> TransactionStatus.t -> t
  val load : Address.t -> Revision.t -> t
  include PersistableS with type t := t
end

module OngoingTransactions : sig
  include Trie.SimpleTrieS with type key = Revision.t and type value = TransactionTracker.t
  val keys : t -> Revision.t list
  val load : Address.t -> Revision.t list -> t
end

(** State for the user client.
    confirmed_state is a digest of the confirmed Ethereum_chain.State that this is relative to.
    confirmed_balance is the balance of the user account relative to that confirmed_state.
*)
module UserState : sig
  type t =
    { address: Address.t
    ; transaction_counter: Revision.t
    ; ongoing_transactions: OngoingTransactions.t }
  [@@deriving lens { prefix=true }]
  include PersistableS with type t := t
  val get : Address.t -> t SimpleActor.t Lwt.t
end

module UserAction : ActionS with type state = UserState.t
module UserAsyncAction : AsyncActionS with type state = UserState.t

val user_action: Address.t -> ('i, 'o) UserAsyncAction.arr -> ('i, 'o) Lwt_exn.arr

val confirmation_of_transaction_receipt : TransactionReceipt.t -> Confirmation.t

val block_depth_for_confirmation : Revision.t
(** How many additional blocks should one wait for before to consider a transaction confirmed
    after it was included in the blockchain? *)

exception Still_pending
(** Exception thrown when you depend on a transaction being confirmed, but it's still pending *)

val wait_for_confirmation : (Digest.t, Confirmation.t) Lwt_exn.arr
(** Wait until a transaction (identified by its hash) has been confirmed by the main chain *)

val unlock_account : ?duration:int -> unit -> unit UserAsyncAction.t
(** unlocks account for given duration (in seconds) on net *)

val make_signed_transaction : Operation.t -> TokenAmount.t -> TokenAmount.t ->
  (Transaction.t * SignedTransaction.t) UserAsyncAction.t
(** Prepare a signed transaction, that you may later issue onto Ethereum network *)

val issue_transaction : (Transaction.t * SignedTransaction.t, TransactionTracker.t) UserAsyncAction.arr
(** Issue a signed transaction on the Ethereum network, return a tracker *)

val track_transaction : (TransactionTracker.t, FinalTransactionStatus.t) UserAsyncAction.arr
(** Track a transaction until it is either confirmed or invalidated *)

val confirm_transaction : (Transaction.t * SignedTransaction.t, Transaction.t * Confirmation.t) UserAsyncAction.arr
(** Issue a transaction on the Ethereum network, wait for it to be confirmed *)

val transfer_tokens : (Address.t * TokenAmount.t, Transaction.t * SignedTransaction.t) UserAsyncAction.arr
(** Transfer tokens from one address to another on the main chain; asynchronous *)

val send_transaction : (Transaction.t, Digest.t) Lwt_exn.arr
(** Send a transaction, return its hash *)

val main_chain_block_notification_stream :
  ?delay:float             (* Wait this long between polls of geth *)
  -> ?start_block:Revision.t (* Don't report until this block number has passed. *)
  -> ?get_block: (?timeout:float -> ?log:bool -> (unit, Revision.t) Lwt_exn.arr) (* Testing affordance. Don't use *)
  -> unit
  -> Revision.t AsyncStream.t Lwt.t (* Stream of block numbers *)
(** [main_chain_block_notification_stream () start delay] is an asynchronous
    stream of notifications that a new block has been observed, based on polling
    geth every [delay] seconds, and starting with block [start]*)

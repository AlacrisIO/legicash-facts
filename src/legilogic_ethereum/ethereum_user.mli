open Legilogic_lib
open Yojsoning
open Marshaling
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
    [ `Wanted of PreTransaction.t
    | `Signed of Transaction.t * SignedTransaction.t
    | `Sent of Transaction.t * SignedTransaction.t * Digest.t
    | FinalTransactionStatus.t ]
  include PersistableS with type t := t
  val of_final : FinalTransactionStatus.t -> t
  val pre_transaction : t -> PreTransaction.t
  val operation : t -> Operation.t
end

module TransactionTracker : sig
  module Key : sig
    type t= { user : Address.t; revision : Revision.t }
    include YojsonMarshalableS with type t := t
  end
  include PersistentActivityS
    with type key = Key.t
     and type context = unit
     and type state = TransactionStatus.t
     and type t = FinalTransactionStatus.t Lwt.t
end

module OngoingTransactions : sig
  include Trie.SimpleTrieS with type key = Revision.t and type value = unit
  val keys : t -> Revision.t list
  val of_keys : Revision.t list -> t
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

module UserAsyncAction : AsyncActionS with type state = UserState.t

val user_action: Address.t -> ('i, 'o) UserAsyncAction.arr -> ('i, 'o) Lwt_exn.arr

val add_ongoing_transaction : (TransactionStatus.t, TransactionTracker.t) UserAsyncAction.arr

val confirmation_of_transaction_receipt : TransactionReceipt.t -> Confirmation.t

val block_depth_for_confirmation : Revision.t
(** How many additional blocks should one wait for before to consider a transaction confirmed
    after it was included in the blockchain? *)

exception Still_pending
(** Exception thrown when you depend on a transaction being confirmed, but it's still pending *)

val unlock_account : ?duration:int -> unit -> unit UserAsyncAction.t
(** unlocks account for given duration (in seconds) on net *)

val make_signed_transaction : Address.t -> Operation.t -> TokenAmount.t -> TokenAmount.t ->
  (Transaction.t * SignedTransaction.t) Lwt_exn.t
(** Prepare a signed transaction, that you may later issue onto Ethereum network,
    from given address, with given operation, value and gas_limit *)

val issue_transaction : (Transaction.t * SignedTransaction.t, TransactionTracker.t) UserAsyncAction.arr
(** Issue a signed transaction on the Ethereum network, return a tracker *)

val track_transaction : (TransactionTracker.t, FinalTransactionStatus.t) UserAsyncAction.arr
(** Track a transaction until it is either confirmed or invalidated *)

val check_transaction_confirmed : (FinalTransactionStatus.t, Transaction.t * Confirmation.t) UserAsyncAction.arr
(** Check that the final transaction status is indeed confirmed, or fail *)

val confirm_transaction : (Transaction.t * SignedTransaction.t, Transaction.t * Confirmation.t) UserAsyncAction.arr
(** Issue a transaction on the Ethereum network, wait for it to be confirmed *)

val transfer_tokens : (Address.t * Address.t * TokenAmount.t, Transaction.t * SignedTransaction.t) Lwt_exn.arr
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

val exn_to_yojson : exn -> yojson

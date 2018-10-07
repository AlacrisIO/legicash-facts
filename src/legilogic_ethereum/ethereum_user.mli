open Legilogic_lib
open Marshaling
open Persisting
open Signing
open Types
open Action
open Trie

open Ethereum_chain
open Ethereum_json_rpc

module OngoingTransactionStatus : sig
  type t =
    | Wanted of PreTransaction.t
    (* TODO: add an intermediate state for when the nonce is known, but the gas price may vary ? *)
    | Signed of Transaction.t * SignedTransaction.t
  include PersistableS with type t := t
  val pre_transaction : t -> PreTransaction.t
end

module FinalTransactionStatus : sig
  type t =
    | Confirmed of Transaction.t * Confirmation.t
    | Failed of OngoingTransactionStatus.t * exn
  include PersistableS with type t := t
  val pre_transaction : t -> PreTransaction.t
end

module TransactionStatus : sig
  type t =
    | Ongoing of OngoingTransactionStatus.t
    | Final of FinalTransactionStatus.t
  include PersistableS with type t := t
  val of_ongoing : OngoingTransactionStatus.t -> t
  val of_final : FinalTransactionStatus.t -> t
  val pre_transaction : t -> PreTransaction.t
  val operation : t -> Operation.t
end

type nonce_operation = Peek | Next | Reset [@@deriving yojson]

module NonceTracker : sig
  module State : PersistableS with type t = Nonce.t option
  include PersistentActivityS
    with type key = Address.t
     and type context = unit
     and type state = State.t
     and type t = (nonce_operation, Revision.t) Lwter.arr
  val peek : (address, Revision.t) Lwter.arr
  val next : (address, Revision.t) Lwter.arr
  val reset : (address, unit) Lwter.arr
end

module TransactionTracker : sig
  module Key : sig
    type t= { user : Address.t; revision : Revision.t }
    include YojsonMarshalableS with type t := t
  end
  include PersistentActivityS
    with type key := Key.t
     and type context = unit
     and type state = TransactionStatus.t
     and type t = Key.t * FinalTransactionStatus.t Lwt.t * unit Lwt.u
end

(** State for the user client.
    confirmed_state is a digest of the confirmed Ethereum_chain.State that this is relative to.
    confirmed_balance is the balance of the user account relative to that confirmed_state.
*)
module UserState : sig
  type t =
    { address: Address.t
    ; transaction_counter: Revision.t
    ; ongoing_transactions: RevisionSet.t }
  [@@deriving lens { prefix=true }]
  include PersistableS with type t := t
end

module User : PersistentActivityS
  with type context = Address.t -> UserState.t SimpleActor.t
   and type key = Address.t
   and type state = UserState.t
   and type t = UserState.t SimpleActor.t

val add_ongoing_transaction : Address.t -> (OngoingTransactionStatus.t, TransactionTracker.t) Lwt_exn.arr

val confirmation_of_transaction_receipt : TransactionReceipt.t -> Confirmation.t

val block_depth_for_confirmation : Revision.t
(** How many additional blocks should one wait for before to consider a transaction confirmed
    after it was included in the blockchain? *)

exception Still_pending
(** Exception thrown when you depend on a transaction being confirmed, but it's still pending *)

val make_signed_transaction : Address.t -> Operation.t -> TokenAmount.t -> TokenAmount.t ->
  (Transaction.t * SignedTransaction.t) Lwt_exn.t
(** Prepare a signed transaction, that you may later issue onto Ethereum network,
    from given address, with given operation, value and gas_limit *)

val issue_transaction : (Transaction.t * SignedTransaction.t, TransactionTracker.t) Lwt_exn.arr
(** Issue a signed transaction on the Ethereum network, return a tracker *)

val track_transaction : (TransactionTracker.t, FinalTransactionStatus.t) Lwter.arr
(** Track a transaction until it is either confirmed or invalidated *)

val check_transaction_confirmed : (FinalTransactionStatus.t, Transaction.t * Confirmation.t) Lwt_exn.arr
(** Check that the final transaction status is indeed confirmed, or fail *)

val confirm_transaction : (Transaction.t * SignedTransaction.t, Transaction.t * Confirmation.t) Lwt_exn.arr
(** Issue a transaction on the Ethereum network, wait for it to be confirmed *)

val transfer_tokens : (Address.t * Address.t * TokenAmount.t, Transaction.t * SignedTransaction.t) Lwt_exn.arr
(** Transfer tokens from one address to another on the main chain; asynchronous *)

module Test : sig
  val get_prefunded_address : unit -> Address.t Lwt_exn.t
  (** get the prefunded address on the test network *)

  val display_balance : (string -> string -> 'a) -> Address.t -> TokenAmount.t -> 'a
  (** display an account having the given balance given a way to print address, optional name and balance *)

  val ensure_address_prefunded : Address.t -> TokenAmount.t -> Address.t -> unit Lwt_exn.t
  (** Given a prefunded address and a minimum amount of tokens, ensure that the second given address
      is prefunded to the tune of at least the given amount *)

  val ensure_test_account : ?min_balance:TokenAmount.t -> Address.t
    -> (string * keypair, unit) Lwt_exn.arr
  (** Given a minimum amount of tokens and a prefunded address with lots of them,
      ensure that our private Ethereum network has an account with given nickname and address,
      an empty geth password, and at least a billion tokens in it. *)

  val fund_accounts : ?min_balance:TokenAmount.t -> (unit, unit) Lwt_exn.arr
  (** transfers funds from funding account to account with given address, if balance less than min_balance *)
end

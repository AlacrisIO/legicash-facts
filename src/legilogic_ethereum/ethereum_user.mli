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

(* TODO: add some [unit Lwt.u] to persist a handle to the transaction
   before to send any message on the network *)
val add_ongoing_transaction : Address.t -> (OngoingTransactionStatus.t, TransactionTracker.t) Lwt_exn.arr

val confirmation_of_transaction_receipt : TransactionReceipt.t -> Confirmation.t

val block_depth_for_confirmation : Revision.t
(** How many additional blocks should one wait for before to consider a transaction confirmed
    after it was included in the blockchain? *)

exception Still_pending
(** Exception thrown when you depend on a transaction being confirmed, but it's still pending *)

val check_confirmation_deep_enough : Confirmation.t -> Confirmation.t Lwt_exn.t

val check_confirmation_deep_enough_bool : Confirmation.t -> bool Lwt_exn.t

val issue_pre_transaction : Address.t -> (PreTransaction.t, TransactionTracker.t) Lwt_exn.arr
(** Issue a pre-transaction as transaction on the Ethereum network, return a tracker *)

val track_transaction : (TransactionTracker.t, FinalTransactionStatus.t) Lwter.arr
(** Track a transaction until it is either confirmed or invalidated *)

val check_transaction_confirmed : (FinalTransactionStatus.t, Transaction.t * Confirmation.t) Lwt_exn.arr
(** Check that the final transaction status is indeed confirmed, or fail *)

val confirm_pre_transaction : Address.t -> (PreTransaction.t, Transaction.t * Confirmation.t) Lwt_exn.arr
(** Issue a transaction on the Ethereum network, wait for it to be confirmed *)

val transfer_tokens : recipient:Address.t -> TokenAmount.t -> PreTransaction.t
(** PreTransaction to transfer tokens from one address to another *)

val make_pre_transaction : sender:Address.t -> Operation.t -> ?gas_limit:TokenAmount.t -> TokenAmount.t -> PreTransaction.t Lwt_exn.t

val create_contract : sender:Address.t -> code:Bytes.t -> ?gas_limit:TokenAmount.t -> TokenAmount.t
                      -> PreTransaction.t Lwt_exn.t
(** PreTransaction to create a contract *)

val call_function : sender:Address.t -> contract:Address.t -> call:Bytes.t
                    -> ?gas_limit:TokenAmount.t -> TokenAmount.t
                    -> PreTransaction.t Lwt_exn.t
(** Return a PreTransaction to call a function; asynchronous *)

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

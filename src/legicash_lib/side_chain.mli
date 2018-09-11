(* Types for LegiCash Facilitator side-chains *)
open Legilogic_lib
open Digesting
open Signing
open Persisting
open Types
open Merkle_trie

open Legilogic_ethereum
module TokenAmount = Main_chain.TokenAmount

(* TODO: use GADTs... but first teach ppx_deriving_yojson about them? *)

(** invoice sent from payee to payer
    TODO: should we specify a deadline for the invoice as part of on-chain data? In what unit?

    The memo identifies the invoice
    The merchant chooses this memo to match payments to invoices on his side;
    the customer must include the proper memo on its payment
    A memo is up to 63 characters max.
*)

module Invoice : sig
  type t = {recipient: Address.t; amount: TokenAmount.t; memo: string}
  [@@deriving lens { prefix=true }]
  include PersistableS with type t := t
end

(** an operation on a facilitator side-chain *)
module UserOperation : sig
  [@warning "-39"]
  type deposit_details =
    { deposit_amount: TokenAmount.t
    ; deposit_fee: TokenAmount.t
    ; main_chain_deposit: Main_chain.Transaction.t
    ; main_chain_deposit_confirmation: Main_chain.Confirmation.t
    ; deposit_expedited: bool }
  [@@deriving lens, yojson]

  type payment_details =
    {payment_invoice: Invoice.t; payment_fee: TokenAmount.t; payment_expedited: bool}
  [@@deriving lens, yojson]

  type withdrawal_details =
    {withdrawal_amount: TokenAmount.t; withdrawal_fee: TokenAmount.t}
  [@@deriving lens, yojson]

  type t =
    | Deposit of deposit_details
    | Payment of payment_details
    | Withdrawal of withdrawal_details

  include PersistableS with type t := t
end

(*
   | Settlement of settlement_details

   type settlement_details =
   { sender: public_key
   ; sender_facilitator: public_key
   ; recipient: public_key
   ; recipient_facilitator: public_key }
*)

(** Headers for a request to a facilitator

    Every client request that initiates a transaction comes with a request
    window, that puts a cap on the validity of the request in terms of inclusion
    in the main chain. Thus, the other parties cannot keep the requestor's
    resource on hold indefinitely. Additionally, the request may contain
    reference to the root of the main chain consensus, so that it is clear which
    fork the transaction happens in; in some cases, it might be OK to be active
    in multiple forks; in other cases, it might lead to the requestor being
    punished in both. Note that the request_window_t data could be summarized in
    a hash, so the details can be omitted in future state logs, saving space; 
    but the content would still need to be published for present validation, so 
    that is a space loss in the short run (and/or for long-term archivers).
    Alternatively, to save space, the root may not be stored in places where the
    validity requires the root to be the same as *the* known consensual root at
    the given date. *)
module RxHeader : sig
  type t =
    { facilitator: Address.t
    ; requester: Address.t
    ; requester_revision: Revision.t
    ; confirmed_main_chain_state_digest: digest (* Main_chain.State.t *)
    ; confirmed_main_chain_state_revision: Revision.t
    ; confirmed_side_chain_state_digest: digest (* State.t *)
    ; confirmed_side_chain_state_revision: Revision.t
    ; validity_within: Duration.t }
  [@@deriving lens { prefix=true }]
  include PersistableS with type t := t
end

(** Request from user to facilitator for operation on the side chain
    an operation, plus headers that provide a reference to the past and a timeout
*)
module UserTransactionRequest : sig
  type t = {rx_header: RxHeader.t; operation: UserOperation.t}
  [@@deriving lens { prefix=true }]
  include PersistableS with type t := t
  include SignableS with type t := t
end

(** header for a confirmation from a facilitator:

    Future Optimization: when storing the transaction in memory or on disk,
    common data that can be deduced from context is omitted from storage and computed instead.
    But when signing the transaction and showing the evidence of the transaction to clients,
    the full data is included.

    give a revision so contradiction is trivial to check.
    Should we also provide log(n) digests to the previous confirmation
    whose revision is a multiple of 2**k for all k?
*)
module TxHeader : sig
  type t = {tx_revision: Revision.t; updated_limit: TokenAmount.t}
  [@@deriving lens { prefix=true }]
  include PersistableS with type t := t
end

module AdminTransactionRequest : sig
  (* Update the side-chain to take into account that the state of the side chain at given revision
     was confirmed on the main chain at given revision,
     thus allowing the facilitator to refresh their spending limit.
     All details are in the RxHeader and TxHeader.
  *)
  type t =
    | StateUpdate
  include PersistableS with type t := t
end

module UserQueryRequest : sig
  type t =
    | Get_account_balance of {address: Address.t}
    | Get_account_balances
    | Get_account_state of {address: Address.t}  (* side chain only *)
    | Get_account_status of {address: Address.t} (* side chain and main chain *)
    | Get_recent_transactions of {address: Address.t; count: Revision.t option}
    | Get_proof of {tx_revision: Revision.t}
  include PersistableS with type t := t
end

module AdminQueryRequest : sig
  type t =
    | Get_all_balances
    | Get_transaction_rate
  include PersistableS with type t := t
end

(* TODO: use GADT to split those cases? *)
module TransactionRequest : sig
  type t =
    [ `UserTransaction of UserTransactionRequest.t signed
    | `AdminTransaction of AdminTransactionRequest.t ]
  include PersistableS with type t := t
  val signed_request : t -> UserTransactionRequest.t signed
  val request : t -> UserTransactionRequest.t
end

module Query : sig
  type t =
    [ `UserQuery of UserQueryRequest.t
    | `AdminQuery of AdminQueryRequest.t ]
  include PersistableS with type t := t
end

module UserRequest : sig
  type t =
    [ `UserQuery of UserQueryRequest.t
    | `UserTransaction of UserTransactionRequest.t signed]
  include PersistableS with type t := t
end

module AdminRequest : sig
  type t =
    [ `AdminQuery of UserQueryRequest.t
    | `AdminTransaction of AdminTransactionRequest.t ]
  include PersistableS with type t := t
end

module ExternalRequest : sig
  type t =
    [ `UserQuery of UserQueryRequest.t
    | `UserTransaction of UserTransactionRequest.t signed
    | `AdminQuery of AdminQueryRequest.t ]
  include PersistableS with type t := t
end

(** A transaction confirmation from a facilitator, as included in the TransactionMap of its State:
    a request, plus headers that help validate against fraud.
*)
module Transaction : sig
  type t = { tx_header: TxHeader.t;
             (* TODO: replace the below with tx_operation: TxOperation.t *)
             tx_request: TransactionRequest.t }
  [@@deriving lens { prefix=true }]
  include PersistableS with type t := t
end

(* TODO: actually maintain the user_revision;
   pass rx_header to apply_side_chain_request (replacing _operation) to account for user_revision *)
(** public state of the account of a user with a facilitator as visible in the public side-chain *)
module AccountState : sig
  type t =
    { balance: TokenAmount.t (* amount of tokens in the account *)
    ; account_revision: Revision.t
    (* number of operations so far that concern this account.
       This both makes verification easier and prevents replay attacks
       like the equivalent Ethereum "nonce" *) }
  [@@deriving lens { prefix=true }]
  include PersistableS with type t := t

  (** Default (empty) state for a new facilitator *)
  val empty : t
end

module TransactionMap : MerkleTrieS with type key = Revision.t and type value = Transaction.t

module AccountMap : MerkleTrieS with type key = Address.t and type value = AccountState.t

(** public state of a facilitator side-chain, as posted to the court registry and main chain

    TODO: somehow store the following?
    ; confirmed_main_chain_state: digest (* Main_chain.State.t *)
    ; confirmed_side_chain_state: digest (* state previously posted on the above *)
    ; bond_posted: TokenAmount.t
    Or "just" keep changes in this data in the TransactionMap as objects beside requests?
    And keep a fast-access index to the latest entry numbers? As other trees?
    As parallel trees that share the same data structures?
    Index not just the confirmed, but also the pending confirmation?
*)
module State : sig
  (* NB: If you modify it, make sure to keep this in synch with TransactionCommitment.t *)
  type t = { facilitator_revision: Revision.t
           ; spending_limit: TokenAmount.t
           (* expedited limit still unspent since confirmation. TODO: find a good way to update it back up when things get confirmed *)
           ; accounts: AccountMap.t
           ; transactions: TransactionMap.t
           ; main_chain_transactions_posted: DigestSet.t }
  [@@deriving lens { prefix=true }]
  include PersistableS with type t := t
  val empty : t
end

(** TODO: verifier state and actions in some module Side_chain_verifier. *)

(** Fee structure for a facilitator
    NB: an important constraint is that we need to advertise this fee structure to users
    TODO: account for size of transaction if memo can be long.
    TODO: make fee structure updatable by posting a new fee schedule in advance.
*)
module FacilitatorFeeSchedule : sig
  type t =
    { deposit_fee: TokenAmount.t (* fee to accept a deposit *)
    ; withdrawal_fee: TokenAmount.t (* fee to accept a withdrawal *)
    ; per_account_limit: TokenAmount.t (* limit for pending expedited transactions per user *)
    ; fee_per_billion: TokenAmount.t
    (* function TokenAmount.t -> TokenAmount.t ? *) }
  [@@deriving lens { prefix=true }]
  include PersistableS with type t := t
end

(** Private state of a facilitator (as opposed to what's public in the side-chain)
    TODO: lawsuits? index expedited vs non-expedited transactions? multiple pending confirmations?
    Remember operations pending operations with the main chain?
    Include a Main_chain.user_state? State for dealing with the court registry? *)
module FacilitatorState : sig
  type t = { keypair: Keypair.t
           ; current: State.t
           ; fee_schedule: FacilitatorFeeSchedule.t }
  [@@deriving lens { prefix=true }]
  include PersistableS with type t := t
  val load : Address.t -> t
end

(** An attestation to commitment of a tx via a Merkle proof.
    The state that is root of the Merkle proof is not necessarily committed to the main chain,
    but is signed by Trent.
    The idea here is that Merkle proofs are vastly cheaper, CPU-wise, than signatures.
    (i.e. a signature takes ~3e5 cycles, whereas a proof should take only a ~1e4 cycles,
    and we need to do the proofs anyway --- NB: numbers unconfirmed.)
*)
module TransactionCommitment : sig
  type t =
    { transaction: Transaction.t (* The Transaction being committed to *)
    ; tx_proof: TransactionMap.Proof.t (* Merkle proof for the tx *)
    ; facilitator_revision: Revision.t (* From State.t *)
    ; spending_limit: TokenAmount.t (* From State.t *)
    ; accounts: Digest.t (* From State.t, digest only *)
    ; main_chain_transactions_posted: Digest.t (* From State.t, digest only *)
    ; signature: signature } (* Signature of the digest of the state reconstituted from the above *)
  [@@deriving lens { prefix=true }]
  include PersistableS with type t := t
end

(** For now, the side chain contract records in its Ethereum on-chain state claims for state updates,
    so we only need a commitment to a state that was thus updated on-chain.
    In the future, we may or may not want to include a Main_chain.Confirmation.t
    of the state update instead.
*)
module Confirmation = TransactionCommitment

type court_clerk_confirmation = {clerk: public_key; signature: signature} [@@deriving lens]

(** Side chain update to be posted on the main chain, including signatures by court registry clerks.
    The update itself has to be signed by the facilitator
    current_state is a digest of State.t *)
type update = {current_state: digest; availability_proof: court_clerk_confirmation list}
(*[@@deriving lens { prefix = true }]*)

exception Facilitator_not_found of string

exception No_facilitator_yet

exception Already_open

exception Already_closed

exception Account_closed_or_nonexistent

exception Invalid_confirmation

val challenge_duration : Duration.t

val one_billion_tokens : TokenAmount.t

(**
   When signing a message, we want to prepend to the message a long unique tag
   that makes it extremely hard for an attacker to pun messages
   between two different protocols using the same keys (e.g. for two different side-chains).
*)
module SignaturePrefix : sig
  include PersistableS
  val state_update : t
end

module Test : sig
  val trent_fee_schedule : FacilitatorFeeSchedule.t
  val trent_state : FacilitatorState.t
  (** sample facilitator state for testing *)
end

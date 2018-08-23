(* Types for LegiCash Facilitator side-chains *)
open Legilogic_lib
open Crypto
open Persisting
open Merkle_trie

open Legilogic_ethereum
module TokenAmount = Main_chain.TokenAmount

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
module Operation : sig
  type payment_details =
    {payment_invoice: Invoice.t; payment_fee: TokenAmount.t; payment_expedited: bool}
  [@@deriving lens, yojson]

  type deposit_details =
    { deposit_amount: TokenAmount.t
    ; deposit_fee: TokenAmount.t
    ; main_chain_deposit_signed: Main_chain.TransactionSigned.t
    ; main_chain_deposit_confirmation: Main_chain.Confirmation.t
    ; deposit_expedited: bool }
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
    Every client request that initiates a transaction comes with a request window,
    that puts a cap on the validity of the request in terms of inclusion in the main chain.
    Thus, the other parties cannot hold the requestor's resource indefinitely on hold.
    Additionally, the request may contains reference to the root of the main chain consensus,
    so that it is clear which fork the transaction happens in;
    in some cases, it might be OK to be active in multiple forks;
    in other cases, it might lead to the requestor being punished in both.
    Note that the request_window_t data could be summarized in a hash,
    so the details can be omitted in future state logs, saving space;
    but the content would still need to be published for present validation,
    so that is a space loss in the short run (and/or for long-term archivers).
    Alternatively, to save space, the root may not be stored in places where the validity
    requires the root to be the same as *the* known consensual root at the given date.
*)
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
module Request : sig
  type t = {rx_header: RxHeader.t; operation: Operation.t}
  [@@deriving lens { prefix=true }]
  include PersistableS with type t := t
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

(** A transaction confirmation from a facilitator:
    a request, plus headers that help validate against fraud.
*)
module Confirmation : sig
  type t = {tx_header: TxHeader.t; signed_request: Request.t signed}
  [@@deriving lens { prefix=true }]
  include PersistableS with type t := t
end

(* TODO: actually maintain the user_revision;
   pass rx_header to apply_side_chain_request (replacing _operation) to account for user_revision *)
(** public state of the account of a user with a facilitator as visible in the public side-chain *)
module AccountState : sig
  type t = {balance: TokenAmount.t; account_revision: Revision.t}
  [@@deriving lens { prefix=true }]
  include PersistableS with type t := t

  (** Default (empty) state for a new facilitator *)
  val empty : t
end

module ConfirmationMap : MerkleTrieS with type key = Revision.t and type value = Confirmation.t

module AccountMap : MerkleTrieS with type key = Address.t and type value = AccountState.t

(** public state of a facilitator side-chain, as posted to the court registry and main chain

    TODO: somehow store the following?
    ; confirmed_main_chain_state: digest (* Main_chain.State.t *)
    ; confirmed_side_chain_state: digest (* state previously posted on the above *)
    ; bond_posted: TokenAmount.t
    Or "just" keep changes in this data in the ConfirmationMap as objects beside requests?
    And keep a fast-access index to the latest entry numbers? As other trees?
    As parallel trees that share the same data structures?
    Index not just the confirmed, but also the pending confirmation?
*)
module State : sig
  type t = { facilitator_revision: Revision.t
           ; spending_limit: TokenAmount.t
           (* expedited limit still unspent since confirmation. TODO: find a good way to update it back up when things get confirmed *)
           ; accounts: AccountMap.t
           ; operations: ConfirmationMap.t
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

exception Invalid_operation of Operation.t

val challenge_duration : Duration.t

val one_billion_tokens : TokenAmount.t

module Test : sig
  val trent_fee_schedule : FacilitatorFeeSchedule.t
  val trent_state : FacilitatorState.t
  (** sample facilitator state for testing *)
end

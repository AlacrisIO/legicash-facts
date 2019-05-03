(* Types for Alacris side-chains *)
open Legilogic_lib
open Digesting
open Signing
open Persisting
open Types
open Merkle_trie

open Legilogic_ethereum
(** We use the very same TokenAmount type for the main chain and the side-chain,
    as indeed the tokens form a two-way peg. *)
module TokenAmount = Ethereum_chain.TokenAmount

(* TODO: use GADTs... but first teach ppx_deriving_yojson about them? *)

(** invoice sent from payee to payer
    TODO: should we specify a deadline for the invoice as part of on-chain data? In what unit?

    The memo identifies the invoice
    The merchant chooses this memo to match payments to invoices on his side;
    the customer must include the proper memo on its payment
    A memo is up to 63 characters max.
*)

module Invoice : sig
  [@warning "-39-32"]
  type t = {recipient: Address.t; amount: TokenAmount.t; memo: string}
  [@@deriving lens { prefix=true }, yojson, rlp]
  include PersistableS with type t := t
end

(** an operation on a operator side-chain *)
module UserOperation : sig
  [@warning "-39"]
  type deposit_details =
    { deposit_amount:                  TokenAmount.t
    ; deposit_fee:                     TokenAmount.t
    ; main_chain_deposit:              Ethereum_chain.SignedTransactionData.t
    ; main_chain_deposit_confirmation: Ethereum_chain.Confirmation.t
    ; request_guid:                    RequestGuid.t
    ; requested_at:                    Timestamp.t
    } [@@deriving lens, yojson, rlp]

  type payment_details =
    { payment_invoice:   Invoice.t
    ; payment_fee:       TokenAmount.t
    ; payment_expedited: bool
    ; request_guid:      RequestGuid.t
    ; requested_at:      Timestamp.t
    } [@@deriving lens, yojson, rlp]

  [@@warning "-32"]
  type withdrawal_details =
    { withdrawal_amount: TokenAmount.t
    ; withdrawal_fee:    TokenAmount.t
    ; request_guid:      RequestGuid.t
    ; requested_at:      Timestamp.t
    } [@@deriving lens, yojson, rlp]

  [@@@warning "-32"]
  type t =
    | Deposit    of deposit_details
    | Payment    of payment_details
    | Withdrawal of withdrawal_details
  [@@deriving yojson, rlp]
  (* TODO: do we need a two-phase send then receive (but only after settlement
   * send was settled) for non-expedited payments? *)

  val guid_and_utc : t -> RequestGuid.t * Timestamp.t

  include PersistableS with type t := t
end

(*
   | Settlement of settlement_details

   type settlement_details =
   { sender: public_key
   ; sender_operator: public_key
   ; recipient: public_key
   ; recipient_operator: public_key }
*)

(** Headers for a request to a operator

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
  [@warning "-39-32"]
  type t =
    { operator: Address.t
    ; requester: Address.t
    ; requester_revision: Revision.t
    ; confirmed_main_chain_state_digest: digest (* Ethereum_chain.State.t *)
    ; confirmed_main_chain_state_revision: Revision.t
    ; confirmed_side_chain_state_digest: digest (* State.t *)
    ; confirmed_side_chain_state_revision: Revision.t
    ; validity_within: Duration.t }
  [@@deriving lens { prefix=true }, yojson, rlp]
  include PersistableS with type t := t
end

(** Request from user to operator for operation on the side chain
    an operation, plus headers that provide a reference to the past and a timeout
*)
module UserTransactionRequest : sig
  [@warning "-39-32"]
  type t = {rx_header: RxHeader.t; operation: UserOperation.t}
  [@@deriving lens { prefix=true }, yojson, rlp]
  include PersistableS with type t := t
end

module SignedUserTransactionRequest : SignedS with type payload = UserTransactionRequest.t

(** header for a confirmation from a operator:

    Future Optimization: when storing the transaction in memory or on disk,
    common data that can be deduced from context is omitted from storage and computed instead.
    But when signing the transaction and showing the evidence of the transaction to clients,
    the full data is included.

    give a revision so contradiction is trivial to check.
    Should we also provide log(n) digests to the previous confirmation
    whose revision is a multiple of 2**k for all k?
*)
module TxHeader : sig
  [@warning "-39-32"]
  type t = {tx_revision: Revision.t; updated_limit: TokenAmount.t}
  [@@deriving lens { prefix=true }, yojson, rlp]
  include PersistableS with type t := t
end

module AdminTransactionRequest : sig
  (* Update the side-chain to take into account that the state of the side chain at given revision
     was confirmed on the main chain at given revision,
     thus allowing the operator to refresh their spending limit.
     All details are in the RxHeader and TxHeader.
  *)
  [@warning "-39-32"]
  type t =
    | StateUpdate of Revision.t * Digest.t
  [@@deriving yojson, rlp]
    (*| BondDeposit
      | BondWithdrawal *)
    (* Revision of the side_chain that was confirmed in the main chain,
       and the transaction hash for the state update transaction on the main chain. *)
  include PersistableS with type t := t
end

module UserQueryRequest : sig
  [@warning "-39-32"]
  type t =
    | Get_account_balance of {address: Address.t}
    | Get_account_balances
    | Get_contract_address
    | Get_account_state of {address: Address.t}  (* side chain only *)
    | Get_account_status of {address: Address.t} (* side chain and main chain *)
    | Get_recent_transactions of {address: Address.t; count: Revision.t option}
    | Get_proof of {tx_revision: Revision.t}
  [@@deriving yojson, rlp]
  include PersistableS with type t := t
end

module AdminQueryRequest : sig
  [@warning "-39-32"]
  type t =
    | Get_all_balances
    | Get_transaction_rate
  [@@deriving yojson, rlp]
  include PersistableS with type t := t
end

(* TODO: use GADT to split those cases? *)
module TransactionRequest : sig
  [@warning "-39-32"]
  type t =
    [ `UserTransaction of UserTransactionRequest.t signed
    | `AdminTransaction of AdminTransactionRequest.t ]
  [@@deriving yojson, rlp]
  include PersistableS with type t := t
  val signed_request : t -> UserTransactionRequest.t signed
  val request : t -> UserTransactionRequest.t
end

module Query : sig
  [@warning "-39-32"]
  type t =
    [ `UserQuery of UserQueryRequest.t
    | `AdminQuery of AdminQueryRequest.t ]
  [@@deriving yojson, rlp]
  include PersistableS with type t := t
end

module UserRequest : sig
  [@warning "-39-32"]
  type t =
    [ `UserQuery of UserQueryRequest.t
    | `UserTransaction of UserTransactionRequest.t signed]
  [@@deriving yojson, rlp]
  include PersistableS with type t := t
end

module AdminRequest : sig
  [@warning "-39-32"]
  type t =
    [ `AdminQuery of UserQueryRequest.t
    | `AdminTransaction of AdminTransactionRequest.t ]
  [@@deriving yojson, rlp]
  include PersistableS with type t := t
end

module ExternalRequest : sig
  [@warning "-39-32"]
  type t =
    [ `UserQuery of UserQueryRequest.t
    | `UserTransaction of UserTransactionRequest.t signed
    | `AdminQuery of AdminQueryRequest.t ]
  [@@deriving yojson, rlp]
  include PersistableS with type t := t
end

(** A transaction confirmation from a operator, as included in the TransactionMap of its State:
    a request, plus headers that help validate against fraud.
*)
module Transaction : sig
  [@warning "-39-32"]
  type t = { tx_header: TxHeader.t;
             (* TODO: replace the below with tx_operation: TxOperation.t *)
             tx_request: TransactionRequest.t }
  [@@deriving lens { prefix=true }, yojson, rlp]
  include PersistableS with type t := t
end

(* TODO: actually maintain the user_revision;
   pass rx_header to apply_side_chain_request (replacing _operation) to account for user_revision *)
(** public state of the account of a user with a operator as visible in the public side-chain *)
module AccountState : sig
  [@warning "-39-32"]
  type t =
    { balance: TokenAmount.t (* amount of tokens in the account *)
    ; account_revision: Revision.t
    (* number of operations so far that concern this account.
       This both makes verification easier and prevents replay attacks
       like the equivalent Ethereum "nonce" *) }
  [@@deriving lens { prefix=true }, yojson, rlp]
  include PersistableS with type t := t

  (** Default (empty) state for a new operator *)
  val empty : t
end

(** Module for Maps that for a operator map consecutive revisions from 0 to N each to
    the Transaction that has the given revision as Side_chain.TxHeader.tx_revision *)
module TransactionMap : MerkleTrieS with type key = Revision.t and type value = Transaction.t

(** Module for Maps that for a given operator map addresses each to the AccountState
    for the given address. *)
module AccountMap : MerkleTrieS with type key = Address.t and type value = AccountState.t

(** Public state of an operator's side-chain, as posted to the court registry and main chain.

The entire point and purpose of a blockchain state, whether it's a main chain or a side chain,
is that its contents and its structure can and will be verified algorithmically by the nodes
validating the protocol: it's a verifiable trace of execution of an ordered set of transactions.
This state must include not just the set of transactions, but a set of indexes sufficient
for the validators to efficiently assess whether the state is indeed correct. Efficiently here
means that each increment of chain state can be checked in a time polynomial in the logarithm
of the size of the state (a polynomial of small degree with small constant terms).

While operators may try to anticipate the future, to e.g. predict how when a block will be confirmed
(thus re-increasing the spending limit) and how much liquidity they should keep on each side-chain,
any speculative state belongs to the operators' private state, unless contractual commitments are
made based on such predictions. There is actually one such case: the computation of minimum deposit
required as collateral.

    TODO: somehow store the following?
    ; confirmed_main_chain_state: digest (* Ethereum_chain.State.t *)
    ; confirmed_side_chain_state: digest (* state previously posted on the above *)
    ; bond_posted: TokenAmount.t
    Or "just" keep changes in this data in the TransactionMap as objects beside requests?
    And keep a fast-access index to the latest entry numbers? As other trees?
    As parallel trees that share the same data structures?
    Index not just the confirmed, but also the pending confirmation?
*)
module State : sig
  (* NB: If you modify it, make sure to keep this in synch with TransactionCommitment.t *)
  [@warning "-39-32"]
  type t = { operator_revision: Revision.t
           ; spending_limit: TokenAmount.t
           (*           ; expedited_spending_limit: TokenAmount.t
                        (* limit to expedited operations. TODO: find a good way to update it back up when things get confirmed *)
                        ; expedited_spending_since_last_confirmed_state: TokenAmount.t *)
           ; accounts: AccountMap.t
           ; transactions: TransactionMap.t
           ; main_chain_transactions_posted: DigestSet.t }
  [@@deriving lens { prefix=true }, yojson, rlp]
  include PersistableS with type t := t
  val empty : t
end

module SignedState : SignedS with type payload = State.t

(** TODO: verifier state and actions in some module Side_chain_verifier. *)

(** Fee structure for a operator
    NB: an important constraint is that we need to advertise this fee structure to users
    TODO: account for size of transaction if memo can be long.
    TODO: make fee structure updatable by posting a new fee schedule in advance.
*)
module OperatorFeeSchedule : sig
  [@warning "-39-32"]
  type t =
    { deposit_fee: TokenAmount.t (* fee to accept a deposit *)
    ; withdrawal_fee: TokenAmount.t (* fee to accept a withdrawal *)
    ; per_account_limit: TokenAmount.t (* limit for pending expedited transactions per user *)
    ; fee_per_billion: TokenAmount.t
    (* function TokenAmount.t -> TokenAmount.t ? *) }
  [@@deriving lens { prefix=true}, yojson, rlp]
  include PersistableS with type t := t
end

(** An attestation to commitment of a tx via a Merkle proof.
    The state that is root of the Merkle proof is not necessarily committed to the main chain,
    but is signed by Trent.
    The idea here is that Merkle proofs are vastly cheaper, CPU-wise, than signatures.
    (i.e. a signature takes ~3e5 cycles, whereas a proof should take only a ~1e4 cycles,
    and we need to do the proofs anyway --- NB: numbers unconfirmed.)
*)
module TransactionCommitment : sig
  [@warning "-39-32"]
  type t =
    { transaction: Transaction.t (* The Transaction being committed to *)
    ; tx_proof: TransactionMap.Proof.t (* Merkle proof for the tx *)
    ; operator_revision: Revision.t (* From State.t *)
    ; spending_limit: TokenAmount.t (* From State.t *)
    ; accounts: Digest.t (* From State.t, digest only *)
    
    ; main_chain_transactions_posted: Digest.t (* From State.t, digest only *)
    ; signature: signature (* Signature of the digest of the state reconstituted from the above *)
    ; state_digest : Digest.t
    }
  [@@deriving lens { prefix=true }, yojson, rlp]
  include PersistableS with type t := t
end

(** For now, the side chain contract records in its Ethereum on-chain state claims for state updates,
    so we only need a commitment to a state that was thus updated on-chain.
    In the future, we may or may not want to include a Ethereum_chain.Confirmation.t
    of the state update instead.
*)
module Confirmation = TransactionCommitment

type court_clerk_confirmation = {clerk: public_key; signature: signature} [@@deriving lens]

(** Side chain update to be posted on the main chain, including signatures by court registry clerks.
    The update itself has to be signed by the operator
    current_state is a digest of State.t *)
type update = {current_state: digest; availability_proof: court_clerk_confirmation list}
(*[@@deriving lens { prefix = true }]*)

exception No_operator_yet

exception Already_open

exception Already_closed

exception Account_closed_or_nonexistent

exception Invalid_confirmation

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

val initial_fee_schedule : OperatorFeeSchedule.t

(** User side of Alacris side-chains

    What makes this module notably more complex than Ethereum_user is that we want to safely do
    distributed transactions between two blockchains (Ethereum and Alacris)
    while we're authoritative in neither. We must persist every message before we send it,
    or else we may lose our transaction state and end up paying twice for a deposit,
    losing a withdrawal. In the near future, we will also have to closely watch the consensus
    for "smart lawsuits" that we have to partake in and win.
*)

open Legilogic_lib
open Action
open Yojsoning
open Marshaling
open Signing
open Persisting
open Types
open Trie
open Merkle_trie

open Legilogic_ethereum

open Side_chain

module DepositWanted : sig
  type t =
    { operator:       Address.t
    ; deposit_amount: TokenAmount.t
    ; request_guid:   RequestGuid.t
    ; requested_at:   Timestamp.t
    } [@@deriving yojson]
end

module PaymentWanted : sig
  type t =
    { operator:          Address.t
    ; recipient:         Address.t
    ; amount:            TokenAmount.t
    ; memo:              string
    ; payment_expedited: bool
    ; request_guid:      RequestGuid.t
    ; requested_at:      Timestamp.t
    } [@@deriving yojson]
end

module WithdrawalWanted : sig
  type t =
    { operator:          Address.t
    ; withdrawal_amount: TokenAmount.t
    ; request_guid:      RequestGuid.t
    ; requested_at:      Timestamp.t
    } [@@deriving yojson]
end

module OngoingTransactionStatus : sig
  (* TODO: include a strong reference to the TransactionTracker, so it won't get garbage collected
     at just the wrong moment; make sure it can properly be persisted. Sigh. *)
  [@@@warning "-39"]
  type t =
    | DepositWanted of DepositWanted.t * TokenAmount.t

    | DepositPosted
      of DepositWanted.t
       * TokenAmount.t
       * Ethereum_user.TransactionTracker.Key.t

    | DepositConfirmed
      of DepositWanted.t
       * TokenAmount.t
       * Ethereum_chain.SignedTransactionData.t
       * Ethereum_chain.Confirmation.t

    | Requested            of UserTransactionRequest.t signed
    | SignedByOperator     of TransactionCommitment.t
    | PostedToRegistry     of TransactionCommitment.t
    | PostedToMainChain    of TransactionCommitment.t * PairRevisionDigest.t * Ethereum_chain.Confirmation.t
    | ConfirmedOnMainChain of TransactionCommitment.t * Ethereum_chain.Confirmation.t

  include PersistableS with type t := t

  val signed_request_opt: t -> UserTransactionRequest.t signed option
  val signed_request:     t -> UserTransactionRequest.t signed
  val request_opt:        t -> UserTransactionRequest.t option
  val request:            t -> UserTransactionRequest.t
end

module FinalTransactionStatus : sig
  type t =
    | SettledOnMainChain of TransactionCommitment.t * Ethereum_chain.Confirmation.t
    | Failed of OngoingTransactionStatus.t * exn
  include PersistableS with type t := t
  val signed_request_opt : t -> UserTransactionRequest.t signed option
end

(** side chain operation + knowledge about the operation *)
module TransactionStatus : sig
  type t =
    | Ongoing of OngoingTransactionStatus.t
    | Final of FinalTransactionStatus.t
  include PersistableS with type t := t
  val signed_request_opt : t -> UserTransactionRequest.t signed option
  val signed_request : t -> UserTransactionRequest.t signed
  val request_opt : t -> UserTransactionRequest.t option
  val request : t -> UserTransactionRequest.t
end

exception TransactionFailed of OngoingTransactionStatus.t * exn

type revision_generator = (unit, Revision.t) Lwter.arr

module TransactionTracker : sig
  module Key : sig
    type t = { user:         Address.t
             ; operator:     Address.t
             ; revision:     Revision.t
             ; request_guid: RequestGuid.t
             ; requested_at: Timestamp.t
             }
    include YojsonMarshalableS with type t := t
  end
  module State = TransactionStatus
  include PersistentActivityS
    with type context = revision_generator
     and type key = Key.t
     and type state = State.t
     and type t = Key.t * FinalTransactionStatus.t Lwt.t
  val wait : FinalTransactionStatus.t Lwt.t -> (TransactionCommitment.t * Ethereum_chain.Confirmation.t) Lwt_exn.t
end

(** private state a user keeps for his account with a operator *)
module UserAccountState : sig
  type t =
    { is_operator_valid: bool
    (* is the operator open for business and not known to be a liar? *)
    ; confirmed_state: AccountState.t
    (* TODO: replace pending_operations with
       ; transaction_counter : Revision.t
       ; ongoing_transactions : (Revision.t, TransactionStatus.t) Hasbtbl *)
    ; side_chain_revision: Revision.t
    (* The revision for the next on-side-chain operation *)
    ; transaction_counter: Revision.t
    (* The revision for the next in-client transaction *)
    ; ongoing_transactions: RevisionSet.t
    (* The set of revisions of ongoing in-client transactions *) }
  [@@deriving lens { prefix=true }]
  include PersistableS with type t := t
  (** User's view of the default (empty) state for a new operator *)
  val empty : t
end

module UserAccountStateMap : (MerkleTrieS with type key = Address.t and type value = UserAccountState.t)

(** User state (for Alice)
    For now, only one operator; but in the future, allow for many.

    Because the system is asynchronous and trustless, we must always maintain multiple views
    of the state of the system, and the status of each change in the system.

    In what follows, the numerals represent the corresponding branches of
    [KnowledgeStage.t]

    main chain status:
    J0 => J1, J2, J3; J1 => J2, J3; J2; J3

    side chain status:
    T0 => T1, T2, T3; T1 => T2, T3; T2 => T3 (ouch); T3 pulls in Ursula, a
    substitute operator(!), with *a separate data structure*
    (T2.J0: unknown to Judy yet
    OR T2.J1: almost confirmed by Judy (seen on the main blockchain, not confirmed yet)
    OR T2.J2: confirmed all the way to Judy
    OR T3.J0: Trent is a liar, got to do something about it -- send to Ursula
    OR T3.J3: LOSER: overridden by another lie of Trent that made it to Judy first.

    OR T3.U1: Trent is a liar, we sent the claim to Ursula
    OR T3.U2.J0: SOME Ursula accepted to replace Trent, Judy doesn't know
    OR T3.U2.J1: SOME Ursula accepted to replace Trent, posted to Judy, who didn't confirm yet
    OR T3.U2.J2: SOME Ursula accepted to replace Trent, posted to Judy, who confirmed
    OR T3.U3.J0: ALL Ursulas are dishonest, do your own thing, quick,
    do an individual exit or become a operator yourself, etc.
    OR T3.U3.J1: ALL Ursulas are dishonest, did our thing, waiting for confirmation.
    OR T3.U3.J2: ALL Ursulas are dishonest, did our thing, won.)

    A. We start from the last state S confirmed by Judy (summary of all operations of status J2).
    B. We want to maintain a list/set of operations that currently matter to the user.
    WHEN the operations are either confirmed or rejected by Judy (status J2 or J3),
    then the user may flush them out of active memory (but they are logged to disk for accounting).
    C. The operations are indexed somehow by knowledge_stage of Trent, Judy, etc.? by type?
    D. The user can play all the operations, and get an idea of what's confirmed,
    what's the expected result if everything goes right,
    what are the upper and lower bounds if some things go wrong.
    E. If Trent lies, we want to be able to divert the unconfirmed *incoming* transactions
    to Ursula and/or Judy (TODO: and their dependency history if any?)

    Notifications are free-form json objects (for now) to be displayed to the user based on
    e.g. failures.
*)
module UserState : sig
  type t =
    { address: Address.t
    ; operators: UserAccountStateMap.t
    ; notification_counter: Revision.t
    ; notifications: (Revision.t * yojson) list }
  [@@deriving lens { prefix=true }]
  include PersistableS with type t := t
end

module UserAsyncAction : AsyncActionS with type state = UserState.t

module User : sig
  val user_actor : Address.t -> UserState.t SimpleActor.t
  val make_tracker_context : Address.t -> Address.t -> TransactionTracker.context
  val action : Address.t -> ('i, 'o) UserAsyncAction.arr -> ('i, 'o) Lwt_exn.arr
  val transaction : Address.t -> ('a, TransactionTracker.t) UserAsyncAction.arr
    -> ('a, TransactionCommitment.t * Ethereum_chain.Confirmation.t) Lwt_exn.arr
end

val get_operator_fee_schedule : (Address.t, OperatorFeeSchedule.t) Lwt_exn.arr

val payment_fee_for : OperatorFeeSchedule.t -> TokenAmount.t -> TokenAmount.t
(** Compute the suitable fee for a payment of given amount *)

val deposit : (DepositWanted.t, TransactionTracker.t) UserAsyncAction.arr
(** Schedule a deposit on the Ethereum_chain, then a matching claim on the Alacris side-chain,
    then wait for confirmation on the main chain.
    Return a TransactionTracker.
*)

val withdrawal : (WithdrawalWanted.t, TransactionTracker.t) UserAsyncAction.arr
(** Schedule a withdrawal from the Alacris side-chain,
    then wait for confirmation on the Ethereum main chain,
    then make a matching claim on the Ethereum chain,
    then partake in any smart lawsuit,
    then execute the claim when confirmed.
    Return a TransactionTracker.
*)

val payment : (PaymentWanted.t, TransactionTracker.t) UserAsyncAction.arr
(** Schedule a payment on the Alacris side-chain,
    then wait for confirmation on the Ethereum main chain.
    Return a TransactionTracker.
*)

(*
   val push_side_chain_withdrawal_to_main_chain :
   Address.t -> (Transaction.t, Ethereum_chain.Transaction.t * Ethereum_json_rpc.TransactionReceipt.t) UserAsyncAction.arr
   (** An withdrawal made on the side chain need a corresponding action on the main chain.
   Specifically, while deposit and payment side-chain transactions require no follow up
   (deposit does have pre-requisites, though), a withdrawal transaction requires
   action on the main chain after the withdrawal was registered on the side-chain.
   TODO: handle persistence and asynchronous action gracefully.
 *)

   (** post an account_activity_status request for closing the account on the *main chain*. TBD *)
   val initiate_individual_exit : (unit, Ethereum_chain.Transaction.t) UserAsyncAction.arr

   (** TBD Flow 3 Step 3: Alice, who can see the final state of her account,
   posts on the main chain a demand for the final funds.
   This is signed then posted on the *main chain* by invoking the contract.
   This puts Trent and all verifiers on notice to check that Alice isn't lying,
   and post a lawsuit within a timeout window.
 *)
   val request_account_liquidation : (Invoice.t, Ethereum_chain.Transaction.t) UserAsyncAction.arr

   val collect_account_liquidation_funds : (unit, Ethereum_chain.Transaction.t) UserAsyncAction.arr
*)

val get_contract_address_from_client_exn : unit -> Address.t Lwt_exn.t

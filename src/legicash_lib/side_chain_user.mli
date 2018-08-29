(* Types for LegiCash Facilitator side-chains *)
open Legilogic_lib
open Action
open Yojsoning
open Signing
open Persisting
open Types
open Merkle_trie

open Legilogic_ethereum

open Side_chain

(** Stage of knowledge of one actor about an operation

    Main chain status: we assume Judy is honest and stable and never goes from Confirmed to Rejected.
    Transitions for the consensus:
    Unknown => Pending, Confirmed, Rejected
    Pending => Confirmed, Rejected

    Self status: Alice assumes she is honest and stable, but she relies on Trent who can lie.
    We don't need to represent self-status: if we don't know about it, we have nothing to represent;
    and if we do know about it, there is no transition about that, only about the status of Trent and Judy.

    Trent status: Alice weakly assumes honesty of Trent (or wouldn't even bother dealing with Trent),
    but has to take into account the possibility that Trent goes rogue at some point,
    and status of some operations go from Confirmed to Rejected via incompetence or malice.

    confirmation/rejection: either we move that to a functor so we have the proper kind,
    or we leave that aside.
*)
module KnowledgeStage : sig
  type t =
    | Unknown
    (* 0. that actor never heard of it *)
    | Pending
    (* 1. that actor heard of it but hasn't confirmed or rejected yet *)
    | Confirmed
    (* of operation_confirmation *)
    (* 2. that actor confirmed it *)
    | Rejected
    (* of operation_rejection *)
  (* 3. that actor rejected it, timed out, or lied, etc. *)
  include PersistableS with type t := t
end

(** side chain operation + knowledge about the operation *)
module Episteme : sig
  type t =
    { request: UserRequest.t signed
    ; transaction_option: Transaction.t option
    (* TODO: as distinguished from the Transaction, a Confirmation, which will be
       a merkle proof relating the transaction to a signed side-chain state.
       When, further, said signed side-chain state confirmed on the Main_chain, we're gold. *)
    ; main_chain_confirmation_option: Main_chain.Confirmation.t option }
  [@@deriving lens { prefix=true }]
  include PersistableS with type t := t
end

(** private state a user keeps for his account with a facilitator *)
module UserAccountState : sig
  type t =
    { facilitator_validity: KnowledgeStage.t
    (* do we know the facilitator to be a liar? If so, Rejected. Or should it be just a bool? *)
    ; confirmed_state: AccountState.t
    ; pending_operations: Episteme.t list }
  [@@deriving lens { prefix=true }]
  include PersistableS with type t := t
  (** User's view of the default (empty) state for a new facilitator *)
  val empty : t
end

module UserAccountStateMap : (MerkleTrieS with type key = Address.t and type value = UserAccountState.t)

(** User state (for Alice)
    For now, only one facilitator; but in the future, allow for many.

    Because the system is asynchronous and trustless, we must always maintain multiple views
    of the state of the system, and the status of each change in the system.

    In what follows, the numerals represent the corresponding branches of
    [KnowledgeStage.t]

    main chain status:
    J0 => J1, J2, J3; J1 => J2, J3; J2; J3

    side chain status:
    T0 => T1, T2, T3; T1 => T2, T3; T2 => T3 (ouch); T3 pulls in Ursula, a
    substitute facilitator(!), with *a separate data structure*
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
    do an individual exit or become a facilitator yourself, etc.
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
    { main_chain_user_state: Main_chain.UserState.t
    ; facilitators: UserAccountStateMap.t
    ; notifications: (Revision.t * yojson) list }
  [@@deriving lens { prefix=true }]
end

module UserAction : ActionS with type state = UserState.t
module UserAsyncAction : AsyncActionS with type state = UserState.t

val issue_user_request : (UserOperation.t, UserRequest.t signed) UserAction.arr

(** Actually do the deposit on the Main_chain *)
val deposit : (Address.t * TokenAmount.t, UserRequest.t signed) UserAsyncAction.arr

(*
   (** Notify the facilitator that the deposit was confirmed on the Main_chain and should be acknowledged
   on the Side_chain. *)
   val request_deposit : (TokenAmount.t * Main_chain.Confirmation.t, UserRequest.t signed) UserAction.arr
*)

(* An action made on the side chain may need a corresponding action on the main chain. Do them.
   Specifically, while deposit and payment side-chain transactions require no follow up
   (deposit does have pre-requisites, though), a withdrawal transaction requires
   action on the main chain after the withdrawal was registered on the side-chain.
   TODO: handle persistence and asynchronous action gracefully.
*)
val push_side_chain_action_to_main_chain :
  FacilitatorState.t -> (Transaction.t, Main_chain.Confirmation.t) UserAsyncAction.arr

(** Build a signed withdrawal request from specification *)
val withdrawal : (Address.t * TokenAmount.t, UserRequest.t signed) UserAsyncAction.arr

(** Compute the suitable fee for a payment of given amount *)
val payment_fee_for : FacilitatorFeeSchedule.t -> TokenAmount.t -> TokenAmount.t

(** Build a signed payment request from specification *)
val payment : (Address.t * Address.t * TokenAmount.t, UserRequest.t signed) UserAsyncAction.arr

(*
   (** post an account_activity_status request for closing the account on the *main chain*. TBD *)
   val initiate_individual_exit : (unit, Main_chain.Transaction.t) UserAsyncAction.arr

   (** TBD Flow 3 Step 3: Alice, who can see the final state of her account,
   posts on the main chain a demand for the final funds.
   This is signed then posted on the *main chain* by invoking the contract.
   This puts Trent and all verifiers on notice to check that Alice isn't lying,
   and post a lawsuit within a timeout window.
 *)
   val request_account_liquidation : (Invoice.t, Main_chain.Transaction.t) UserAsyncAction.arr

   val collect_account_liquidation_funds : (unit, Main_chain.Transaction.t) UserAsyncAction.arr
*)

val get_facilitator_fee_schedule : (unit, FacilitatorFeeSchedule.t) UserAsyncAction.arr

val mark_request_rejected : (UserRequest.t signed, unit) UserAction.arr

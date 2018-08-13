(* Types for LegiCash Facilitator side-chains *)
open Action
open Crypto
open Db
open Merkle_trie
open Yojson.Safe
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
    { request: Request.t signed
    ; confirmation_option: Confirmation.t signed option
    ; main_chain_confirmation_option: Main_chain.Confirmation.t option }
  [@@deriving lens, yojson]
  include PersistableS with type t := t
end

(** private state a user keeps for his account with a facilitator *)
module UserAccountStatePerFacilitator : sig
  type t =
    { facilitator_validity: KnowledgeStage.t
    (* do we know the facilitator to be a liar? If so, Rejected. Or should it be just a bool? *)
    ; confirmed_state: AccountState.t
    ; pending_operations: Episteme.t list }
  [@@deriving lens { prefix=true }, yojson]
  include PersistableS with type t := t
  (** User's view of the default (empty) state for a new facilitator *)
  val empty : t
end

module UserAccountStateMap : (MerkleTrieS with type key = Address.t and type value = UserAccountStatePerFacilitator.t)

(** User state (for Alice)
    For now, only one facilitator; but in the future, allow for many.

    Because the system is asynchronous and trustless, we must always maintain multiple views
    of the state of the system, and the status of each change in the system.

    main chain status:
    J0 => J1, J2, J3; J1 => J2, J3; J2; J3

    side chain status:
    T0 => T1, T2, T3; T1 => T2, T3; T2 => T3 (ouch); T3 pulls in Ursula(!), with *a separate data structure*
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
*)
type user_state =
  { main_chain_user_state: Main_chain.user_state
  ; facilitators: UserAccountStateMap.t }
[@@deriving lens]


(** function from 'input to 'output that acts on a user_state *)
type ('input, 'action) user_action = ('input, 'action, user_state) action

(** asynchronous function from 'input to 'output that acts on a user_state *)
type ('input, 'action) user_async_action = ('input, 'action, user_state) async_action


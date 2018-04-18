(* base.mli -- base types for Legicash platform
   This code is for basic infrastructure somewhat specific to Legicash
 *)

exception Timeout of string

exception Double_spend of string

(** 'a or exception *)
type 'a legi_result = ('a, exn) result

(** function from 'a to 'b that acts on a user_state *)
type ('a, 'b, 'c) action = 'c * 'a -> 'c * 'b legi_result

(** run the action, with side-effects and all *)
val effect_action: ('a, 'b, 'c) action -> 'c ref -> 'a -> 'b

(** apply an action, left to right *)
val do_action: ('c * 'a) -> ('a,'b,'c) action -> ('c * 'b legi_result)

(** compose two actions *)
val compose_actions: ('b, 'c, 's) action -> ('a, 'b, 's) action -> ('a, 'c, 's) action

(** compose two actions, left to right *)
val action_seq: ('a, 'b, 's) action -> ('b, 'c, 's) action -> ('a, 'c, 's) action

(** compose a list of actions (NB: monomorphic in type being passed around *)
val compose_action_list: (('a, 'a, 'c) action) list -> ('a, 'a, 'c) action

(** a pure action can read the global state, but not modify it, and not fail *)
type ('a, 'b, 'c) pure_action = 'c * 'a -> 'b

val action_of_pure_action : ('a, 'b, 'c) pure_action -> ('a, 'b, 'c) action

val compose_pure_actions: ('b, 'c, 'd) pure_action -> ('a, 'b, 'd) pure_action -> ('a, 'c, 'd) pure_action

val pure_action_seq: ('a, 'b, 'd) pure_action -> ('b, 'c, 'd) pure_action -> ('a, 'c, 'd) pure_action


(** A module for public keys *)
module PublicKey: Map.OrderedType

type public_key = PublicKey.t

(** private counterpart to public key *)
type private_key

(** morally a 256-bit integer *)
type int256

(** a signature for an object of type 'a *)
type 'a signature

val is_signature_valid: public_key -> 'a signature -> 'a -> bool

val make_signature: private_key -> 'a -> 'a signature

(** an object of type 'a with its signature by one party *)
type 'a signed = {payload: 'a; signature: 'a signature}

val sign: private_key -> 'a -> 'a signed

(** a cryptographic digest, "hash", for an object of type 'a *)
type 'a digest

val get_digest: 'a -> 'a digest

val null_digest: 'a digest

(** count of changes in an object.
    A positive integer less than 2**63, incremented at every change to a notional object's state.
    In Quake, we used to call that the mod-count (modification count).
    Can serve as a local counter in a Lamport clock.
    Should a user record have two of them, one for the user one for the server?
    Not for now: the server can use the global clock for the server.
 *)

(** sequence number for changes in a side-chain *)
module Revision = Int64

(** type of a timestamp *)
module Timestamp = Int64

(** type for a duration in terms of consensus cycles, for use in timeouts *)
module Duration = Int64

(** A conversation between two parties
    The type embodies an endpoint + state of communication + possibility of reconnection
    Maybe make the state of communication static, with a session type?
 *)
type conversation

(** a pure mapping from PublicKey.t to 'a suitable for use in interactive merkle proofs *)
module Data256Map: Map.S with type key = PublicKey.t

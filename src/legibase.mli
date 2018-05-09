(* base.mli -- base types for Legicash platform
   This code is for basic infrastructure somewhat specific to Legicash
 *)

exception Internal_error of string

exception Timeout of string

exception Double_spend of string

(** Secp256k1 context for signing and validation *)
val secp256k1_ctx : Secp256k1.Context.t

(** 'a or exception *)
type 'a legi_result = ('a, exn) result

(** function from 'a to 'b that acts on a user_state *)
type ('a, 'b, 'c) action = 'c * 'a -> 'c * 'b legi_result

(** run the action, with side-effects and all *)
val effect_action: ('a, 'b, 'c) action -> 'c ref -> 'a -> 'b

val no_action: ('a, 'a, 'b) action

val fail_action: exn -> ('a, 'b, 'c) action


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

exception Assertion_failed

(** given a pure_action returning a bool, make an action that asserts the bool is true *)
val action_assert: ('a, bool, 's) pure_action -> ('a, 'a, 's) action

type public_key = Secp256k1.Key.public Secp256k1.Key.t

(** private counterpart to public key *)
type private_key = Secp256k1.Key.secret Secp256k1.Key.t

(** a signature for an object of type 'a *)
type 'a signature

val is_signature_valid: public_key -> 'a signature -> 'a -> bool

val make_signature: private_key -> 'a -> 'a signature

(** an object of type 'a with its signature by one party *)
type 'a signed = {payload: 'a; signature: 'a signature}

val sign: private_key -> 'a -> 'a signed

(** a cryptographic digest, "hash", for an object of type 'a *)
module Digest = Data256

val get_digest: 'a -> 'a Digest.t

val null_digest: 'a Digest.t

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

module Address : sig
  type t
  val of_public_key : Secp256k1.Key.public Secp256k1.Key.t -> t
  val to_string : t -> string
  val equal : t -> t -> bool
end

val identity : 'a -> 'a
val konstant : 'a -> 'b -> 'a
val schoenfinkel : ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c
val defaulting : (unit -> 'a) -> 'a option -> 'a

module type MapS = sig
  include Map.S
  val lens : key -> ('a t, 'a) Lens.t
  val find_defaulting : (unit -> 'a) -> key -> 'a t -> 'a
end

module MapMake (Key : Map.OrderedType) : MapS with type key = Key.t

(** a pure mapping from PublicKey.t to 'a suitable for use in interactive merkle proofs *)
module AddressMap: MapS with type key = Address.t

module type SetS = sig
  include Set.S

  val lens : elt -> (t, elt) Lens.t

  val find_defaulting : (unit -> elt) -> elt -> t -> elt
end

(** ordered type with a type parameter *)
module type SetOrderedType = sig
  type 'a t
  val compare : 'a t -> 'a t -> int
end

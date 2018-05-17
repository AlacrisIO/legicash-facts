(* base.mli -- base types for Legicash platform
   This code is for basic infrastructure somewhat specific to Legicash
 *)

exception Timeout of string

exception Double_spend of string

val secp256k1_ctx : Secp256k1.Context.t
(** Secp256k1 context for signing and validation *)

(** 'output or exception *)
type 'output legi_result = ('output, exn) result

(** function from 'input to 'output that acts on a 'state and may return an exception *)
type ('input, 'output, 'state) action = 'state * 'input -> 'state * 'output legi_result

val effect_action : ('input, 'output, 'state) action -> 'state ref -> 'input -> 'output
(** run the action, with side-effects and all *)

val no_action : ('passthrough, 'passthrough, 'state) action

val fail_action : exn -> ('input, 'bottom, 'state) action

val do_action : 'state * 'input -> ('input, 'output, 'state) action -> 'state * 'output legi_result
(** apply an action, left to right *)

val ( ^|> ) : 'state * 'input -> ('input, 'output, 'state) action -> 'state * 'output legi_result

val compose_actions :
  ('intermediate, 'output, 'state) action -> ('input, 'intermediate, 'state) action
  -> ('input, 'output, 'state) action
(** compose two actions *)

val action_seq :
  ('input, 'intermediate, 'state) action -> ('intermediate, 'output, 'state) action
  -> ('input, 'output, 'state) action
(** compose two actions, left to right *)

val ( ^>> ) :
  ('input, 'intermediate, 'state) action -> ('intermediate, 'output, 'state) action
  -> ('input, 'output, 'state) action

val compose_action_list : ('a, 'a, 'state) action list -> ('a, 'a, 'state) action
(** compose a list of actions (NB: monomorphic in type being passed around *)

(** a pure action can read the global state, but not modify it, and not fail *)
type ('input, 'output, 'state) pure_action = 'state * 'input -> 'output

val action_of_pure_action :
  ('input, 'output, 'state) pure_action -> ('input, 'output, 'state) action

val compose_pure_actions :
  ('intermediate, 'output, 'state) pure_action -> ('input, 'intermediate, 'state) pure_action
  -> ('input, 'output, 'state) pure_action

val pure_action_seq :
  ('input, 'intermediate, 'state) pure_action -> ('intermediate, 'output, 'state) pure_action
  -> ('input, 'output, 'state) pure_action

exception Assertion_failed

val action_assert : ('a, bool, 'state) pure_action -> ('a, 'a, 'state) action
(** given a pure_action returning a bool, make an action that asserts the bool is true *)

type public_key = Secp256k1.Key.public Secp256k1.Key.t

(** private counterpart to public key *)
type private_key = Secp256k1.Key.secret Secp256k1.Key.t

(** a signature for an object of type 'a *)
type 'a signature

val is_signature_valid : public_key -> 'a signature -> 'a -> bool

val make_signature : private_key -> 'a -> 'a signature

(** an object of type 'a with its signature by one party *)
type 'a signed = {payload: 'a; signature: 'a signature}

val sign : private_key -> 'a -> 'a signed

(** a cryptographic digest, "hash", for an object of type 'a *)
module Digest : sig
  include module type of Data256

  val make : 'a -> t
end

type 'a digest = Digest.t

val null_digest : Digest.t

module DigestSet : sig
  include Set.S with type elt = Digest.t

  val lens : Digest.t -> (t, bool) Lens.t
end

(** count of changes in an object.
    A positive integer less than 2**63, incremented at every change to a notional object's state.
    In Quake, we used to call that the mod-count (modification count).
    Can serve as a local counter in a Lamport clock.
    Should a user record have two of them, one for the user one for the server?
    Not for now: the server can use the global clock for the server.
 *)

(** sequence number for changes in a side-chain *)
module Revision : Unsigned.S

(** type of a timestamp *)
module Timestamp : Unsigned.S

(** duration in terms of nanoseconds, for use in timeouts. TODO: should the unit be consensus cycles instead? *)
module Duration : Unsigned.S

(** A conversation between two parties
    The type embodies an endpoint + state of communication + possibility of reconnection
    Maybe make the state of communication static, with a session type?
 *)
type conversation

module Address : sig
  type t

  val of_public_key : Secp256k1.Key.public Secp256k1.Key.t -> t

  val of_string : string -> t

  val to_string : t -> string

  val equal : t -> t -> bool
end

module type MapS = sig
  include Map.S

  val lens : key -> ('a t, 'a) Lens.t

  val find_defaulting : (unit -> 'a) -> key -> 'a t -> 'a
end

val defaulting_lens : (unit -> 'b) -> ('a, 'b) Lens.t -> ('a, 'b) Lens.t
(** Assuming that the lens raises Not_found if the value is not found, and then using the provided default, modify the value found (or the default) and put it back in the object *)

module MapMake (Key : Map.OrderedType) : MapS with type key = Key.t

(** a pure mapping from PublicKey.t to 'a suitable for use in interactive merkle proofs *)
module AddressMap : MapS with type key = Address.t

open Lib
open Marshaling
open Digesting
open Persisting

module type UIntS = sig
  include Integer.UIntS
  include PersistableS with type t := t
end

module UInt16 : sig
  include module type of Integer.UInt16
  include PersistableS with type t := t
end

module UInt32 : sig
  include module type of Integer.UInt32
  include PersistableS with type t := t
end

module UInt64 : sig
  include module type of Integer.UInt64
  include PersistableS with type t := t
end

module UInt128 : sig
  include module type of Integer.UInt128
  include PersistableS with type t := t
end

module UInt192 : sig
  include module type of Integer.UInt192
  include PersistableS with type t := t
end

module Data160 : sig
  include module type of Integer.Data160
  include PersistableS with type t := t
end

module Data256 : sig
  include module type of Integer.Data256
  include PersistableS with type t := t
end

module UInt256 : sig
  include module type of Integer.UInt256
  include PersistableS with type t := t
end

module Digest : sig
  include module type of Digesting.Digest
  include PersistableS with type t := t
end

(** Sequence number for changes in a side-chain.
    A positive integer less than 2**64, incremented at every change to a notional object's state.
    In Quake, we used to call that the mod-count (modification count).
    Can serve as a local counter in a Lamport clock.
    Should a user record have two of them, one for the user one for the server?
    Not for now: the server can use the global clock for the server.
*)
module Revision : UIntS

module Timestamp : sig
  (** UTC milliseconds from UNIX epoch - see:
    * https://currentmillis.com
    * https://en.wikipedia.org/wiki/Unix_time
    * https://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html
    *
    * Note we need to emit values that are easily shared with JavaScript:
    * https://moment.github.io/luxon/docs/manual/parsing.html#unix-timestamps
    *)
  type t = UInt64.t
  include module type of UInt64 with type t := UInt64.t
  val now : unit -> t
end

(** duration in terms of nanoseconds, for use in timeouts. TODO: should the unit be consensus cycles instead? *)
module Duration : UIntS

module StringT : sig
  type t = string
  [@@deriving rlp]
  include PersistableS with type t := string
end

(* string as data to be printed as 0x strings in JSON *)
module Data : sig
  type t = string
  [@@deriving rlp]
  include PersistableS with type t := t
  include ShowableS with type t := t
end

(* This marshals to the empty string and json [null]. It is useful for
   type-system compatibility, e.g. you can have a tree with trivial leaves, to
   represent a set. *)
module Unit : sig
  type t = unit
  [@@deriving rlp]
  include PersistableS with type t := unit
  include ShowableS with type t := unit
end

module Exception : sig
  include PersistableS with type t = exn
  include ShowableS with type t := exn
end


(** Content-addressed pointer to an ['a]. *)
type +'a dv = {digest: Digest.t Lazy.t; value: 'a Lazy.t; mutable persisted: bool}
val dv_get : 'a dv -> 'a
val dv_digest : 'a dv -> digest
val dv_make : ('a -> digest) -> 'a -> 'a dv
val dv_of_digest : (string -> 'a) -> digest -> 'a dv
val dv_marshal : 'a dv marshaler
val dv_unmarshal : (string -> 'a) -> 'a dv unmarshaler
val dv_marshaling : (string -> 'a) -> 'a dv marshaling

module type DigestValueBaseS = sig
  include WrapS
  type digest
  val of_digest : digest -> t
  val digest : t -> digest
end

(** Asynchronously digestible and content-addressed persistable values. *)
module type DigestValueS = sig
  type value
  include DigestValueBaseS
    with type value := value
     and type t = value dv
     and type digest = Digest.t
  include PersistableS with type t := t
end

(** Asynchronously digestible, content-addressed persistable values. *)
module DigestValueType : sig
  type +'a t = 'a dv
  [@@deriving rlp]
end

(** Asynchronously digestible, content-addressed persistable values, with
    auto-generated methods. *)
module DigestValue (Value : PersistableS) : sig
  type value = Value.t
  type digest = Digest.t
  type t = value dv
  val get : t -> value
  val make : value -> t
  val of_digest : digest -> t
  include PersistableS with type t := t
end

module RequestGuid : sig
  type t = UInt128.t * UInt64.t * UInt64.t * UInt64.t * UInt192.t
  [@@deriving rlp]

  type from_string_result =
    | WellFormed of t
    | Malformed  of string

  val from_string : string -> from_string_result
  val to_string   : t      -> string
  val nil         : t

  include PersistableS with type t := t
end

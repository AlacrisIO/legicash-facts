open Lib
open Yojsoning
open Marshaling
open Crypto

(** Expresses methods needed to process a recursive object of type ['a]. *)
type 'a dependency_walking_methods =
  {
    (** Construct a cryptographic digest of ['a]. Used for content-addressable
       storage in addition to cryptographic validation. *)
    digest: 'a -> digest
  ; (** [marshal_string x] is the binary representation of [x] *)
    marshal_string: 'a -> string
  ; (** [make_persistent x] puts [x] in DB keyed to representation of [digest]
       *)
    make_persistent: ('a -> unit Lwt.t) -> 'a -> unit Lwt.t
  ; (** Logic for recursing into ['a] objects *)
    walk_dependencies: 'a dependency_walker }
(** Wrapper around [dependency_walker] needed by OCaml type system for universal
   quantification (i.e., the ['a .] at the start of the RHS.)  *)
and dependency_walking_context =
  { walk: 'a. 'a dependency_walker }
(** Expresses logic for asynchronously recursing into ['a] objects and
   processing them according to the logic encoded in the given methods and
   context. *)
and 'a dependency_walker = 'a dependency_walking_methods -> dependency_walking_context -> 'a -> unit Lwt.t

(** Trivial dependency walker. Does nothing  *)
val no_dependencies : 'a dependency_walker

(** [walk_dependency methods context x] recursively persists [x]. *)
val walk_dependency : 'a dependency_walker

(** [one_dependency f methods _methods context x] lifts f to a
   [dependency_walker] functor. *)
val one_dependency : ('a -> 'b) -> 'b dependency_walking_methods -> 'a dependency_walker

(** Sequential composition of two walkers. Result from one is passed to next. *)
val seq_dependencies : 'a dependency_walker -> 'a dependency_walker -> 'a dependency_walker

(** [normal_persistent f x] applies f, presumably a persisting operation, to x
   *)
val normal_persistent : ('a -> unit Lwt.t) -> 'a -> unit Lwt.t

(** Does nothing, on the assumption that object is already persisted *)
val already_persistent : ('a -> unit Lwt.t) -> 'a -> unit Lwt.t

(** Walker methods which just fail when invoked *)
val dependency_walking_not_implemented : 'a dependency_walking_methods

module type PrePersistableDependencyS = sig
  type t
  val make_persistent: (t -> unit Lwt.t) -> t -> unit Lwt.t
  val walk_dependencies : t dependency_walker
end

module type PrePersistableS = sig
  include PreYojsonMarshalableS
  include PrePersistableDependencyS with type t := t
end

(** Content-addressed persistence. *)
module type PersistableS = sig
  include PrePersistableS
  include MarshalableS with type t := t
  include DigestibleS with type t := t
  include YojsonableS with type t := t
  val dependency_walking : t dependency_walking_methods
  val save : t -> unit Lwt.t
end

(** Auto-defined methods for content-addressed persistence *)
module Persistable (P : PrePersistableS) : PersistableS with type t = P.t

(** Non-recursive persistence *)
module TrivialPersistable (P : PreYojsonMarshalableS) : PersistableS with type t = P.t

(** Persistable to json representation. *)
module YojsonPersistable (Y: PreYojsonableS) : PersistableS with type t = Y.t

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

module Data256 : sig
  include module type of Integer.Data256
  include PersistableS with type t := t
end

module UInt256 : sig
  include module type of Integer.UInt256
  include PersistableS with type t := t
end

module Digest : sig
  include module type of Crypto.Digest
  include PersistableS with type t := t
end

module Address : sig
  include module type of Crypto.Address
  include PersistableS with type t := t
end

module Signature : sig
  include module type of Crypto.Signature
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

module Timestamp : UIntS

(** duration in terms of nanoseconds, for use in timeouts. TODO: should the unit be consensus cycles instead? *)
module Duration : UIntS

module StringT : PersistableS with type t = string

(* This marshals to the empty string and json [null]. It is useful for
   type-system compatibility, e.g. you can have a tree with trivial leaves, to
   represent a set. *)
module Unit : sig
  include PersistableS with type t = unit
  include ShowableS with type t := unit
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

val db_value_of_digest : (string -> 'a) -> digest -> 'a

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

module UInt16int : PersistableS with type t = int

(** Do NOT use this module in production. Only for demos and temporary cut-throughs *)
module OCamlPersistable (T: TypeS) : PersistableS with type t = T.t

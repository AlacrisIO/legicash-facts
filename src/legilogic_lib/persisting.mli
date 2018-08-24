open Lib
open Yojsoning
open Marshaling
open Digesting

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

val db_value_of_digest : (string -> 'a) -> digest -> 'a

(** Auto-defined methods for content-addressed persistence *)
module Persistable (P : PrePersistableS) : PersistableS with type t = P.t

(** Non-recursive persistence *)
module TrivialPersistable (P : PreYojsonMarshalableS) : PersistableS with type t = P.t

(** Persistable to json representation. *)
module YojsonPersistable (Y: PreYojsonableS) : PersistableS with type t = Y.t

(** Do NOT use this module in production. Only for demos and temporary cut-throughs *)
module OCamlPersistable (T: TypeS) : PersistableS with type t = T.t

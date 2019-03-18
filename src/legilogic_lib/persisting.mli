open Lib
open Action
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

module type PreRlpYojsonablePersistableDependencyS = sig
  type t
  [@@deriving rlp]
  include PreYojsonableS with type t := t
  include PrePersistableDependencyS with type t := t
end

module type PrePersistableRlpS = sig
  type t
  [@@deriving rlp]
  include PrePersistableS with type t := t
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

module type PersistableRlpS = sig
  type t
  [@@deriving rlp]
  include PersistableS with type t := t
end

val db_value_of_digest : (string -> 'a) -> digest -> 'a

(** Auto-defined methods for content-addressed persistence *)
module Persistable (P : PrePersistableS) : PersistableS with type t = P.t

module PersistableOfRlp (P : PreRlpYojsonablePersistableDependencyS) : PersistableS with type t = P.t

module PersistableRlp (P : PrePersistableRlpS) : PersistableRlpS with type t = P.t

(** Non-recursive persistence *)
module TrivialPersistable (P : PreYojsonMarshalableS) : PersistableS with type t = P.t

(** Persistable to json representation. *)
module YojsonPersistable (Y: PreYojsonableS) : PersistableS with type t = Y.t

(** Do NOT use this module in production. Only for demos and temporary cut-throughs *)
module OCamlPersistable (T: TypeS) : PersistableS with type t = T.t


val persistent_actor_no_default_state : string -> ('key -> string) -> _ -> 'key -> _

(** PersistentActivity

    This module describes one or a set of persistent activities that each persist
    their running state into the database.

    Each activity is parameterized by a [context] (ambient entry points, shared by all activities),
    and a [key] that identifies the activity and that together with the module-wide [key_prefix]
    specifies where in the database the state will be stored. You can [get] an existing activity
    given the [context] and its [key], assuming it either already exists, has been saved in the
    database, or a method to create a default key was provided. Or you can [make] a new activity
    given the [context], the [key], and an action that provided a saving action will return the
    initial state, saved and committed or not as required by your protocol.

    The user provides the [Key] module, the [key_prefix], the [State] module,
    a [make_default_state] function to create a default state given the context and a key
    (and may raise an exception if this module provides no default state),
    and a [make_activity] function to recreate the activity given the context, the key,
    a [saving] action that will save a given state to the database, and the current state.

    The activity may be anything: a promise, a mailbox behind which sits a server, an actor,
    a record containing one or many of the previous, or a function abstracting away the
    use of the previous, etc. Getting an existing or making a new activity may or may not
    start a new management thread in the background, and may or may not recursively activate
    more sub-activities.

    Saving, committing, using transactions, etc., is entirely up to the user, and not covered
    by the PersistentActivity module. The PersistentActivity module provides a saving action
    for the activity users to use.
*)
module type PersistentActivityBaseS = sig
  module Key : YojsonMarshalableS
  (** Key used to locate the actor state in the DB, by marshaling it with given prefix
      For global objects, the key is unit, and the system better load the object at restart.
      For other objects, the key must contain a revision number that must be incremented,
      an identifying tuple or a digest thereof, or a random nonce that must be generated.
      The key must itself be somehow registered and persisted in an object managing the wider scope
      with a narrower key (transitively, down to a global object), as part of the same transaction
      that creates the object.
  *)

  val key_prefix : string
  (** a prefix to the database key *)

  type context
  (** Inherited context for the actor: beyond the key, data and activities from the surrounding scope. *)

  module State : PersistableS
  (** State of the actor, to be stored in the DB *)

  type t
  (** The external type of the in-image activity, as visible within the rest of the application:
      an actor, a promise, a mailbox, a ref, of a record of several of previous, etc. *)

  val make_default_state : context -> Key.t -> State.t
  (** create a default state if none is present in the DB.
      May call [persistent_actor_no_default_state key_prefix Key.to_yojson_string]
      if there is no meaningful such default state. *)

  val make_activity : context -> Key.t -> (State.t, State.t) Lwter.arr -> State.t -> t
  (** Make the activity.

      Given context, Key.t, an action that saves state to the database (but doesn't commit), and
      an initial or current state, return the activity, after launching any background thread,
      activating any other activity, etc.
  *)
end

(** See documentation above for [PersistentActivityBaseS] and for [Action.SimpleActor] *)
module type PersistentActivityS = sig
  type context
  type key
  type state
  type t
  val make : context -> key -> ((state, state) Lwter.arr -> state Lwt.t) -> t Lwt.t
  val get : context -> key -> t
end
module PersistentActivity (Base: PersistentActivityBaseS) :
  PersistentActivityS
  with type context = Base.context
   and type key = Base.Key.t
   and type state = Base.State.t
   and type t = Base.t

(*
   TODO: Clojure agents have it right: they can register messages to be sent
   (equivalent to arbitrary asynchronous actions being spawned, yet reified)
   but only if and when the current transaction succeeds.
   The messages to be sent can "simply" be part of the state,
   and the sender part of the normal state processor,
   with a state update happening to remove them once they have been safely received.
   We want the same, in a persistent setting.
   And of course all messages should be idempotent,
   since they will be re-sent in case of crash.
   In practice, it should be enough to call Db.commit () then wait for the result,
   but only do that in the spawned thread --- and return immediately with the saved state.
   Note that to implement ('i, 'o) actions with persistent actor state,
   the receiver for the 'o message must themselves be persistent;
   regular anomymous functions won't do, they must be named functions,
   that only close over marshalable data that is part of the name.
   There probably needs be a monadic type for such functions.
*)

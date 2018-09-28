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


val persistent_actor_no_default_state : string -> ('key -> string) -> _ -> 'key -> _

(** PersistentActor

    An elaboration of [Action.SimpleActor] whereby the state is persisted to the database.

    Note that the [action] function will return _before_ the state is saved. That's OK if the caller
    (1) uses the result as part of a [Db.with_transaction], and (2) waits for [Db.commit] before
    to communicate with any external system based on this result.

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
module type PersistentActorBaseS = sig
  module Key : YojsonMarshalableS
  (** Marshalable key used to locate the actor state in the DB
      For global objects, the key is unit, the system better load the object at restart.
      For other objects, the key must contain a revision number that must be incremented,
      an identifying tuple or a digest thereof, or a random nonce that must be generated.
      The key must itself be somehow registered and persisted in an object managing the wider scope
      with a narrower key, as part of the same transaction that creates the object.
  *)

  val key_prefix : string
  (** a prefix to the key *)

  type context
  (** Inherited context for the actor: beyond the key, data and activities from the surrounding scope. *)

  module State : PersistableS
  (** State of the actor, to be stored in the DB *)

  type activity
  (** The in-image activity, as visible in the application: promises, mailboxes, refs, etc. *)

  val make_default_state : context -> Key.t -> State.t
  (** create a default state if none is present in the DB.
      May call [persistent_actor_no_default_state key_prefix Key.to_yojson_string]
      if there is no meaningful such default state. *)

  val make_activity : context -> Key.t -> State.t SimpleActor.t -> activity
  (** given an initial state, make the activity *)

  val behavior : context -> Key.t -> activity -> (State.t SimpleActor.t, unit) Lwter.arr
  (** behavior to run in the background to interact with the actor (???) *)

  val is_synchronous : bool
  (** If [is_synchronous], then state is committed at every state transition
      as due to call to [modify] or [action], before the next state transition may take place.
      The background [behavior] is called once when the actor is instantiated,
      and has to handle the entire actor's lifecycle.
      Thus there can be only one state transition per database commit batch,
      so about 60 per second on a typical 2018 PC.

      If not [is_synchronous], then multiple state transitions may be batched,
      and the [behavior] is called once and only once after every batch commit.

      TODO: have a more uniform [behavior], or have two distinct functors.
  *)
end

(** See documentation above for [PersistentActorBaseS] and for [Action.SimpleActor] *)
module type PersistentActorS = sig
  type key
  type context
  type state
  type activity
  type t
  val make : context -> key -> state -> t Lwt.t
  val get : context -> key -> t
  val peek : t -> state
  val peek_action : t -> ('i, 'o, state) async_action -> ('i, 'o) Lwter.arr (* TODO: use async_reader instead *)
  val modify : t -> (state -> state Lwt.t) -> unit Lwt.t
  val action : t -> ('i, 'o, state) async_action -> ('i, 'o) Lwter.arr
  val activity : t -> activity
end

module PersistentActor (Base: PersistentActorBaseS) :
  PersistentActorS
  with type key = Base.Key.t
   and type context = Base.context
   and type state = Base.State.t
   and type activity = Base.activity

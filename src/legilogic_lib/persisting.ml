(** Persisting Data *)
open Lib
open Action
open Lwter
open Yojsoning
open Marshaling
open Digesting

(** Walking across the dependencies of an object *)
type 'a dependency_walking_methods =
  { digest: 'a -> digest
  ; marshal_string: 'a -> string
  ; make_persistent: ('a -> unit Lwt.t) -> 'a -> unit Lwt.t
  ; walk_dependencies: 'a dependency_walker }
and dependency_walking_context =
  { walk: 'a. 'a dependency_walker }
and 'a dependency_walker = 'a dependency_walking_methods -> dependency_walking_context -> 'a -> unit Lwt.t

let normal_persistent f x = f x

let already_persistent _fun _x = Lwt.return_unit

let no_dependencies _methods _context _x = Lwt.return_unit

let walk_dependency methods context x =
  context.walk methods context x

let one_dependency f methods _methods context x =
  walk_dependency methods context (f x)

let seq_dependencies dep1 dep2 methods context x =
  dep1 methods context x >>= (fun () -> dep2 methods context x)

let dependency_walking_not_implemented =
  { digest= bottom; marshal_string= bottom; make_persistent= bottom; walk_dependencies= bottom }

let content_addressed_storage_prefix = "K256"

let content_addressed_storage_key digest =
  content_addressed_storage_prefix ^ Digest.to_big_endian_bits digest

let db_string_of_digest digest =
  digest |> content_addressed_storage_key |> Db.get |> Option.get

let db_value_of_digest unmarshal_string digest =
  digest |> db_string_of_digest |> unmarshal_string

(** TODO: have a version that computes the digest from the marshal_string *)
(** Have both content- and intent- addressed storage in the same framework *)
let saving_walker methods context x =
  methods.make_persistent
    (fun x ->
       let key = x |> methods.digest |> content_addressed_storage_key in
       if Db.has_key key then
         Lwt.return_unit
       else
         (methods.walk_dependencies methods context x >>=
          (fun () -> Db.put key (methods.marshal_string x))))
    x

let saving_context = { walk = saving_walker }

let save_of_dependency_walking methods x = walk_dependency methods saving_context x

module type PrePersistableDependencyS = sig
  type t
  val make_persistent: (t -> unit Lwt.t) -> t -> unit Lwt.t
  val walk_dependencies : t dependency_walker
end

module type PrePersistableS = sig
  include PreYojsonMarshalableS
  include PrePersistableDependencyS with type t := t
end

module type PersistableS = sig
  include PrePersistableS
  include MarshalableS with type t := t
  include DigestibleS with type t := t
  include YojsonableS with type t := t
  val dependency_walking : t dependency_walking_methods
  val save : t -> unit Lwt.t
end

module Persistable (P : PrePersistableS) = struct
  include P
  include (Yojsonable(P) : YojsonableS with type t := t)
  include (DigestibleOfPreMarshalable(P) : DigestibleS with type t := t)
  let dependency_walking = { digest; marshal_string; walk_dependencies; make_persistent }
  let save = save_of_dependency_walking dependency_walking
end

module TrivialPersistable (P : PreYojsonMarshalableS) =
  Persistable(struct
    include P
    let make_persistent = normal_persistent
    let walk_dependencies = no_dependencies
  end)

module OCamlPersistable (T: TypeS) =
  TrivialPersistable(struct
    module M = Marshalable(OCamlMarshaling (T))
    include M
    include (YojsonableOfMarshalable(M) : YojsonableS with type t := t)
  end)

module YojsonPersistable (J: PreYojsonableS) = struct
  module MJ = struct
    include MarshalableOfYojsonable (Yojsonable(J))
    let make_persistent = normal_persistent
    let walk_dependencies = no_dependencies
  end
  include Persistable(MJ)
  let marshal_string = MJ.marshal_string
end

let persistent_actor_no_default_state key_prefix to_yojson_string _context key =
  Lib.bork "Failed to load key %s %s: Not_found" key_prefix (to_yojson_string key)

module type PersistentActorBaseS = sig
  module Key : YojsonMarshalableS
  val key_prefix : string
  type context
  module State : PersistableS
  type activity
  val make_default_state : context -> Key.t -> State.t
  val make_activity : context -> Key.t -> State.t SimpleActor.t -> activity
  val behavior : context -> Key.t -> activity -> (State.t SimpleActor.t, unit) Lwter.arr
  val is_synchronous : bool
end

module PersistentActor (Base: PersistentActorBaseS) = struct
  include Base
  type key = Key.t
  type state = State.t
  type t = state SimpleActor.t * activity
  open Lwter
  let table = Hashtbl.create 8
  let db_key key = key_prefix ^ (Key.marshal_string key)
  let save key state =
    State.walk_dependencies State.dependency_walking saving_context state
    >>= fun () ->
    State.marshal_string state |> Db.put (db_key key)
    >>= Db.commit
  let resume context key state =
    match Hashtbl.find_opt table key with
    | Some _ -> Lib.bork "object with key ~s ~s already resumed!" key_prefix (Key.to_yojson_string key)
    | None ->
      if is_synchronous then
        (let actor = SimpleActor.make ~save:(save key >>> Db.commit) state in
         let activity = make_activity context key actor in
         Hashtbl.replace table key (actor, activity);
         Lwt.async (fun () -> behavior context key activity actor);
         (actor, activity))
      else
        (let hook_ref = ref const_unit in
         let spawn_commit_hook () = Lwt.async !hook_ref in
         let actor = SimpleActor.make ~save:(save key >>> arr spawn_commit_hook) state in
         let activity = make_activity context key actor in
         let hook () = behavior context key activity actor in
         hook_ref := hook;
         Hashtbl.replace table key (actor, activity);
         spawn_commit_hook ();
         (actor, activity))
  let make context key initial_state =
    match Db.get (db_key key) with
    | Some _ -> Lib.bork "object with key %s %s already created!" key_prefix (Key.to_yojson_string key)
    | None ->
      save key initial_state
      >>= fun () ->
      resume context key initial_state
      |> return
  let get context key =
    let db_key = db_key key in
    match Hashtbl.find_opt table key with
    | Some x -> x
    | None -> (match Db.get db_key with
      | Some s -> (try State.unmarshal_string s with
          e -> Lib.bork "Failed to load %s %s: corrupted database content %s, %s"
                 key_prefix (Key.to_yojson_string key) (Hex.unparse_0x_data s) (Printexc.to_string e))
      | None -> make_default_state context key) |> resume context key
  let peek x = fst x |> SimpleActor.peek
  let peek_action x a = SimpleActor.peek_action (fst x) a
  let modify x a = SimpleActor.modify (fst x) a
  let action x a = SimpleActor.action (fst x) a
  let activity x = snd x
end

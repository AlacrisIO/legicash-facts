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

let no_dependencies _methods _context _x =
  Lwt.return_unit

let walk_dependency methods context x =
  context.walk methods context x

let one_dependency f methods _methods context x =
  walk_dependency methods context (f x)

(*
let seq_dependencies dep1 dep2 methods context x =
  dep1 methods context x >>= (fun () -> dep2 methods context x)
 *)

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
         (fun () ->
           Db.put key (methods.marshal_string x)
           >>= fun () ->
           Lwt.return_unit
    )))
    x

let saving_context = { walk = saving_walker }

let save_of_dependency_walking methods x =
  walk_dependency methods saving_context x

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

module Persistable (P : PrePersistableS) = struct
  include P
  include (Yojsonable(P) : YojsonableS with type t := t)
  include (DigestibleOfPreMarshalable(P) : DigestibleS with type t := t)
  let dependency_walking = { digest; marshal_string; walk_dependencies; make_persistent }
  let save = save_of_dependency_walking dependency_walking
end

module PrePersistableOfRlp (P : PreRlpYojsonablePersistableDependencyS) = struct
  include P
  let marshaling = marshaling_of_rlping rlping
end

module PersistableOfRlp (P : PreRlpYojsonablePersistableDependencyS) = struct
  include P
  include (Persistable(PrePersistableOfRlp(P)) : PersistableS with type t := t)
end

module PersistableRlp (P : PrePersistableRlpS) = struct
  include P
  include (Persistable(P) : PersistableS with type t := t)
end

module TrivialPersistable (P : PreYojsonMarshalableS) =
  Persistable(struct
    include P
    let make_persistent = normal_persistent
    let walk_dependencies = no_dependencies
  end)

module OCamlPersistable (T: TypeS) =
  TrivialPersistable(struct
    module M = MarshalableRlp(OCamlMarshaling (T))
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

module type PersistentActivityBaseS = sig
  type context
  module Key : YojsonMarshalableS
  val key_prefix : string
  module State : PersistableS
  val make_default_state : context -> Key.t -> State.t
  type t
  val make_activity : context -> Key.t -> (State.t, State.t) Lwter.arr -> State.t -> t
end
module type PersistentActivityS = sig
  type context
  type key
  type state
  type t
  val make : context -> key -> ((state, state) Lwter.arr -> state Lwt.t) -> t Lwt.t
  val get : context -> key -> t
end
module PersistentActivity (Base: PersistentActivityBaseS) = struct
  include Base
  type key = Key.t
  type state = State.t
  type activity = t
  let table = Hashtbl.create 8 (* TODO: make it a weak reference table with Weak.create *)
  let db_key key = key_prefix ^ (Key.marshal_string key)
  let saving (db_key : string) (state : state) : state Lwt.t =
    State.walk_dependencies State.dependency_walking saving_context state >>= fun () ->
    State.marshal_string state |> Db.put db_key >>=
      const state
  let resume (context: context) (key : key) (current_state: state) : activity =
    (*Logging.log "RESUME prefix %s key %s state %s" key_prefix (Key.to_yojson_string key) (State.to_yojson_string initial_state);*)
    let activity = make_activity context key (saving (db_key key)) current_state in
    (if Hashtbl.mem table key then
       Lib.bork "object with key ~s ~s already resumed!" key_prefix (Key.to_yojson_string key));
    Hashtbl.replace table key activity;
    activity
  let make context key init =
    let db_key = db_key key in
    match Db.get db_key with
    | Some _ -> Lib.bork "object with key %s %s already created!" key_prefix (Key.to_yojson_string key)
    | None -> init (saving db_key) >>= fun state -> return (resume context key state)
  let get context key =
    (*    Logging.log "GET prefix %s key %s" key_prefix (Key.to_yojson_string key);*)
    let db_key = db_key key in
    (match Hashtbl.find_opt table key with
     | Some x -> x
     | None ->
        let state =
         match Db.get db_key with
         | Some s -> (try State.unmarshal_string s with
             e -> Lib.bork "Failed to load %s %s: corrupted database content %s, %s"
                    key_prefix (Key.to_yojson_string key) (Hex.unparse_0x_data s) (Printexc.to_string e))
         | None -> make_default_state context key in
       resume context key state)
    (*|> fun obj -> Logging.log "GOT prefix %s key %s obj %d" key_prefix (Key.to_yojson_string key) (0 + Obj.magic obj); obj*)
end

module type CachedValueBaseS = sig
  module Key : PersistableS
  module Value : PersistableS
  val key_prefix : string
  val make : Key.t -> Value.t Lwt_exn.t
end
module type CachedValueS = sig
  module Key : PersistableS
  module Value : PersistableS
  val get : Key.t -> Value.t Lwt_exn.t
end
module CachedValue (Base: CachedValueBaseS) = struct
  include Base
  let get key =
    let open Lwt_exn in
    let db_key = key_prefix ^ Key.marshal_string key in
    match Db.get db_key with
    | Some v -> return (Value.unmarshal_string v)
    | None -> trying make key >>= function
                | Ok v -> of_lwt (Db.put db_key) (Value.marshal_string v) >>= fun _ ->
                          of_lwt Db.committing v
                | Error e -> fail e
end

(** Database access

    Note that we assume that there are global objects used in a single-threaded way.
    Otherwise, we would want thread-local objects, dynamically bound objects, or lexical objects,
    instead of global objects, to communicate with the database, and/or we would want locks around access.
*)
open Lib
open Marshaling
open Integer
open Crypto

type db = LevelDB.db

type transaction = LevelDB.writebatch

let db_name = "legicash_state"

let the_global ?(ref=ref None) make_foo =
  fun () ->
    if !ref == None then
      ref := Some (make_foo ());
    option_get !ref

let the_db : unit -> db =
  the_global (fun () -> LevelDB.open_db db_name)

let the_db_batching_channel : unit -> (transaction * unit Event.channel) Event.channel =
  the_global Event.new_channel

let run_db_batching_thread () =
  let db = the_db () in
  let channel = the_db_batching_channel () in
  while true do
    let (transaction, continuation) = Event.sync (Event.receive channel) in
    LevelDB.Batch.write db ~sync:true transaction ;
    ignore (Event.send continuation ()) ;
  done

let the_db_batching_thread =
  the_global (Thread.create run_db_batching_thread)

let the_db_transaction_ref : (transaction option ref) =
  ref None

let the_db_transaction =
  the_global ~ref:the_db_transaction_ref LevelDB.Batch.make

let post_db_transaction () =
  let channel = the_db_batching_channel () in
  let _ = the_db_batching_thread () in
  let transaction = the_db_transaction () in
  let continuation = Event.new_channel () in
  the_db_transaction_ref := None ;
  ignore (Event.send channel (transaction, continuation)) ;
  continuation

let has_db_key key =
  LevelDB.mem (the_db ()) key

let get_db key =
  LevelDB.get (the_db ()) key

let put_db key data =
  let transaction = the_db_transaction () in
  LevelDB.Batch.put transaction key data

let remove_db key =
  let transaction = the_db_transaction () in
  LevelDB.Batch.delete transaction key


(** Walking across the dependencies of an object *)
type 'a dependency_walking_methods =
  { digest: 'a -> digest
  ; marshal_string: 'a -> string
  ; make_persistent: ('a -> unit) -> 'a -> unit
  ; walk_dependencies: 'a dependency_walker }
and dependency_walking_context =
  { walk: 'a. 'a dependency_walker }
and 'a dependency_walker = 'a dependency_walking_methods -> dependency_walking_context -> 'a -> unit

let normal_persistent f x = f x

let already_persistent _fun _x = ()

let no_dependencies _methods _context _x = ()

let walk_dependency methods context x =
  context.walk methods context x

let one_dependency f methods _methods context x =
  walk_dependency methods context (f x)

let seq_dependencies dep1 dep2 methods context x = dep1 methods context x ; dep2 methods context x

let dependency_walking_not_implemented =
  { digest= bottom; marshal_string= bottom; make_persistent= bottom; walk_dependencies= bottom }

let content_addressed_storage_prefix = "K256"

let content_addressed_storage_key digest =
  content_addressed_storage_prefix ^ Digest.to_big_endian_bits digest

let db_value_of_digest unmarshal_string digest =
  digest |> content_addressed_storage_key |> get_db |> option_get |> unmarshal_string

(** TODO: have a version that computes the digest from the marshal_string *)
let saving_walker methods context x =
  methods.make_persistent
    (fun _ ->
       let key = x |> methods.digest |> content_addressed_storage_key in
       if not (has_db_key key) then
         (methods.walk_dependencies methods context x;
          put_db key (methods.marshal_string x)))
    x

let saving_context = { walk = saving_walker }

let save_of_dependency_walking methods x = walk_dependency methods saving_context x

module type PrePersistableDependencyS = sig
  type t
  val make_persistent: (t -> unit) -> t -> unit
  val walk_dependencies : t dependency_walker
end

module type PrePersistableS = sig
  include PreMarshalableS
  include PrePersistableDependencyS with type t := t
end

module type PersistableS = sig
  include PrePersistableS
  include MarshalableS with type t := t
  include DigestibleS with type t := t
  val dependency_walking : t dependency_walking_methods
  val save : t -> unit
end

module Persistable (P : PrePersistableS) = struct
  include P
  module M = Marshalable(P)
  include (M : MarshalableS with type t := t)
  include (DigestibleOfMarshalable(M) : DigestibleS with type t := t)
  let dependency_walking = { digest; marshal_string; walk_dependencies; make_persistent }
  let save = save_of_dependency_walking dependency_walking
end

module TrivialPersistable (P : PreMarshalableS) =
  Persistable(struct
    include P
    let make_persistent = normal_persistent
    let walk_dependencies = no_dependencies
  end)

module OCamlPersistable (Type: T) =
  Persistable(struct
    include OCamlMarshaling (Type)
    let make_persistent = normal_persistent
    let walk_dependencies = no_dependencies
  end)

module type IntS = sig
  include Integer.IntS
  include PersistableS with type t := t
end

module DBInt(I : Integer.IntS) = struct
  include I
  include (Persistable (struct
             include I
             include (TrivialPersistable (I) : PrePersistableDependencyS with type t := t)
           end) : PersistableS with type t := t)
end

module UInt16 = DBInt(Integer.UInt16)
module UInt32 = DBInt(Integer.UInt32)
module UInt64 = DBInt(Integer.UInt64)
module Data256 = DBInt(Integer.Data256)
module UInt256 = DBInt(Integer.UInt256)
module Digest = DBInt(Crypto.Digest)
module Address = struct
  include Crypto.Address
  include (DBInt(Crypto.Address) : PersistableS with type t := t)
end
module Signature = Persistable(struct
    include Crypto.Signature
    include (TrivialPersistable (Crypto.Signature) : PrePersistableDependencyS with type t := t)
  end)

module Revision = UInt64

module Duration = UInt64

module Timestamp = UInt64


(** TODO: mechanism to forget old values? Or is GC enough? *)
type +'a dv = {digest: Digest.t Lazy.t; value: 'a Lazy.t; mutable persisted: bool}
let dv_get dv = Lazy.force dv.value
let dv_digest dv = Lazy.force dv.digest
let dv_make digest value =
  { digest=lazy (digest value)
  ; value=lazy value
  ; persisted=false }
let dv_of_digest unmarshal_string digest =
  { digest=lazy digest
  ; value=lazy (db_value_of_digest unmarshal_string digest)
  ; persisted=true }
let dv_marshal buffer x =
  marshal_map dv_digest Digest.marshal buffer x
let dv_unmarshal unmarshal_string =
  unmarshal_map (dv_of_digest unmarshal_string) Digest.unmarshal
let dv_marshaling value_unmarshal_string =
  { marshal= dv_marshal
  ; unmarshal= dv_unmarshal value_unmarshal_string }

module type DigestValueBaseS = sig
  include WrapS
  type digest
  val of_digest : digest -> t
  val digest : t -> digest
end

module type DigestValueS = sig
  type value
  include DigestValueBaseS
    with type value := value
     and type t = value dv
     and type digest = Digest.t
  include PersistableS with type t := t
end

module DigestValueType = struct
  type +'a t = 'a dv
end

module DigestValue (Value : PersistableS) = struct
  type value = Value.t
  type digest = Digest.t
  let get = dv_get
  let make = dv_make Value.digest
  let of_digest = dv_of_digest Value.unmarshal_string
  include Persistable(struct
      type t = value dv
      let marshaling = { marshal= marshal_map dv_digest Digest.marshal
                       ; unmarshal= unmarshal_map of_digest Digest.unmarshal }
      let walk_dependencies _methods context x =
        walk_dependency Value.dependency_walking context (dv_get x)
      let make_persistent f dv = if not dv.persisted then (dv.persisted <- true; f dv)
    end)
end

module StringT = struct
  include String
  module PrePersistable = struct
    type t = string
    let marshaling =
      { marshal = (fun buffer x ->
          let len = Nat.of_int (String.length x) in
          assert (Nat.compare len (Nat.shift_left Nat.one 32) < 0);
          Buffer.add_string buffer (big_endian_bits_of_nat 32 len);
          Buffer.add_string buffer x)
      ; unmarshal = (fun ?start:(start=0) b ->
          let (l, p) = unmarshal_map UInt32.to_int UInt32.unmarshal ~start b in
          assert (l >= 0);
          Bytes.sub_string b p l, p + l) }
    let make_persistent = normal_persistent
    let walk_dependencies = no_dependencies
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
end

module Unit = struct
  type t = unit
  module PrePersistable = struct
    type t = unit
    let marshaling = { marshal = (fun _buffer () -> ())
                     ; unmarshal = (fun ?(start=0) _bytes -> ((), start)) }
    let make_persistent = already_persistent
    let walk_dependencies = no_dependencies
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
  let pp formatter _ = Format.fprintf formatter "%s" "()"
  let show x = Format.asprintf "%a" pp x
end

module UInt16int = Marshalable(struct
    type t = int
    let marshaling = marshaling_map UInt16.of_int UInt16.to_int UInt16.marshaling
  end)


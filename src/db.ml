(** Database access

    Note that we assume that there are global objects used in a single-threaded way.
    Otherwise, we would want thread-local objects, dynamically bound objects, or lexical objects,
    instead of global objects, to communicate with the database, and/or we would want locks around access.

    WARNING: Ugly pure-functional-ish interface!

    Our interface assumes that database reads of content-addressed data is "not a side-effect"
    and can be written in direct style, whereas database writes are side-effects that require monadic
    style, except that flagging an object as persisted for walk optimization can be done directly.
    A conceptual mess? Yes it is! All because OCaml's type system can't deal with this kind of abstraction.
*)
open Lib
open Marshaling
open Integer
open Crypto
open Lwt.Infix

type db = LevelDB.db
type transaction = LevelDB.writebatch

(* type snapshot = LevelDB.snapshot *)

(* Move that to lib ? *)
let the_global ref maker =
  fun () ->
    match !ref with
    | Some x -> x
    | None ->
      let x = maker () in
      ref := Some x;
      x

type db_request =
  | Put of {key: string; data: string}
  | Remove of string
  | Commit of unit Lwt.u
  | Flush of int

type connection =
  { db_name : string
  ; db : db
  ; mailbox : db_request Lwt_mvar.t }

let the_connection_ref : (connection option ref) = ref None

(* timeout for batching, in seconds *)
let transaction_timeout_trigger = 0.01

(* overflow size for batching, in seconds *)
let transaction_size_trigger = 4*1024*1024

let start_server ~db_name ~db ~mailbox () =
  Lwt_io.printf "Opening LevelDB connection to db %s\n%!" db_name >>=
  (let rec outer_loop count previous () =
     let transaction = LevelDB.Batch.make () in
     let (size_trigger, trigger_size) = Lwt.task () in
     let timeout = Lwt_unix.sleep transaction_timeout_trigger in
     Lwt.async (fun () -> Lwt.join [previous; Lwt.pick [timeout; size_trigger]]
                 >>= (fun () -> Lwt_mvar.put mailbox (Flush count)));
     let rec inner_loop triggered size tinuations =
       if (not triggered) && (size >= transaction_size_trigger) then
         begin
           Lwt.wakeup_later trigger_size ();
           inner_loop true size tinuations
         end
       else
         Lwt_mvar.take mailbox
         >>= function
         | Put {key;data} ->
           LevelDB.Batch.put transaction key data;
           (* TODO: check sizes for overflow *)
           inner_loop triggered (size + String.length key + String.length data) tinuations
         | Remove key ->
           LevelDB.Batch.delete transaction key;
           (* TODO: check sizes for overflow *)
           inner_loop triggered (size + String.length key) tinuations
         | Commit tinuation ->
           inner_loop triggered size (tinuation::tinuations)
         | Flush n ->
           if n = count then
             outer_loop
               (count + 1)
               (Lwt_preemptive.detach
                  (fun () ->
                     if size != 0 then
                       LevelDB.Batch.write db ~sync:true transaction) ()
                >>= (fun () ->
                  List.iter (fun tinuation -> Lwt.wakeup_later tinuation ()) tinuations;
                  Lwt.return_unit))
               ()
           else
             (* ignore timeout for  *)
             inner_loop triggered size tinuations in
     inner_loop false 0 [] in
   outer_loop 0 Lwt.return_unit)

let open_connection db_name =
  match !the_connection_ref with
  | Some x ->
    bork (Printf.sprintf "Process already has a LevelDB connection to db %s, won't start %s"
            x.db_name (if x.db_name = db_name then "another one" else "one for db " ^ db_name))
  | None ->
    let db = LevelDB.open_db db_name in
    let mailbox = Lwt_mvar.create_empty () in
    the_connection_ref := Some { db_name ; db ; mailbox };
    Lwt.async (start_server ~db_name ~db ~mailbox);
    Lwt.return_unit

let the_connection =
  the_global the_connection_ref
    (fun () -> bork (Printf.sprintf "no db connection for pid %d\n%!" (Unix.getpid ())))

let the_db () = (the_connection ()).db

let request r =
  Lwt_mvar.put (the_connection ()).mailbox r

let run ~db_name thunk =
  Lwt_main.run (open_connection db_name >>= thunk)

let commit () =
  let (continuation, tinuation) = Lwt.task () in
  Lwt.async (fun () -> request (Commit tinuation));
  continuation

let has_db_key key =
  LevelDB.mem (the_db ()) key

let get_db key =
  LevelDB.get (the_db ()) key

let put_db key data =
  request (Put {key; data})

let remove_db key =
  request (Remove key)



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

let db_value_of_digest unmarshal_string digest =
  digest |> content_addressed_storage_key |> get_db |> option_get |> unmarshal_string

(** TODO: have a version that computes the digest from the marshal_string *)
let saving_walker methods context x =
  methods.make_persistent
    (fun x ->
       let key = x |> methods.digest |> content_addressed_storage_key in
       if has_db_key key then
         Lwt.return_unit
       else
         (methods.walk_dependencies methods context x >>=
          (fun () -> put_db key (methods.marshal_string x))))
    x

let saving_context = { walk = saving_walker }

let save_of_dependency_walking methods x = walk_dependency methods saving_context x

module type PrePersistableDependencyS = sig
  type t
  val make_persistent: (t -> unit Lwt.t) -> t -> unit Lwt.t
  val walk_dependencies : t dependency_walker
end

module type PreTrivialPersistableS = sig
  include PreMarshalableS
  include JsonableS with type t := t
end

module type PrePersistableS = sig
  include PreTrivialPersistableS
  include PrePersistableDependencyS with type t := t
end

module type PersistableS = sig
  include PrePersistableS
  include MarshalableS with type t := t
  include DigestibleS with type t := t
  val dependency_walking : t dependency_walking_methods
  val save : t -> unit Lwt.t
  val to_json_string : t -> string
  val of_json_string : string -> t
end

module Persistable (P : PrePersistableS) = struct
  include P
  module M = Marshalable(P)
  include (M : MarshalableS with type t := t)
  include (DigestibleOfMarshalable(M) : DigestibleS with type t := t)
  let dependency_walking = { digest; marshal_string; walk_dependencies; make_persistent }
  let save = save_of_dependency_walking dependency_walking
  let to_json_string x = x |> to_json |> Yojson.Basic.to_string
  let of_json_string x = of_json (Yojson.Basic.from_string x)
end

module TrivialPersistable (P : PreTrivialPersistableS) =
  Persistable(struct
    include P
    let make_persistent = normal_persistent
    let walk_dependencies = no_dependencies
  end)

module OCamlPersistable (Type: T) =
  Persistable(struct
    include Marshalable(OCamlMarshaling (Type))
    let make_persistent = normal_persistent
    let walk_dependencies = no_dependencies
    let to_json x = `String (marshal_string x)
    let of_json = function
      | `String s -> (unmarshal_string s)
      | _ -> Yojson.json_error "bad ocaml json"
  end)

module JsonPersistable (Type: JsonableS) = struct
  module J = struct
    include MarshalableOfJsonable (Type)
    let make_persistent = normal_persistent
    let walk_dependencies = no_dependencies
  end
  include Persistable(J)
  let marshal_string = J.marshal_string
end

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
      let make_persistent f dv =
        if dv.persisted then Lwt.return_unit else (dv.persisted <- true; f dv)
      let to_json x = x |> get |> Value.to_json
      let of_json j = make (Value.of_json j)
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
    let to_json x = `String x
    let of_json = function `String x -> x | _ -> Yojson.json_error "not a json string"
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
    let to_json _x = `Null
    let of_json = function `Null -> () | _ -> Yojson.json_error "not a json null"
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
  let pp formatter _ = Format.fprintf formatter "%s" "()"
  let show x = Format.asprintf "%a" pp x
end

module UInt16int = struct
  module U = struct
    type t = int
    let verify x =
      if 0 <= x && x <= 0xFFFF then x else bork "bad UInt16int"
    let marshaling = marshaling_map UInt16.of_int UInt16.to_int UInt16.marshaling
    let make_persistent = already_persistent
    let walk_dependencies = no_dependencies
    let to_json x = `Int x
    let of_json = function
      | `Int x -> verify x
      | _ -> Yojson.json_error "not a json null"
  end
  include Persistable(U)
end


open Lib
open Yojsoning
open Marshaling
open Integer
open Persisting

module type UIntS = sig
  include Integer.UIntS
  include PersistableS with type t := t
end

module DBInt(U : Integer.UIntS) = struct
  include U
  include (Persistable (struct
             include U
             include (TrivialPersistable (U) : PrePersistableDependencyS with type t := t)
           end) : PersistableS with type t := t)
end

module UInt16 = DBInt(Integer.UInt16)
module UInt32 = DBInt(Integer.UInt32)
module UInt64 = DBInt(Integer.UInt64)
module UInt128 = DBInt(Integer.UInt128)
module UInt256 = DBInt(Integer.UInt256)
module Data160 = DBInt(Integer.Data160)
module Data256 = DBInt(Integer.Data256)
module Digest = DBInt(Digesting.Digest)

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
  (* let equal x y = Digest.equal (dv_digest x) (dv_digest y) (* Assume no hash collision *) *)
  include Persistable(struct
      type t = value dv
      let marshaling = marshaling_map dv_digest of_digest Digest.marshaling
      let yojsoning = yojsoning_map get make Value.yojsoning
      let walk_dependencies _methods context x =
        walk_dependency Value.dependency_walking context (dv_get x)
      let make_persistent _f dv =
        if dv.persisted then
          Lwt.return_unit
        else
          (dv.persisted <- true;
           Value.save (dv_get dv))
    end)
end

module StringT = struct
  include String
  module PrePersistable = struct
    type t = string
    let marshaling =
      { marshal = (fun buffer x ->
          let len = Nat.of_int (String.length x) in
          assert Nat.(compare len (shift_left one 32) < 0);
          Buffer.add_string buffer (big_endian_bits_of_nat 32 len);
          Buffer.add_string buffer x)
      ; unmarshal = (fun start b ->
          let (l, p) = unmarshal_map UInt32.to_int UInt32.unmarshal start b in
          assert (l >= 0);
          Bytes.sub_string b p l, p + l) }
    let make_persistent = normal_persistent
    let walk_dependencies = no_dependencies
    include (Yojsonable(struct
               type nonrec t = t
               let yojsoning = string_yojsoning
             end) : YojsonableS with type t := t)
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
end

module Unit = struct
  type t = unit
  module PrePersistable = struct
    type t = unit
    let marshaling = { marshal = (fun _buffer () -> ())
                     ; unmarshal = (fun start _bytes -> ((), start)) }
    let make_persistent = already_persistent
    let walk_dependencies = no_dependencies
    let yojsoning =
      { to_yojson = konstant `Null
      ; of_yojson = function `Null -> Ok () | _ -> Error "not a json null" }
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
  let pp formatter _ = Format.fprintf formatter "%s" "()"
  let show x = Format.asprintf "%a" pp x
end

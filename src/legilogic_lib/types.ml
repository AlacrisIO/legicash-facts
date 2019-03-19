open Lib
open Yojsoning
open Marshaling
open Persisting
open Ppx_deriving_rlp_runtime
open Rlping

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

module UInt8  = DBInt(Integer.UInt8)
module UInt16  = DBInt(Integer.UInt16)
module UInt32  = DBInt(Integer.UInt32)
module UInt64  = DBInt(Integer.UInt64)
module UInt128 = DBInt(Integer.UInt128)
module UInt192 = DBInt(Integer.UInt192)
module UInt256 = DBInt(Integer.UInt256)
module Data160 = DBInt(Integer.Data160)
module Data256 = DBInt(Integer.Data256)
module Digest  = DBInt(Digesting.Digest)

module Revision  = UInt64
module Duration  = UInt64

module Timestamp = struct
  include UInt64

  (* TODO as of 2019-02-27:
   * - Increase `Timestamp` precision from milliseconds to microseconds
   *   https://gitlab.com/legicash/legicash-facts/issues/90
   *)

  let now = fun () -> 1000.0 *. Unix.gettimeofday ()
    |> Int64.of_float
    |> UInt64.of_int64
end

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

(* TODO: This `dv` RLP marshaling forces both the value and the
         digest, and includes them both in the RLP representation.
         Is this right? Or should it only include the digest,
         and use something like `dv_of_digest` in the unmarshaling? *)
let (dv_to_rlp_item, dv_of_rlp_item, dv_rlping) =
  let dv_to_rlp_item _ x =
    Digest.to_rlp_item (dv_digest x)
  and dv_of_rlp_item vof it =
    dv_of_digest (fun x -> vof (Rlp_decode.rlp_item_of_rlp x)) (Digest.of_rlp_item it) in
  let dv_rlping vrlp =
    rlping { to_rlp_item = dv_to_rlp_item vrlp.to_rlp_item;
             of_rlp_item = dv_of_rlp_item vrlp.of_rlp_item } in
  (dv_to_rlp_item, dv_of_rlp_item, dv_rlping)


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
  [@@deriving rlp]
end

(* TODO: somehow replace the strong reference to the value by a weak reference once it's been persisted
   (and not merely scheduled for persistence as part of a transaction), so it may be garbage-collected. *)
module DigestValue (Value : PersistableRlpS) = struct
  type value = Value.t
  [@@deriving rlp]
  type digest = Digest.t
  type t = value dv
  [@@deriving rlp]
  let get = dv_get
  let make = dv_make Value.digest
  let of_digest = dv_of_digest Value.unmarshal_string
  (* let equal x y = Digest.equal (dv_digest x) (dv_digest y) (* Assume no hash collision *) *)
  module P = struct
      type nonrec t = t
      let marshaling = marshaling_of_rlping rlping
      let yojsoning = yojsoning_map get make Value.yojsoning
      let walk_dependencies _methods context x =
        walk_dependency Value.dependency_walking context (dv_get x)
      let make_persistent _f dv =
        if dv.persisted then
          Lwt.return_unit
        else
          (dv.persisted <- true;
           Value.save (dv_get dv))
  end
  include (Persistable(P) : PersistableS with type t := t)
end

module StringT = struct
  type t = string
  [@@deriving rlp]
  include (String : module type of String with type t := t)
  include (TrivialPersistable (String1G) : PersistableS with type t := t)
end

module Data = struct
  include Data
  include (TrivialPersistable (Data) : PersistableS with type t := t)
end

module Unit = struct
  type t = unit
  [@@deriving rlp]
  module PrePersistable = struct
    type t = unit
    let marshaling = marshaling_of_rlping rlping
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

module Exception = struct
  module P = struct
    type t = exn
    let to_yojson = function
      | e -> `String (Printexc.to_string e)
    let of_yojson = function
      | `String x -> Ok (Internal_error x)
      | _ -> Error "Not an error"
    let yojsoning = {to_yojson;of_yojson}
  end
  include YojsonPersistable(P)
  let pp formatter x = Format.fprintf formatter "%s" (Printexc.to_string x)
  let show x = Format.asprintf "%a" pp x
end

module RequestGuid = struct
  (* Request GUIDs follow the UUIDv4 spec and look like this:
   * fc0f69bb-5482-43ca-9f0b-f005f0bedc4c
   * 9f996f2a-d7ee-4104-84e2-b0e4576d16be
   * 086a3f80-9706-4d89-b31b-8507112e7188
   * 3b5ddd6c-c2e0-4c71-a7d0-486964c450ef
   * 5e49458d-b1b4-49f5-8069-b33943201e83
   * 66d8b845-07ec-40a8-8f6d-93fa9ce293d2
   *
   * See: https://en.wikipedia.org/wiki/Universally_unique_identifier#Version_4_(random)
   * and: https://gitlab.com/legicash/legicash-demo-frontend/blob/6161bdd9/src/types/guid.tsx
   *
   * ---
   * TODO as of 2019-02-27:
   *
   * - Replace 512bit `RequestGuid` encoding with 128bit
   *   https://gitlab.com/legicash/legicash-facts/issues/88
   *
   * - Improve `RequestGuid` construction + validation
   *   https://gitlab.com/legicash/legicash-facts/issues/84
   *
   * - DB read to prevent `RequestGuid` collisions
   *   https://gitlab.com/legicash/legicash-facts/issues/83
   *
   * - Identify and leverage an existing OCaml UUID library that's solid or roll our own
   *   https://gitlab.com/legicash/legicash-facts/issues/89
   *
   * - Replace `from_string_result` with more idiomatic `(t, string) result`
   *   https://gitlab.com/legicash/legicash-facts/issues/91
   *
   * - Drop ad-hoc left padding when we move to 128bits
   *)

  type t = UInt128.t * UInt64.t * UInt64.t * UInt64.t * UInt192.t
  [@@deriving rlp]

  type from_string_result =
    | WellFormed of t
    | Malformed  of string

  let to_string (a, b, c, d, e) =
    let lpad n s =
      (String.make (n - (String.length s)) '0') ^ s

    in String.concat "-" [ lpad  8 @@ UInt128.to_hex_string a
                         ; lpad  4 @@  UInt64.to_hex_string b
                         ; lpad  4 @@  UInt64.to_hex_string c
                         ; lpad  4 @@  UInt64.to_hex_string d
                         ; lpad 12 @@ UInt192.to_hex_string e ]

  let from_string s =
    let make ss =
      try WellFormed ( UInt128.of_hex_string @@ List.nth ss 0
                     , UInt64.of_hex_string  @@ List.nth ss 1
                     , UInt64.of_hex_string  @@ List.nth ss 2
                     , UInt64.of_hex_string  @@ List.nth ss 3
                     , UInt192.of_hex_string @@ List.nth ss 4 )
      with Internal_error e -> Malformed e

    in match String.split_on_char '-' s with
      | ss when List.length ss <> 5 -> Malformed s
      | ss                          -> make ss

  let nil =
    (* Note that valid `UUIDv4`s will never result in this value since the 15th
     * character should always be "4". `nil` is useful for cases such as
     * testing or when composing "throwaway" records which must satisfy the
     * type system but serve no other purpose, e.g. the ignored record produced
     * in `Side_chain_user.User.resume_transactions`
     *)
    match from_string "00000000-0000-0000-0000-000000000000" with
        | WellFormed s -> s
        | _            -> raise (Internal_error "Invalid `nil` UUID")

  module P = struct
    type nonrec t = t

    let to_yojson = function
      | g -> `String (to_string g)

    let iso s =
      match from_string s with
        | WellFormed g -> Ok g
        | _            -> Error ("Malformed GUID: " ^ s)

    let of_yojson = function
      | `String s -> iso s
      | _         -> Error "Malformed GUID"

    let yojsoning = {to_yojson; of_yojson}
  end
  include (YojsonPersistable(P) : PersistableS with type t := t)

  module Test = struct
    let%test "RequestGuid to/from_string survives round-trip" =
      let good_guid = "fc0f69bb-0482-43ca-9f0b-0f05f0bedc4c"
      in match from_string good_guid with
        | WellFormed w -> to_string w = good_guid
        | _            -> false

    let%test "RequestGuid to/of_yojson survives round-trip" =
      let good_guid = "fc0f69bb-0482-43ca-9f0b-0f05f0bedc4c"
      in match of_yojson (`String good_guid) with
        | Ok g -> to_yojson g = `String good_guid
        | _    -> false

    let%test "RequestGuid.from_string <non-hexadecimal> yields Malformed" =
      match from_string "gggggggg-5482-43ca-9f0b-f005f0bedc4c" with
        | Malformed e -> e = "Invalid hex character g"
        | _           -> false

    let fail_for bad_guid =
      match from_string bad_guid with
        | Malformed e -> e = bad_guid
        | _           -> false

    let%test "RequestGuid.from_string <empty> yields Malformed" =
      fail_for ""

    let%test "RequestGuid.from_string <too-few-stanzas> yields Malformed" =
      fail_for "3b5ddd6c-c2e0-4c71-486964c450ef"

    let%test "RequestGuid.from_string <extra-dashes> yields Malformed" =
      fail_for "-3b5ddd6c--e02f-c2e0-4c71-486964c450ef"

    (* TODO FIXME *)
    (*
    let%test "RequestGuid.from_string <wrong-stanza-order> yields Malformed" =
      let bad_guid = "07ec-40a8-8f6d-66d8b845-93fa9ce293d2"
      in match from_string bad_guid with
        | Malformed e -> Printf.printf "%s" e; e = bad_guid
        | _           -> false
    *)
  end
end

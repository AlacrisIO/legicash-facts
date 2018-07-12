open Lib
open Integer

let keccak256_string s =
  Cryptokit.hash_string (Cryptokit.Hash.keccak 256) s
(* NB: reusing the (keccak 256) object causes a segfault *)

(* create context just once, because expensive operation; assumes
   single instantiation of this module
*)
let secp256k1_ctx = Secp256k1.Context.create [Sign; Verify]

let marshall_of_sized_string_of num_bytes string_of b x =
  let s = string_of x in
  if String.length s != num_bytes then
    raise (Internal_error
             (Printf.sprintf "marshall_of_sized_string_of expected %d bytes but got string %S of length %d"
                num_bytes s (String.length s)));
  Buffer.add_string b s
let unmarshall_of_sized_of_string num_bytes of_string b start =
  let s = Bytes.sub_string b start num_bytes in
  (of_string s, start + num_bytes)
let marshall_bytes_of_marshall marshall x =
  let buffer = Buffer.create 256 in
  marshall buffer x ;
  Buffer.to_bytes buffer
let unmarshall_bytes_of_unmarshall unmarshall b =
  let (t, r) = unmarshall b 0 in
  assert (r = Bytes.length b) ;
  t
let digest_of_string s =
  nat_of_big_endian_bits 256 (keccak256_string s)
let digest_of_marshall_bytes marshall_bytes x =
  digest_of_string (Bytes.to_string (marshall_bytes x))

let marshall_string_of_any v = Marshal.to_string v [Marshal.Compat_32]

let marshall_any b v =
  Buffer.add_string b (marshall_string_of_any v)
[@@deprecated "Use marshall_any only as temporary stopgap for demos and testing."]

(* TODO: check bounds, after every operation, etc. *)
module UInt256 = struct
  include Integer.UInt256
  let of_big_endian_bits = nat_of_big_endian_bits 256
  let to_big_endian_bits = big_endian_bits_of_nat 256
  let marshall = marshall_of_sized_string_of 32 to_big_endian_bits
  let unmarshall = unmarshall_of_sized_of_string 32 of_big_endian_bits
  let marshall_bytes = marshall_bytes_of_marshall marshall
  let unmarshall_bytes = unmarshall_bytes_of_unmarshall unmarshall
  let digest = digest_of_marshall_bytes marshall_bytes
end

module Data256 = struct
  include UInt256
  let of_hex_string = sized_nat_of_hex_string 256
  let to_hex_string = hex_string_of_sized_nat 256
end

module Digest = struct
  include Data256
end

type 'a digest = Digest.t

(** Special magic digest for None. A bit ugly. *)
let null_digest = Digest.zero

module type DigestibleS = sig
  include MarshallableS
  val marshall_bytes: t -> Bytes.t
  val unmarshall_bytes: Bytes.t -> t
  val digest: t -> t digest
end

module DigestibleOfMarshallable (T : MarshallableS) = struct
  include T
  let marshall_bytes = marshall_bytes_of_marshall marshall
  let unmarshall_bytes = unmarshall_bytes_of_unmarshall unmarshall
  let digest = digest_of_marshall_bytes marshall_bytes
end

module type IntS = sig
  include Integer.IntS
  include DigestibleS with type t := t
end

module UInt64 = struct
  include Integer.UInt64
  let of_big_endian_bits b = of_z (nat_of_big_endian_bits 64 b)
  let to_big_endian_bits u = big_endian_bits_of_nat 64 (z_of u)
  let marshall = marshall_of_sized_string_of 8 to_big_endian_bits
  let unmarshall = unmarshall_of_sized_of_string 8 of_big_endian_bits
  let marshall_bytes = marshall_bytes_of_marshall marshall
  let unmarshall_bytes = unmarshall_bytes_of_unmarshall unmarshall
  let digest = digest_of_marshall_bytes marshall_bytes
end

module Revision = UInt64
module Duration = UInt64
module Timestamp = UInt64


module StringT = struct
  include String
  module Marshallable__ = struct
    type t = string
    let marshall x = Buffer.add_string x
    let unmarshall b start =
      let len = Bytes.length b - start in (Bytes.sub_string b start len, len)
  end
  include (DigestibleOfMarshallable (Marshallable__) : DigestibleS with type t := t)
end

module Address = struct
  (* an address identifies a party (user, facilitator)
     per Ethereum, use the last 20 bytes of the Keccak256 hash of the party's public key *)

  include UInt256

  let address_size = 20

  let of_hex_string = sized_nat_of_hex_string 160
  let to_hex_string = hex_string_of_sized_nat 160
  let of_big_endian_bits = nat_of_big_endian_bits 160
  let to_big_endian_bits = big_endian_bits_of_nat 160

  let of_public_key public_key =
    let open Bigarray in
    let public_keylen = 64 in
    let buffer = Secp256k1.Key.to_bytes ~compress:false secp256k1_ctx public_key in
    (* uncompressed public key has an extra byte at the beginning, which we remove:
       https://bitcoin.stackexchange.com/questions/57855/c-secp256k1-what-do-prefixes-0x06-and-0x07-in-an-uncompressed-public-key-signif
    *)
    let pubkey_string = String.init public_keylen (fun ndx -> Array1.get buffer (ndx + 1)) in
    let hash = keccak256_string pubkey_string in
    let hash_len = String.length hash in
    Nat.of_bits (String.init address_size (fun ndx -> hash.[hash_len - ndx - 1]))

  let compare address1 address2 = Pervasives.compare address1 address2

  let equal address1 address2 = compare address1 address2 = 0

  let pp formatter x = Format.fprintf formatter "0x%s" (to_hex_string x)
  let show x = Format.asprintf "%a" pp x
end

module Unit = struct
  type t = unit
  module Marshallable = struct
    type t = unit
    let marshall _ _ = ()
    let unmarshall _ start = ((), start)
  end
  include (DigestibleOfMarshallable (Marshallable) : DigestibleS with type t := t)
  let pp formatter _ = Format.fprintf formatter "%s" "()"
  let show x = Format.asprintf "%a" pp x
end

type signature = Secp256k1.Sign.recoverable Secp256k1.Sign.t

(* create context just once, because expensive operation; assumes
   single instantiation of this module
   suppress unused variable warning
*)
let [@warning "-32"] secp256k1_ctx = Secp256k1.Context.create [Sign; Verify]

(** unique identifier for all parties, that is, customers and facilitators *)
type public_key = Secp256k1.Key.public Secp256k1.Key.t

(** private key in public-key cryptography *)
type private_key = Secp256k1.Key.secret Secp256k1.Key.t

(*module Int256 : Int with type t = Z.t : sig
  end*)

(* convert OCaml string of suitable length (32 only?) to Secp256k1 msg format
   for strings representing hashes, the msg format is suitable for signing.
*)
let secp256k1_msg_of_string s =
  let open Bigarray in
  let sz = String.length s in
  let buffer = Array1.create char c_layout sz in
  let _ = for i = 0 to sz - 1 do Bigarray.Array1.set buffer i s.[i] done in
  match Secp256k1.Sign.msg_of_bytes buffer with
  | Some msg -> msg
  | None -> raise (Internal_error "Could not create SECP256K1.Sign.msg from string")

(* convert a Digest.t to a Secp256k1 msg representing the digest *)
let secp256k1_msg_of_digest digest =
  secp256k1_msg_of_string (Digest.to_big_endian_bits digest)

let string_of_signature signature =
  Cstruct.to_string (Cstruct.of_bigarray (Secp256k1.Sign.to_bytes secp256k1_ctx signature))

let [@warning "-32"] signature_of_string string =
  Secp256k1.Sign.read secp256k1_ctx (Cstruct.of_string string).buffer

module Signature = struct
  type t = signature
  let marshall = marshall_of_sized_string_of 64 string_of_signature
  let unmarshall = bottom (* unmarshall_of_sized_of_string 64 signature_of_string *)
end


type 'a signed = {payload: 'a; signature: signature}
let marshall_signed marshall b {payload; signature} =
  marshall b payload; Signature.marshall b signature

(* digital signature is encrypted hash *)
let make_signature make_digest private_key data =
  (* change representation of data to use Secp256k1 signing *)
  let secp256k1_msg = secp256k1_msg_of_digest (make_digest data) in
  match Secp256k1.Sign.sign_recoverable secp256k1_ctx ~sk:private_key secp256k1_msg with
  | Ok signature -> signature
  | Error s -> raise (Internal_error s)

let address_matches_public_key address public_key =
  Address.equal address (Address.of_public_key public_key)

(* check validity of signature for data *)
let is_signature_valid make_digest (address: Address.t) (signature: Signature.t) data =
  let msg = secp256k1_msg_of_digest (make_digest data) in
  match Secp256k1.Sign.recover secp256k1_ctx ~msg ~signature with
  | Ok public_key -> address_matches_public_key address public_key
  | Error _ -> false

(* check validity of signature for payload within signed value *)
let is_signed_value_valid make_digest address signed_value =
  is_signature_valid make_digest address signed_value.signature signed_value.payload

let sign make_digest private_key data =
  {payload= data; signature= make_signature make_digest private_key data}

module Test = struct
  (* test digests *)
  let mk_digest_test data expected =
    let digest = digest_of_string data in
    expected = Digest.to_hex_string digest

  let%test "digest_1" =
    mk_digest_test "this is a test"
      "9fd09c38c2a5ae0a0bcd617872b735e37909ccc05c956460be7d3d03d881a0dc"

  let%test "digest_2" =
    mk_digest_test (UInt64.to_big_endian_bits UInt64.one)
      "6c31fc15422ebad28aaf9089c306702f67540b53c7eea8b7d2941044b027100f"
end

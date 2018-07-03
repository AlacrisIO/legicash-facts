open Lib
open Legibase
open Integer

(* create context just once, because expensive operation; assumes
   single instantiation of this module
*)
let secp256k1_ctx = Secp256k1.Context.create [Sign; Verify]

(** unique identifier for all parties, that is, customers and facilitators *)
type public_key = Secp256k1.Key.public Secp256k1.Key.t

(** private key in public-key cryptography *)
type private_key = Secp256k1.Key.secret Secp256k1.Key.t

(*module Int256 : Int with type t = Z.t : sig
  end*)

(* convert OCaml string to Secp256k1 msg format
   for strings representing hashes, the msg format is suitable for signing
*)
let string_to_secp256k1_msg s =
  let open Bigarray in
  let sz = String.length s in
  let buffer = Array1.create char c_layout sz in
  let _ = for i = 0 to sz - 1 do Bigarray.Array1.set buffer i s.[i] done in
  match Secp256k1.Sign.msg_of_bytes buffer with
  | Some msg -> msg
  | None -> raise (Internal_error "Could not create SECP256K1.Sign.msg from string")

(* convert arbitrary OCaml value to a Secp256k1 msg representing a hash *)
let data_to_secp256k1_hashed data =
  (* data is of arbitrary type, marshal to string *)
  let data_string = Marshal.to_string data [Marshal.Compat_32] in
  let hash = Cryptokit.hash_string (Cryptokit.Hash.keccak 256) in
  let hashed = hash data_string in
  string_to_secp256k1_msg hashed

(* create context just once, because expensive operation; assumes
   single instantiation of this module
*)
let secp256k1_ctx = Secp256k1.Context.create [Sign; Verify]

let make_digest v =
  let data_string = Marshal.to_string v [Marshal.Compat_32] in
  let hash = Cryptokit.Hash.keccak 256 in
  let hashed = Cryptokit.hash_string hash data_string in
  Nat.of_big_endian_bits hashed
[@@deprecated "Use make_digest only for demos and testing."]

(* TODO: check bounds, after every operation, etc. *)
module UInt256 = struct
  include Integer.UInt256
  let digest = make_digest
end

module Data256 = struct
  include UInt256
  let of_hex_string = sized_nat_of_hex_string 256
  let to_hex_string = hex_string_of_sized_nat 256
  let of_big_endian_bits = nat_of_big_endian_bits 256
  let to_big_endian_bits = big_endian_bits_of_nat 256
end

module Digest = struct
  include Data256
  let make = make_digest
end

type 'a digest = Digest.t

(** Special magic digest for None. A bit ugly. *)
let null_digest = Digest.zero

module type DigestibleS = sig
  type t
  val digest: t -> t digest
end

module type IntS = sig
  include Integer.IntS
  include DigestibleS with type t := t
end

module Nat = struct
  include Integer.Nat
  let digest x = Digest.make x
end

module UInt64 = struct
  include Integer.UInt64
  let digest x = Digest.make x
end

module Revision = UInt64
module Duration = UInt64
module Timestamp = UInt64


module StringT = struct
  include String
  let digest = make_digest
end

module Address = struct
  (* an address identifies a party (user, facilitator)
     this is per Ethereum: use the last 20 bytes of the party's public key *)

  include Nat

  let address_size = 20

  let of_hex_string = sized_nat_of_hex_string 160
  let to_hex_string = hex_string_of_sized_nat 160
  let of_big_endian_bits = nat_of_big_endian_bits 160
  let to_big_endian_bits = big_endian_bits_of_nat 160

  let of_public_key public_key =
    let open Bigarray in
    let buffer = Secp256k1.Key.to_bytes ~compress:false secp256k1_ctx public_key in
    let buffer_max = Array1.dim buffer - 1 in
    Nat.of_bits (String.init address_size (fun ndx -> Array1.get buffer (buffer_max - ndx)))

  let compare address1 address2 = Pervasives.compare address1 address2

  let equal address1 address2 = compare address1 address2 = 0

  let digest = make_digest
  let pp formatter x = Format.fprintf formatter "0x%s" (to_hex_string x)
  let show x = Format.asprintf "%a" pp x
end

module Unit = struct
  type t = unit
  let digest = konstant (Digest.make "")
  let pp formatter x = Format.fprintf formatter "%s" "()"
  let show x = Format.asprintf "%a" pp x
end

type 'a signature = Secp256k1.Sign.recoverable Secp256k1.Sign.t

type 'a signed = {payload: 'a; signature: 'a signature}

(* digital signature is encrypted hash *)
let make_signature private_key data =
  (* change representation of data to use Secp256k1 signing *)
  let secp256k1_hashed = data_to_secp256k1_hashed data in
  match Secp256k1.Sign.sign_recoverable secp256k1_ctx private_key secp256k1_hashed with
  | Ok signature -> signature
  | Error s -> raise (Internal_error s)

let address_matches_public_key address public_key =
  Address.equal address (Address.of_public_key public_key)

let is_signature_valid (address: Address.t) (signature: Secp256k1.Sign.recoverable signature) data =
  let hashed = data_to_secp256k1_hashed data in
  match Secp256k1.Sign.recover secp256k1_ctx ~msg:hashed ~signature with
  | Ok public_key -> address_matches_public_key address public_key
  | Error _ -> false

let sign private_key data = {payload= data; signature= make_signature private_key data}
module Test = struct
  (* test digests *)
  let mk_digest_test data expected =
    let digest = Digest.make data in
    expected = Digest.to_hex_string digest

  let%test "digest_1" =
    mk_digest_test "this is a test"
      "d5023901b6e1b3fd03543aa1ee403b7736a9085ab04e71a047d45b2a577f72e8"

  let%test "digest_2" =
    mk_digest_test (Some "nonsense")
      "e29dd9aecad9443bf6ea173d7057d3221c97cb941ac9aa9386abedace71688d0"

  let%test "digest_3" =
    mk_digest_test Int64.one
      "c6c680477d5c20cd351eab565405853a9f0900f493d03ec4e572c6f598534183"

  let%test "digest_4" =
    mk_digest_test [99.9; 100.4; 22.0; 1033.7]
      "f4d7eed0ed8614cfaa4cf1af0ff5dc2345a4a662d5aa57ed7a9bf4759450654a"
end

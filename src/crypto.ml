open Lib
open Legibase

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

module Digest = struct
  include Integer.Nat

  let make v =
    let data_string = Marshal.to_string v [Marshal.Compat_32] in
    let hash = Cryptokit.Hash.keccak 256 in
    let hashed = Cryptokit.hash_string hash data_string in
    let hashed_hex = "0x" ^ (unparse_hex ~with_colons:false hashed) in
    of_string hashed_hex
end

type 'a digest = Digest.t

(** Special magic digest for None. A bit ugly. *)
let null_digest = Digest.zero

module DigestSet = struct
  include Set.Make (Digest)

  let lens k = Lens.{get= mem k; set= (fun b -> if b then add k else remove k)}
end

module type DigestibleS = sig
  type t
  val digest: t -> t digest
end

module StringT = struct
  type t = string

  let digest string = Digest.make string
end

module Address : sig
  type t [@@deriving show]

  val of_public_key : Secp256k1.Key.public Secp256k1.Key.t -> t

  val compare : t -> t -> int

  val of_string : string -> t

  val to_string : t -> string

  val equal : t -> t -> bool

  val digest : t -> t digest
end = struct
  (* an address identifies a party (user, facilitator)
     this is per Ethereum: use the last 20 bytes of the party's public key *)

  type t = char array [@@deriving show]

  let address_size = 20

  let of_public_key public_key =
    let open Bigarray in
    let buffer = Secp256k1.Key.to_bytes ~compress:false secp256k1_ctx public_key in
    let buffer_len = Array1.dim buffer in
    let offset = buffer_len - address_size in
    Array.init address_size (fun ndx -> Array1.get buffer (offset + ndx))

  let compare address1 address2 = Pervasives.compare address1 address2

  let equal address1 address2 = compare address1 address2 = 0

  let of_string s =
    let len = String.length s in
    if len != address_size then
      raise (Internal_error (Printf.sprintf "String length is %d, expected %d" len address_size)) ;
    Array.init address_size (fun ndx -> s.[ndx])

  let to_string address = String.init address_size (Array.get address)

  let digest address =
    let s = to_string address in
    Integer.Nat.of_bits s
end

module AddressMap = MapMake (Address)

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
    expected = Digest.to_string digest

  let%test "digest_1" =
    mk_digest_test "this is a test"
      "96346563888126697166588750846833720374728628752847738452105337420790724784872"

  let%test "digest_2" =
    mk_digest_test (Some "nonsense")
      "102501601160338844936142598555848829880685647109855746236499212080808421918928"

  let%test "digest_3" =
    mk_digest_test Int64.one
      "89908665089203403328549695563738178150748540732630798379661072574227034620291"

  let%test "digest_4" =
    mk_digest_test [99.9; 100.4; 22.0; 1033.7]
      "110745855421557965285881006516000194355535263091999206059295831136738250614090"
end

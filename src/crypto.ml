open Lib
open Marshaling
open Integer

let keccak256_string s =
  Cryptokit.hash_string (Cryptokit.Hash.keccak 256) s
(* To debug encoding issues in small problems, you can uncomment the following line:
   |> fun h -> Printf.printf "hash %s <= %s\n%!" (Hex.unparse_hex_string h) (Hex.unparse_hex_string s); h *)
(* NB: trying to reuse (Cryptokit.Hash.keccak 256) object across calls results in a segfault *)

(* Create context just once, because it's an expensive operation.
   This assumes single instantiation of this module.
*)
let secp256k1_ctx = Secp256k1.Context.create [Sign; Verify]

module Digest = Data256

type digest = Digest.t

let digest_of_string s =
  Digest.of_big_endian_bits (keccak256_string s)
let digest_of_marshal_bytes marshal_bytes x =
  x |> marshal_bytes |> Bytes.to_string |> digest_of_string
let digest_of_marshal marshal =
  digest_of_marshal_bytes (marshal_bytes_of_marshal marshal)

(** Special magic digest for None. A bit ugly. *)
let null_digest = Digest.zero

module type DigestibleS = sig
  include MarshalableS
  val digest: t -> digest
end

module Digestible (M : MarshalableS) = struct
  include M
  let digest = digest_of_marshal_bytes M.marshal_bytes
end

module DigestibleOfPreMarshalable (P : PreMarshalableS) = Digestible(Marshalable(P))

module Address = struct
  include UInt256_z
  let address_size = 20
  let of_hex_string = sized_nat_of_hex_string 160
  let to_hex_string = hex_string_of_sized_nat 160
  let of_big_endian_bits = nat_of_big_endian_bits 160
  let to_big_endian_bits = big_endian_bits_of_nat 160
  let marshaling = marshaling_sized_string address_size to_big_endian_bits of_big_endian_bits
  let pp formatter x = Format.fprintf formatter "0x%s" (to_hex_string x)
  let show x = Format.asprintf "%a" pp x
  let of_public_key public_key =
    let buffer = Secp256k1.Key.to_bytes ~compress:false secp256k1_ctx public_key in
    (* uncompressed public key has an extra byte at the beginning, which we remove:
       https://bitcoin.stackexchange.com/questions/57855/c-secp256k1-what-do-prefixes-0x06-and-0x07-in-an-uncompressed-public-key-signif
    *)
    let pubkey_string = Cstruct.to_string (Cstruct.of_bigarray ~off:1 buffer) in
    let hash = keccak256_string pubkey_string in
    let hash_len = String.length hash in
    of_bits (String.init address_size (fun ndx -> hash.[hash_len - ndx - 1]))
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
  let buffer = Cstruct.to_bigarray (Cstruct.of_string s) in
  match Secp256k1.Sign.msg_of_bytes buffer with
  | Some msg -> msg
  | None -> raise (Internal_error "Could not create SECP256K1.Sign.msg from string")

(* convert a Digest.t to a Secp256k1 msg representing the digest *)
let secp256k1_msg_of_digest digest =
  secp256k1_msg_of_string (Digest.to_big_endian_bits digest)

let string_of_signature signature =
  (* see https://bitcoin.stackexchange.com/questions/38351/ecdsa-v-r-s-what-is-v
     for information about recovery id
  *)
  let bytes, recid = Secp256k1.Sign.to_bytes_recid secp256k1_ctx signature in
  let buffer = Buffer.create 8 in
  UInt64.marshaling.marshal buffer (UInt64.of_int recid);
  (Buffer.contents buffer) ^ (Cstruct.to_string (Cstruct.of_bigarray bytes))

let [@warning "-32"] signature_of_string string =
  let recid_bytes = Bytes.of_string (String.sub string 0 8) in
  let recid64,_ = UInt64.marshaling.unmarshal recid_bytes in
  let recid = UInt64.to_int recid64 in
  let signature_string = String.sub string 8 (String.length string - 8) in
  match Secp256k1.Sign.read_recoverable ~recid secp256k1_ctx
          (Cstruct.to_bigarray (Cstruct.of_string signature_string)) with
  | Ok signature -> signature
  | Error msg ->
    raise (Internal_error (Format.sprintf "Could not get signature from string: %s" msg))

module Signature = struct
  (* 8 bytes for the recovery id + 64 bytes for the signature proper *)
  let width = 72
  module S = struct
    type t = signature
    let marshaling = marshaling_sized_string width string_of_signature signature_of_string
  end
  include YojsonableOfMarshalable(Marshalable(S))
end

type 'a signed = {payload: 'a; signature: signature}

let marshal_signed marshal buffer {payload; signature} =
  marshal buffer payload; Signature.marshaling.marshal buffer signature

let unmarshal_signed (unmarshal:'a unmarshaler) ?(start=0) bytes : 'a signed * int =
  let payload,payload_offset = unmarshal ~start bytes in
  let signature,final_offset = Signature.marshaling.unmarshal ~start:payload_offset bytes in
  ( { payload
    ; signature
    }
  , final_offset
  )

let marshaling_signed marshaling =
  { marshal = marshal_signed marshaling.marshal
  ; unmarshal = unmarshal_signed marshaling.unmarshal }

let signed_to_yojson to_yojson { payload ; signature } =
  `Assoc [ ("payload", to_yojson payload)
         ; ("signature", Signature.to_yojson signature) ]

let signed_of_yojson of_yojson = function
  | `Assoc [ ("payload", pj); ("signature", sj) ] ->
    Result.bind (of_yojson pj)
      (fun payload ->
         Result.bind (Signature.of_yojson sj)
           (fun signature -> Ok {payload;signature}))
  | _ -> Error "bad json for signed data"

let signed_of_yojson_exn of_yojson_exn = function
  | `Assoc [ ("payload", p); ("signature", s) ] ->
    {payload=of_yojson_exn p;signature=Signature.of_yojson_exn s}
  | _ -> Yojson.json_error "bad json for signed data"


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

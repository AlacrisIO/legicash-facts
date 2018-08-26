open Lib
open Hex
open Yojsoning
open Marshaling
open Integer
open Tag
open Digesting

module Address = struct
  include Data160
  include (YojsonMarshalable(Data160) : YojsonMarshalableS with type t := t)
end
type address = Address.t

(* Create context just once, because it's an expensive operation.
   This assumes single instantiation of this module in a single-threaded environment.
*)
let secp256k1_ctx = Secp256k1.Context.create [Sign; Verify]

let private_key_length = 32

let public_key_length = 65

let bytes_of_key key =
  key |> Secp256k1.Key.to_bytes ~compress:false secp256k1_ctx |> Cstruct.of_bigarray |> Cstruct.to_bytes

module PublicKey = struct
  module P = struct
    type t = Secp256k1.Key.public Secp256k1.Key.t
    let marshal buffer (public_key : t) =
      Buffer.add_bytes buffer (bytes_of_key public_key)
    let unmarshal ?(start=0) bytes =
      let public_buffer = Cstruct.create public_key_length in
      Cstruct.blit_from_bytes bytes start public_buffer 0 public_key_length;
      match Secp256k1.Key.read_pk secp256k1_ctx (Cstruct.to_bigarray public_buffer) with
      | Ok (key : t) -> key, start + public_key_length
      | Error s -> bork "Could not unmarshal public key: %s" s
    let marshaling = {marshal;unmarshal}
  end
  include YojsonableOfPreMarshalable(P)
end
type public_key = PublicKey.t

let address_of_public_key public_key =
  let buffer = Secp256k1.Key.to_bytes ~compress:false secp256k1_ctx public_key in
  (* uncompressed public key has an extra byte at the beginning, which we remove:
     https://bitcoin.stackexchange.com/questions/57855/c-secp256k1-what-do-prefixes-0x06-and-0x07-in-an-uncompressed-public-key-signif
  *)
  let pubkey_string = Cstruct.to_string (Cstruct.of_bigarray ~off:1 buffer) in
  let hash = keccak256_string pubkey_string in
  let hash_len = String.length hash in
  Address.of_bits (String.init Address.size_in_bytes (fun ndx -> hash.[hash_len - ndx - 1]))

module PrivateKey = struct
  module P = struct
    type t = Secp256k1.Key.secret Secp256k1.Key.t
    let marshal buffer (private_key: t) =
      Buffer.add_bytes buffer (bytes_of_key private_key)
    let unmarshal ?(start=0) bytes =
      let private_buffer = Cstruct.create private_key_length in
      Cstruct.blit_from_bytes bytes start private_buffer 0 private_key_length;
      match Secp256k1.Key.read_sk secp256k1_ctx (Cstruct.to_bigarray private_buffer) with
      | Ok key -> key, start + private_key_length
      | Error s -> bork "Could not unmarshal private key: %s" s
    let marshaling = {marshal;unmarshal}
  end
  include YojsonableOfPreMarshalable(P)
end
type private_key = PrivateKey.t

let string_of_signature signature =
  (* see https://bitcoin.stackexchange.com/questions/38351/ecdsa-v-r-s-what-is-v
     for information about recovery id
  *)
  let bytes, recid = Secp256k1.Sign.to_bytes_recid secp256k1_ctx signature in
  let buffer = Buffer.create 8 in
  UInt64.marshaling.marshal buffer (UInt64.of_int recid);
  (Buffer.contents buffer) ^ (Cstruct.to_string (Cstruct.of_bigarray bytes))

let signature_of_string string =
  let recid_bytes = Bytes.of_string (String.sub string 0 8) in
  let recid64,_ = UInt64.marshaling.unmarshal recid_bytes in
  let recid = UInt64.to_int recid64 in
  let signature_string = String.sub string 8 (String.length string - 8) in
  match Secp256k1.Sign.read_recoverable ~recid secp256k1_ctx
          (Cstruct.to_bigarray (Cstruct.of_string signature_string)) with
  | Ok signature -> signature
  | Error msg -> bork "Could not get signature from string: %s" msg

module Signature = struct
  (* 8 bytes for the recovery id + 64 bytes for the signature proper *)
  let width = 72
  module P = struct
    type t = Secp256k1.Sign.recoverable Secp256k1.Sign.t
    let marshaling = marshaling_sized_string width string_of_signature signature_of_string
    let yojsoning = yojsoning_map string_of_signature signature_of_string string_0x_yojsoning
  end
  include Marshalable(P)
  include (Yojsonable(P) : YojsonableS with type t := t)
end
type signature = Signature.t

module Keypair = struct
  [@warning "-39"]
  type t =
    { address: Address.t
    ; public_key: PublicKey.t
    ; private_key: PrivateKey.t }
  [@@deriving lens {prefix=true}, yojson]
  module P = struct
    type nonrec t = t
    let yojsoning = {to_yojson;of_yojson}
    let marshaling =
      marshaling_tagged Tag.keypair
        (marshaling2
           (fun {public_key; private_key} -> public_key, private_key)
           (fun public_key private_key ->
              {address=address_of_public_key public_key; public_key; private_key})
           PublicKey.marshaling PrivateKey.marshaling)
  end
  include (YojsonMarshalable(P) : YojsonMarshalableS with type t := t)
end
type keypair = Keypair.t


let make_public_key public_key_string =
  let len = String.length public_key_string in
  if len <> public_key_length then
    bork "Bad public key length %d for %s" len (unparse_coloned_hex_string public_key_string);
  let public_key_buffer = Cstruct.to_bigarray (Cstruct.of_string public_key_string) in
  match Secp256k1.Key.read_pk secp256k1_ctx public_key_buffer with
  | Ok pk -> pk
  | Error msg -> bork "%s" msg

let make_private_key private_key_string =
  let len = String.length private_key_string in
  if len <> private_key_length then
    bork "Bad private key length %d for %s" len (unparse_coloned_hex_string private_key_string);
  let private_key_buffer = Cstruct.to_bigarray (Cstruct.of_string private_key_string) in
  match Secp256k1.Key.read_sk secp256k1_ctx private_key_buffer with
  | Ok sk -> sk
  | Error msg -> bork "%s" msg

type 'a signed = {payload: 'a; signature: signature}


let make_keypair private_key_string public_key_string =
  let public_key = make_public_key public_key_string in
  let private_key = make_private_key private_key_string in
  let address = address_of_public_key public_key in
  Keypair.{address; public_key; private_key}

let make_keypair_from_hex private_key_hex public_key_hex =
  make_keypair (parse_coloned_hex_string private_key_hex) (parse_coloned_hex_string public_key_hex)

let registered_keypairs = Hashtbl.create 8

let register_keypair keypair =
  Hashtbl.replace registered_keypairs keypair.Keypair.address keypair

let unregister_keypair keypair =
  Hashtbl.remove registered_keypairs keypair.Keypair.address

let keypair_of_address address =
  Hashtbl.find registered_keypairs address


(* convert OCaml string of suitable length (32 only?) to Secp256k1 msg format
   for strings representing hashes, the msg format is suitable for signing.
*)
let secp256k1_msg_of_string s =
  let buffer = Cstruct.to_bigarray (Cstruct.of_string s) in
  match Secp256k1.Sign.msg_of_bytes buffer with
  | Some msg -> msg
  | None -> bork "Could not create SECP256K1.Sign.msg from string"

(* convert a Digest.t to a Secp256k1 msg representing the digest *)
let secp256k1_msg_of_digest digest =
  secp256k1_msg_of_string (Digest.to_big_endian_bits digest)

(* digital signature is encrypted hash *)
let make_signature make_digest private_key data =
  (* change representation of data to use Secp256k1 signing *)
  let secp256k1_msg = secp256k1_msg_of_digest (make_digest data) in
  match Secp256k1.Sign.sign_recoverable secp256k1_ctx ~sk:private_key secp256k1_msg with
  | Ok signature -> signature
  | Error e -> bork "Could not sign: %s" e

let signature_vrs signature =
  let (signature_buffer, recid) = Secp256k1.Sign.to_bytes_recid secp256k1_ctx signature in
  let signature_cstruct = Cstruct.of_bigarray signature_buffer in
  let string_from_signature_subarray start len =
    let bytes = Bytes.create len in
    Cstruct.blit_to_bytes signature_cstruct start bytes 0 len;
    Bytes.to_string bytes in
  let v = String.make 1 (Char.chr (recid + 27)) in
  let r = string_from_signature_subarray 0 32 in
  let s = string_from_signature_subarray 32 32 in
  (v, r, s)

let address_matches_public_key address public_key =
  Address.equal address (address_of_public_key public_key)

(* check validity of signature for data *)
let is_signature_valid make_digest (address: Address.t) (signature: Signature.t) data =
  let msg = secp256k1_msg_of_digest (make_digest data) in
  match Secp256k1.Sign.recover secp256k1_ctx ~msg ~signature with
  | Ok public_key -> address_matches_public_key address public_key
  | Error _ -> false

(* check validity of signature for payload within signed value *)
let is_signed_value_valid make_digest address signed_value =
  is_signature_valid make_digest address signed_value.signature signed_value.payload

let signed make_digest private_key data =
  {payload= data; signature= make_signature make_digest private_key data}

let marshal_signed marshal buffer {payload; signature} =
  marshal buffer payload; Signature.marshal buffer signature

let unmarshal_signed (unmarshal:'a unmarshaler) ?(start=0) bytes : 'a signed * int =
  let payload,payload_offset = unmarshal ~start bytes in
  let signature,final_offset = Signature.unmarshal ~start:payload_offset bytes in
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



module type SignableS = sig
  include DigestibleS
  val signed : keypair -> t -> t signed
end

let signed_of_digest digest kp = signed digest kp.Keypair.private_key

module Signable (M : MarshalableS) = struct
  include M
  let digest = digest_of_marshal_bytes M.marshal_bytes
  let signed = signed_of_digest digest
end

module Test = struct

  let trent_keys =
    make_keypair_from_hex
      "b6:fb:0b:7e:61:36:3e:e2:f7:48:16:13:38:f5:69:53:e8:aa:42:64:2e:99:90:ef:f1:7e:7d:e9:aa:89:57:86"
      "04:26:bd:98:85:f2:c9:e2:3d:18:c3:02:5d:a7:0e:71:a4:f7:ce:23:71:24:35:28:82:ea:fb:d1:cb:b1:e9:74:2c:4f:e3:84:7c:e1:a5:6a:0d:19:df:7a:7d:38:5a:21:34:be:05:20:8b:5d:1c:cc:5d:01:5f:5e:9a:3b:a0:d7:df"

  let trent_address = trent_keys.address

  let alice_keys =
    make_keypair_from_hex
      "d5:69:84:dc:08:3d:76:97:01:71:4e:eb:1d:4c:47:a4:54:25:5a:3b:bc:3e:9f:44:84:20:8c:52:bd:a3:b6:4e"
      "04:23:a7:cd:9a:03:fa:9c:58:57:e5:14:ae:5a:cb:18:ca:91:e0:7d:69:45:3e:d8:51:36:ea:6a:00:36:10:67:b8:60:a5:b2:0f:11:53:33:3a:ef:2d:1b:a1:3b:1d:7a:52:de:28:69:d1:f6:23:71:bf:81:bf:80:3c:21:c6:7a:ca"

  let alice_address = alice_keys.address

  let bob_keys =
    make_keypair_from_hex
      "f1:d3:cd:20:22:e1:d6:64:98:32:76:04:83:4d:f0:73:06:64:f7:1a:8d:d1:1e:46:a3:3b:4a:0e:bb:40:ca:8e"
      "04:7d:52:54:04:9f:02:3e:e7:aa:ea:1e:fa:4f:17:ae:70:0f:af:67:23:24:02:5a:a9:b5:32:5a:92:1f:d0:f1:51:0e:68:31:f1:bf:90:b4:a1:df:e1:cd:49:e5:03:ec:7d:b5:9f:6e:78:73:d0:3a:3a:09:6c:46:5c:87:22:22:69"

  let bob_address = bob_keys.address

  let invert_hex hex_str = unparse_coloned_hex_string (parse_coloned_hex_string hex_str)

  (* test that hex parsing and unparsing are inverses *)

  let%test "inverse_1" =
    let key =
      "d5:69:84:dc:08:3d:76:97:01:71:4e:eb:1d:4c:47:a4:54:25:5a:3b:bc:3e:9f:44:84:20:8c:52:bd:a3:b6:4e"
    in
    invert_hex key = key

  let%test "inverse_2" =
    let key =
      "04:7d:52:54:04:9f:02:3e:e7:aa:ea:1e:fa:4f:17:ae:70:0f:af:67:23:24:02:5a:a9:b5:32:5a:92:1f:d0:f1:51:0e:68:31:f1:bf:90:b4:a1:df:e1:cd:49:e5:03:ec:7d:b5:9f:6e:78:73:d0:3a:3a:09:6c:46:5c:87:22:22:69"
    in
    invert_hex key = key

  (* test validity of digital signatures *)

  let%test "alice_signature" =
    let alice_data = "some arbitrary string for Alice to sign" in
    let alice_signature = make_signature digest_of_string alice_keys.private_key alice_data in
    is_signature_valid digest_of_string alice_keys.address alice_signature alice_data

  let%test "bob_signature" =
    let bob_data = "some arbitrary string for Bob to sign" in
    let bob_signature = make_signature digest_of_string bob_keys.private_key bob_data in
    is_signature_valid digest_of_string bob_keys.address bob_signature bob_data

  let%test "trent_signature" =
    let trent_data = "some arbitrary string for Trent to sign" in
    let trent_signature = make_signature digest_of_string trent_keys.private_key trent_data in
    is_signature_valid digest_of_string trent_keys.address trent_signature trent_data

  (* test that Cryptokit's Keccak256 hash is same as Ethereum's

     this is the hash and message found in function TestKeccak256Hash in
     https://github.com/ethereum/go-ethereum/blob/master/crypto/crypto_test.go

     we put this test here because parse_coloned_hex_string is not exported
  *)

  let%test "ethereum_keccak256_hash" =
    let msg = "abc" in
    let hash = Cryptokit.hash_string (Cryptokit.Hash.keccak 256) msg in
    hash
    = parse_coloned_hex_string
        "4e:03:65:7a:ea:45:a9:4f:c7:d4:7b:a8:26:c8:d6:67:c0:d1:e6:e3:3a:64:a0:36:ec:44:f5:8f:a1:2d:6c:45"

  (* test that addresses are really last 20 bytes of Keccak256 hash of public keys *)

  let%test "alice_address_from_public_key" =
    let alice_address = alice_keys.address in
    unparse_coloned_hex_string (Address.to_big_endian_bits alice_address)
    = "bb:d1:7b:e6:f6:83:f7:20:23:87:3a:fe:aa:57:c8:8d:24:b5:88:84"

  let%test "bob_address_from_public_key" =
    let bob_address = bob_keys.address in
    unparse_coloned_hex_string (Address.to_big_endian_bits bob_address)
    = "8a:6b:38:3d:f4:79:7f:28:67:2b:77:17:9e:be:3d:b9:03:cc:ad:34"

  let%test "trent_address_from_public_key" =
    let trent_address = trent_keys.address in
    unparse_coloned_hex_string (Address.to_big_endian_bits trent_address)
    = "f4:74:08:14:3d:32:7e:4b:c6:a8:7e:f4:a7:0a:4e:0a:f0:9b:9a:1c"
end

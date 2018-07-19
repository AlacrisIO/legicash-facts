(* keypair.ml -- Secp256k1 key pairs *)

open Bigarray
open Lib
open Crypto

type t =
  { private_key: Secp256k1.Key.secret Secp256k1.Key.t
  ; public_key: Secp256k1.Key.public Secp256k1.Key.t
  ; address: Address.t }

let private_key_length = 32

let public_key_length = 65

let string_to_secp256k1_buffer s =
  let len = String.length s in
  let buffer = Array1.create char c_layout len in
  for ndx = 0 to len - 1 do Bigarray.Array1.set buffer ndx s.[ndx] done ;
  buffer

let make_public_key public_key_string =
  let len = String.length public_key_string in
  if len <> public_key_length then
    raise
      (Internal_error
         (Printf.sprintf "Bad public key length %d for %s" len
            (unparse_coloned_hex_string public_key_string))) ;
  let public_key_buffer = string_to_secp256k1_buffer public_key_string in
  match Secp256k1.Key.read_pk secp256k1_ctx public_key_buffer with
  | Ok pk -> pk
  | Error msg -> raise (Internal_error msg)

let make_private_key private_key_string =
  let len = String.length private_key_string in
  if len <> private_key_length then
    raise
      (Internal_error
         (Printf.sprintf "Bad private key length %d for %s" len
            (unparse_coloned_hex_string private_key_string))) ;
  let private_key_buffer = string_to_secp256k1_buffer private_key_string in
  match Secp256k1.Key.read_sk secp256k1_ctx private_key_buffer with
  | Ok sk -> sk
  | Error msg -> raise (Internal_error msg)

let make_keys private_key_string public_key_string =
  let public_key = make_public_key public_key_string in
  let private_key = make_private_key private_key_string in
  let address = Address.of_public_key public_key in
  {private_key; public_key; address}

let make_keys_from_hex private_key_hex public_key_hex =
  make_keys (parse_coloned_hex_string private_key_hex) (parse_coloned_hex_string public_key_hex)

module Marshalable = struct
  open Bigarray

  type nonrec t = t

  let marshal buffer t =
    let bytes_of_key key =
      let buffer = Secp256k1.Key.to_bytes ~compress:false secp256k1_ctx key in
      Bytes.init (Array1.dim buffer) (Array1.get buffer)
    in
    Buffer.add_bytes buffer (bytes_of_key t.private_key);
    Buffer.add_bytes buffer (bytes_of_key t.public_key);
    Address.marshal buffer t.address

  let unmarshal ?(start=0) bytes =
    let fill_buffer buffer offset len =
      for ndx = offset to offset + len - 1 do
        Array1.set buffer ndx (Bytes.get bytes ndx)
      done
    in
    let private_buffer = Array1.create char c_layout private_key_length in
    let _ = fill_buffer private_buffer start private_key_length in
    let private_key =
      match Secp256k1.Key.read_sk secp256k1_ctx private_buffer with
      | Ok key -> key
      | Error s -> raise (Internal_error ("Could not unmarshal private key: " ^ s))
    in
    let public_buffer = Array1.create char c_layout public_key_length in
    let _ = fill_buffer private_buffer (start + private_key_length) public_key_length in
    let public_key =
      match Secp256k1.Key.read_pk secp256k1_ctx public_buffer with
      | Ok key -> key
      | Error s -> raise (Internal_error ("Could not unmarshal public key: " ^ s))
    in
    let address,final_offset = Address.unmarshal ~start:(start + private_key_length + public_key_length) bytes in
    ( { private_key
      ; public_key
      ; address
      }
      ,
      final_offset
    )

end

include (DigestibleOfMarshalable (Marshalable) : DigestibleS with type t := t)

module Test = struct

  let trent_keys =
    make_keys_from_hex
      "b6:fb:0b:7e:61:36:3e:e2:f7:48:16:13:38:f5:69:53:e8:aa:42:64:2e:99:90:ef:f1:7e:7d:e9:aa:89:57:86"
      "04:26:bd:98:85:f2:c9:e2:3d:18:c3:02:5d:a7:0e:71:a4:f7:ce:23:71:24:35:28:82:ea:fb:d1:cb:b1:e9:74:2c:4f:e3:84:7c:e1:a5:6a:0d:19:df:7a:7d:38:5a:21:34:be:05:20:8b:5d:1c:cc:5d:01:5f:5e:9a:3b:a0:d7:df"

  let trent_address = trent_keys.address

  let alice_keys =
    make_keys_from_hex
      "d5:69:84:dc:08:3d:76:97:01:71:4e:eb:1d:4c:47:a4:54:25:5a:3b:bc:3e:9f:44:84:20:8c:52:bd:a3:b6:4e"
      "04:23:a7:cd:9a:03:fa:9c:58:57:e5:14:ae:5a:cb:18:ca:91:e0:7d:69:45:3e:d8:51:36:ea:6a:00:36:10:67:b8:60:a5:b2:0f:11:53:33:3a:ef:2d:1b:a1:3b:1d:7a:52:de:28:69:d1:f6:23:71:bf:81:bf:80:3c:21:c6:7a:ca"

  let alice_address = alice_keys.address

  let bob_keys =
    make_keys_from_hex
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

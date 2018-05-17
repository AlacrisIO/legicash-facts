(* keypair.ml -- Secp256k1 key pairs *)

open Bigarray
open Legibase
open Lib

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


let make_keys private_key_string public_key_string =
  let _ =
    let len = String.length private_key_string in
    if len <> private_key_length then
      raise
        (Internal_error
           (Printf.sprintf "Bad private key length %d for %s" len (unparse_hex private_key_string)))
  in
  let _ =
    let len = String.length public_key_string in
    if len <> public_key_length then
      raise
        (Internal_error
           (Printf.sprintf "Bad public key length %d for %s" len (unparse_hex public_key_string)))
  in
  let private_key_buffer = string_to_secp256k1_buffer private_key_string in
  let public_key_buffer = string_to_secp256k1_buffer public_key_string in
  let private_key =
    match Secp256k1.Key.read_sk secp256k1_ctx private_key_buffer with
    | Ok sk -> sk
    | Error msg -> raise (Internal_error msg)
  in
  let public_key =
    match Secp256k1.Key.read_pk secp256k1_ctx public_key_buffer with
    | Ok pk -> pk
    | Error msg -> raise (Internal_error msg)
  in
  let address = Address.of_public_key public_key in
  {private_key; public_key; address}


let address_matches_public_key address public_key =
  Address.equal address (Address.of_public_key public_key)


let make_keys_from_hex private_key_hex public_key_hex =
  make_keys (parse_hex private_key_hex) (parse_hex public_key_hex)


module Test = struct
  let invert_hex hex_str = unparse_hex (parse_hex hex_str)

  (* test that hex parsing and unparsing are inverses *)

  [%%test
  let "inverse_1" =
    let key =
      "d5:69:84:dc:08:3d:76:97:01:71:4e:eb:1d:4c:47:a4:54:25:5a:3b:bc:3e:9f:44:84:20:8c:52:bd:a3:b6:4e"
    in
    invert_hex key = key]

  [%%test
  let "inverse_2" =
    let key =
      "04:7d:52:54:04:9f:02:3e:e7:aa:ea:1e:fa:4f:17:ae:70:0f:af:67:23:24:02:5a:a9:b5:32:5a:92:1f:d0:f1:51:0e:68:31:f1:bf:90:b4:a1:df:e1:cd:49:e5:03:ec:7d:b5:9f:6e:78:73:d0:3a:3a:09:6c:46:5c:87:22:22:69"
    in
    invert_hex key = key]

  (* test validity of digital signatures *)

  let alice_keys =
    make_keys_from_hex
      "d5:69:84:dc:08:3d:76:97:01:71:4e:eb:1d:4c:47:a4:54:25:5a:3b:bc:3e:9f:44:84:20:8c:52:bd:a3:b6:4e"
      "04:23:a7:cd:9a:03:fa:9c:58:57:e5:14:ae:5a:cb:18:ca:91:e0:7d:69:45:3e:d8:51:36:ea:6a:00:36:10:67:b8:60:a5:b2:0f:11:53:33:3a:ef:2d:1b:a1:3b:1d:7a:52:de:28:69:d1:f6:23:71:bf:81:bf:80:3c:21:c6:7a:ca"


  let bob_keys =
    make_keys_from_hex
      "f1:d3:cd:20:22:e1:d6:64:98:32:76:04:83:4d:f0:73:06:64:f7:1a:8d:d1:1e:46:a3:3b:4a:0e:bb:40:ca:8e"
      "04:7d:52:54:04:9f:02:3e:e7:aa:ea:1e:fa:4f:17:ae:70:0f:af:67:23:24:02:5a:a9:b5:32:5a:92:1f:d0:f1:51:0e:68:31:f1:bf:90:b4:a1:df:e1:cd:49:e5:03:ec:7d:b5:9f:6e:78:73:d0:3a:3a:09:6c:46:5c:87:22:22:69"


  let trent_keys =
    make_keys_from_hex
      "b6:fb:0b:7e:61:36:3e:e2:f7:48:16:13:38:f5:69:53:e8:aa:42:64:2e:99:90:ef:f1:7e:7d:e9:aa:89:57:86"
      "04:26:bd:98:85:f2:c9:e2:3d:18:c3:02:5d:a7:0e:71:a4:f7:ce:23:71:24:35:28:82:ea:fb:d1:cb:b1:e9:74:2c:4f:e3:84:7c:e1:a5:6a:0d:19:df:7a:7d:38:5a:21:34:be:05:20:8b:5d:1c:cc:5d:01:5f:5e:9a:3b:a0:d7:df"


  [%%test
  let "alice_signature" =
    let alice_data = "some arbitrary string for Alice to sign" in
    let alice_signature = Legibase.make_signature alice_keys.private_key alice_data in
    Legibase.is_signature_valid alice_keys.public_key alice_signature alice_data]

  [%%test
  let "bob_signature" =
    let bob_data = "some arbitrary string for Bob to sign" in
    let bob_signature = Legibase.make_signature bob_keys.private_key bob_data in
    Legibase.is_signature_valid bob_keys.public_key bob_signature bob_data]

  [%%test
  let "trent_signature" =
    let trent_data = "some arbitrary string for Trent to sign" in
    let trent_signature = Legibase.make_signature trent_keys.private_key trent_data in
    Legibase.is_signature_valid trent_keys.public_key trent_signature trent_data]

  (* test that Cryptokit's Keccak256 hash is same as Ethereum's

   this is the hash and message found in function TestKeccak256Hash in
   https://github.com/ethereum/go-ethereum/blob/master/crypto/crypto_test.go

   we put this test here because parse_hex is not exported

 *)

  [%%test
  let "ethereum_keccak256_hash" =
    let msg = "abc" in
    let hash = Cryptokit.hash_string (Cryptokit.Hash.keccak 256) msg in
    hash
    = parse_hex
        "4e:03:65:7a:ea:45:a9:4f:c7:d4:7b:a8:26:c8:d6:67:c0:d1:e6:e3:3a:64:a0:36:ec:44:f5:8f:a1:2d:6c:45"]

  (* test that addresses are really last 20 bytes of public keys *)

  [%%test
  let "alice_address_from_public_key" =
    let alice_address = alice_keys.address in
    unparse_hex (Address.to_string alice_address)
    = "3b:1d:7a:52:de:28:69:d1:f6:23:71:bf:81:bf:80:3c:21:c6:7a:ca"]

  [%%test
  let "bob_address_from_public_key" =
    let bob_address = bob_keys.address in
    unparse_hex (Address.to_string bob_address)
    = "e5:03:ec:7d:b5:9f:6e:78:73:d0:3a:3a:09:6c:46:5c:87:22:22:69"]

  [%%test
  let "trent_address_from_public_key" =
    let trent_address = trent_keys.address in
    unparse_hex (Address.to_string trent_address)
    = "38:5a:21:34:be:05:20:8b:5d:1c:cc:5d:01:5f:5e:9a:3b:a0:d7:df"]
end

open Marshaling
open Integer

module Digest = Data256
type digest = Digest.t
[@@deriving rlp]

let keccak256_string s = Cryptokit.hash_string (Cryptokit.Hash.keccak 256) s
let digest_of_string s = Digest.of_big_endian_bits (keccak256_string s)

let digest_of_marshal_bytes marshal_bytes x =
  x |> marshal_bytes |> Bytes.to_string |> digest_of_string

let digest_of_marshal marshal =
  digest_of_marshal_bytes (marshal_bytes_of_marshal marshal)

let null_digest = Digest.zero

module type DigestibleS = sig
  include MarshalableS
  val digest : t -> digest
end

module Digestible (M : MarshalableS) = struct
  include M
  let digest = digest_of_marshal_bytes M.marshal_bytes
end

module DigestibleOfPreMarshalable (P : PreMarshalableS) = Digestible(Marshalable(P))

module Test = struct
  (* test digests *)
  let mk_digest_test data expected =
    let digest = digest_of_string data in
    expected = Digest.to_hex_string digest

  (* test that Cryptokit's Keccak256 hash, that we use, is the same as Ethereum's
     this is the hash and message found in function TestKeccak256Hash in
     https://github.com/ethereum/go-ethereum/blob/master/crypto/crypto_test.go
  *)
  let%test "ethereum_keccak256_hash" =
    mk_digest_test "abc"
      "4e03657aea45a94fc7d47ba826c8d667c0d1e6e33a64a036ec44f58fa12d6c45"

  let%test "digest_1" =
    mk_digest_test "this is a test"
      "9fd09c38c2a5ae0a0bcd617872b735e37909ccc05c956460be7d3d03d881a0dc"

  let%test "digest_2" =
    mk_digest_test (UInt64.to_big_endian_bits UInt64.one)
      "6c31fc15422ebad28aaf9089c306702f67540b53c7eea8b7d2941044b027100f"
end

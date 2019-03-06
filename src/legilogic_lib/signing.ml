open Lib
open Hex
open Yojsoning
open Marshaling
open Tag
open Digesting
open Persisting
open Types
open Action
open Ppx_deriving_rlp_runtime
open Rlping

module Address = struct
  include Data160
  include (YojsonMarshalable(Data160) : YojsonMarshalableS with type t := t)
end
type address = Address.t
[@@deriving rlp]

(* Create context just once, because it's an expensive operation.
   This assumes single instantiation of this module in a single-threaded environment.
*)
let (secp256k1_ctx : Secp256k1.Context.t) = Secp256k1.Context.create [Sign; Verify]

let private_key_length = 32

(* Ethereum can deduce the correct 65th byte from just 64 ones.
   TODO: Be like Ethereum.
   Experimentally: looks like it is always 0x04:
   cat src/endpoints/demo-keys-*json | grep public_key | cut -c1-21 | sort -u
*)
let public_key_length = 65

let bytes_of_key (key : 'a Secp256k1.Key.t) : bytes =
  key |> Secp256k1.Key.to_bytes ~compress:false secp256k1_ctx |> Cstruct.of_bigarray |> Cstruct.to_bytes

let string_of_key key = Bytes.to_string (bytes_of_key key)

module PublicKey = struct
  module P = struct
    type t = Secp256k1.Key.public Secp256k1.Key.t
    let marshal buffer (public_key : t) =
      Buffer.add_bytes buffer (bytes_of_key public_key)
    let unmarshal start bytes =
      let public_buffer = Cstruct.create public_key_length in
      Cstruct.blit_from_bytes bytes start public_buffer 0 public_key_length;
      match Secp256k1.Key.read_pk secp256k1_ctx (Cstruct.to_bigarray public_buffer) with
      | Ok (key : t) -> key, start + public_key_length
      | Error s -> bork "Could not unmarshal public key: %s" s
    let marshaling = {marshal;unmarshal}

    let of_string string =
      let (v,_) = unmarshal 0 (Bytes.of_string string) in
      v
    let rlping = rlping_by_isomorphism of_string string_of_key string_rlping
  end
  include YojsonableOfPreMarshalable(P)
  let pp formatter x = Format.fprintf formatter "%s" (x |> marshal_string |> unparse_0x_data)
  let show x = Format.asprintf "%a" pp x
  let rlping = P.rlping
  let { to_rlp_item; of_rlp_item; of_rlp_item_opt;
        to_rlp; of_rlp; of_rlp_opt;
        marshal_rlp; unmarshal_rlp; unmarshal_rlp_opt }
      =
      rlping
end
type public_key = PublicKey.t
[@@deriving rlp]

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
    let unmarshal start bytes =
      let private_buffer = Cstruct.create private_key_length in
      Cstruct.blit_from_bytes bytes start private_buffer 0 private_key_length;
      match Secp256k1.Key.read_sk secp256k1_ctx (Cstruct.to_bigarray private_buffer) with
      | Ok key -> key, start + private_key_length
      | Error s -> bork "Could not unmarshal private key: %s" s
    let marshaling = {marshal;unmarshal}

    let of_string string =
      let (v,_) = unmarshal 0 (Bytes.of_string string) in
      v
    let rlping = rlping_by_isomorphism of_string string_of_key string_rlping
  end
  include YojsonableOfPreMarshalable(P)
  let rlping = P.rlping
  let { to_rlp_item; of_rlp_item; of_rlp_item_opt;
        to_rlp; of_rlp; of_rlp_opt;
        marshal_rlp; unmarshal_rlp; unmarshal_rlp_opt }
      =
      rlping
end
type private_key = PrivateKey.t
[@@deriving rlp]

let string_of_signature signature =
  (* see https://bitcoin.stackexchange.com/questions/38351/ecdsa-v-r-s-what-is-v
     for information about recovery id
  *)
  let bytes, recid = Secp256k1.Sign.to_bytes_recid secp256k1_ctx signature in
  let buffer = Buffer.create 8 in
  UInt64.marshaling.marshal buffer (UInt64.of_int recid);
  (Buffer.contents buffer) ^ (Cstruct.to_string (Cstruct.of_bigarray bytes))

let signature_of_string string =
  let recid64 = UInt64.unmarshal_string (String.sub string 0 8) in
  let recid = UInt64.to_int recid64 in
  let signature_string = String.sub string 8 (String.length string - 8) in
  match Secp256k1.Sign.read_recoverable ~recid secp256k1_ctx
          (Cstruct.to_bigarray (Cstruct.of_string signature_string)) with
  | Ok signature -> signature
  | Error msg -> bork "Could not get signature from string: %s" msg

module Signature = struct
  (* 8 bytes for the recovery id + 64 bytes for the signature proper *)
  (* let width = 72 *)
  module P = struct
    type t = Secp256k1.Sign.recoverable Secp256k1.Sign.t
    let rlping = rlping_by_isomorphism signature_of_string string_of_signature string_rlping
    let marshaling = marshaling_of_rlping rlping
    let yojsoning = yojsoning_map string_of_signature signature_of_string string_0x_yojsoning
  end
  include TrivialPersistable (P)

  let rlping = P.rlping
  let { to_rlp_item; of_rlp_item; of_rlp_item_opt;
        to_rlp; of_rlp; of_rlp_opt;
        marshal_rlp; unmarshal_rlp; unmarshal_rlp_opt }
      =
      rlping
end
type signature = Signature.t
[@@deriving rlp]

module Keypair = struct
  [@warning "-39"]
  type t =
    { address: Address.t
    ; public_key: PublicKey.t
    ; private_key: PrivateKey.t
    ; password: string }
  [@@deriving lens {prefix=true}, yojson, rlp]
  module P = struct
    type nonrec t = t
    let yojsoning = {to_yojson;of_yojson}
    let marshaling = marshaling_of_rlping rlping
  end
  include (YojsonMarshalable(P) : YojsonMarshalableS with type t := t)
end
type keypair = Keypair.t
[@@deriving rlp]

(* Used only for tests *)
let make_public_key public_key_string =
  let len = String.length public_key_string in
  if len <> public_key_length then
    bork "Bad public key length %d for %s" len (unparse_coloned_hex_string public_key_string);
  let public_key_buffer = Cstruct.to_bigarray (Cstruct.of_string public_key_string) in
  match Secp256k1.Key.read_pk secp256k1_ctx public_key_buffer with
  | Ok pk -> pk
  | Error msg -> bork "%s" msg

(* Used only for tests *)
let make_private_key private_key_string =
  let len = String.length private_key_string in
  if len <> private_key_length then
    bork "Bad private key length %d for %s" len (unparse_coloned_hex_string private_key_string);
  let private_key_buffer = Cstruct.to_bigarray (Cstruct.of_string private_key_string) in
  match Secp256k1.Key.read_sk secp256k1_ctx private_key_buffer with
  | Ok sk -> sk
  | Error msg -> bork "%s" msg

type 'a signed = {payload: 'a; signature: signature}
[@@deriving rlp]

(* Used only for tests *)
let make_keypair private_key_string public_key_string password =
  let public_key = make_public_key public_key_string in
  let private_key = make_private_key private_key_string in
  let address = address_of_public_key public_key in
  Keypair.{address; public_key; private_key; password}

(* Used only for tests *)
let keypair_of_0x private_key_0x public_key_0x password =
  make_keypair (parse_0x_data private_key_0x) (parse_0x_data public_key_0x) password

(* TODO: handle collisions, exceptions *)
let address_by_nickname = Hashtbl.create 8
let nickname_by_address = Hashtbl.create 8
let register_address nickname address =
  Hashtbl.replace nickname_by_address address nickname;
  Hashtbl.replace address_by_nickname nickname address
let get_nickname_of_address address =
  match Hashtbl.find_opt nickname_by_address address with
  | Some x -> Ok x
  | None -> OrExn.bork "No registered nickname for address %s" (Address.to_0x address)
let nickname_of_address = get_nickname_of_address >> OrExn.get
let get_address_of_nickname nickname =
  match Hashtbl.find_opt address_by_nickname nickname with
  | Some x -> Ok x
  | None -> OrExn.bork "No registered nickname %S" nickname
let address_of_nickname = get_address_of_nickname >> OrExn.get
let nicknamed_string_of_address address =
  let s = address |> Address.to_0x in
  match (get_nickname_of_address address) with
  | Ok nickname -> Printf.sprintf "%s (%s)" s nickname
  | Error _ -> s
let unregister_address nickname =
  let address = address_of_nickname nickname in
  Hashtbl.remove address_by_nickname nickname;
  Hashtbl.remove nickname_by_address address

let keypair_by_address = Hashtbl.create 8
let register_keypair nickname keypair =
  let address = keypair.Keypair.address in
  Hashtbl.replace keypair_by_address address keypair;
  register_address nickname address
let unregister_keypair nickname =
  let address = address_of_nickname nickname in
  Hashtbl.remove keypair_by_address address;
  unregister_address nickname
let get_keypair_of_address address =
  match Hashtbl.find_opt keypair_by_address address with
  | Some x -> Ok x
  | None -> OrExn.bork "No registered keypair for address %s" (Address.to_0x address)
let keypair_of_address = get_keypair_of_address >> OrExn.get

let decode_keypairs =
  YoJson.to_assoc
  >> List.map (fun (name, kpjson) -> (name, (Keypair.of_yojson_exn kpjson)))

(** TODO: Add a layer of encryption for these files. *)
let register_file_keypairs file =
  Yojsoning.yojson_of_file file
  |> decode_keypairs
  |> List.iter (uncurry register_keypair)

let addresses_with_registered_keypair () : address list =
  Hashtbl.fold (fun k _ l -> k :: l) keypair_by_address []

let nicknames_with_registered_keypair () : string list =
  addresses_with_registered_keypair ()
  |> List.map (get_nickname_of_address >> function Ok x -> [x] | Error _ -> [])
  |> List.flatten

(* convert OCaml string of suitable length (32 only?) to Secp256k1 msg format
   for strings representing hashes, the msg format is suitable for signing.
*)
let secp256k1_msg_of_string (s : string) : Secp256k1.Sign.msg =
  let buffer = Cstruct.to_bigarray (Cstruct.of_string s) in
  match Secp256k1.Sign.msg_of_bytes buffer with
  | Some msg -> msg
  | None -> bork "Could not create SECP256K1.Sign.msg from string"

(* convert a Digest.`t to a Secp256k1 msg representing the digest *)
let secp256k1_msg_of_digest (digest : Digest.t) : Secp256k1.Sign.msg =
  secp256k1_msg_of_string (Digest.to_big_endian_bits digest)

(* digital signature is encrypted hash *)
let make_signature make_digest private_key data =
  (* change representation of data to use Secp256k1 signing *)
  let secp256k1_msg = secp256k1_msg_of_digest (make_digest data) in
  match Secp256k1.Sign.sign_recoverable secp256k1_ctx ~sk:private_key secp256k1_msg with
  | Ok signature -> signature
  (* For debugging what signatures are being made:
     |> fun s -> Printf.printf "MADE sig %s <- digest %s privkey %s\n%!" (signature |> string_of_signature |> unparse_hex_string) (Digest.to_hex_string (make_digest data)) (unparse_hex_string (Bytes.to_string (bytes_of_key private_key))) ; s *)
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

let public_key_of_signature signature digest =
  let msg = secp256k1_msg_of_digest digest in
  Secp256k1.Sign.recover secp256k1_ctx ~msg ~signature

let address_of_signature signature digest =
  public_key_of_signature signature digest |> Result.map address_of_public_key
(* For debugging what signatures are being checked:
   |> Result.map (fun a -> Printf.printf "RECO sig %s digest %s -> addr %s\n%!" (signature |> string_of_signature |> unparse_hex_string) (Digest.to_hex_string digest) (Address.to_hex_string a) ; a) *)

(* check validity of signature for data *)
let is_signature_valid (make_digest : 'a->Digest.t) (address: Address.t) (signature: Signature.t) (data : 'a) : bool =
  match address_of_signature signature (make_digest data) with
  | Ok signer -> Address.equal signer address
  | Error _ -> false

(* check validity of signature for payload within signed value *)
let is_signed_value_valid make_digest address signed_value =
  is_signature_valid make_digest address signed_value.signature signed_value.payload

let signed (make_digest : 'a->digest) (private_key : private_key) (data : 'a) : 'a signed =
  {payload= data; signature= make_signature make_digest private_key data}

let marshal_signed marshal buffer {payload; signature} =
  marshal buffer payload; Signature.marshal buffer signature

let unmarshal_signed (unmarshal:'a unmarshaler) start bytes : 'a signed * int =
  let payload,payload_offset = unmarshal start bytes in
  let signature,final_offset = Signature.unmarshal payload_offset bytes in
  ({payload; signature}, final_offset)

let signed_marshaling marshaling =
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

let signed_yojsoning yojsoning =
  { to_yojson= signed_to_yojson yojsoning.to_yojson
  ; of_yojson= signed_of_yojson yojsoning.of_yojson }

let signed_of_digest (digest : 'a->digest) (kp : keypair) = signed digest kp.Keypair.private_key

module type SignedS = sig
  type payload
  include PersistableS with type t = payload signed
  val make : keypair -> payload -> t
end

module Signed (P : PersistableS) = struct
  type payload = P.t
  module Pre = struct
    type t = payload signed
    let yojsoning = signed_yojsoning P.yojsoning
    let marshaling = signed_marshaling P.marshaling
    let walk_dependencies _methods context x =
      walk_dependency P.dependency_walking context x.payload
    let make_persistent = normal_persistent
  end
  include Persistable(Pre)
  let make = signed_of_digest P.digest
end

module Test = struct
  open Lib.Test

  let trent_keys =
    keypair_of_0x
      "0xb6fb0b7e61363ee2f748161338f56953e8aa42642e9990eff17e7de9aa895786"
      "0x0426bd9885f2c9e23d18c3025da70e71a4f7ce237124352882eafbd1cbb1e9742c4fe3847ce1a56a0d19df7a7d385a2134be05208b5d1ccc5d015f5e9a3ba0d7df"
      ""
  let trent_address = trent_keys.address

  let alice_keys =
    keypair_of_0x
      "0xfdc8f15b2dd9229b0b9246094393afc23b3b705c07e674f6cb614120d1627818"
      "0x045562695c85f88f6cbaec121d2a3da6666c5dc8540d86358bd569a1882bbe6ddcf45b76f5643133939c8e7a339947ca1b115290d577343023d79c256dbc54bc97"
      ""
  let alice_address = alice_keys.address

  let bob_keys =
    keypair_of_0x
      "0x9b21b9b06ba77824b8ba6a815f5a075229a708ae88ba7fd935c968fe2c3df172"
      "0x049e0a7e3c05e3328c603b0c27fbfdfc5030c95d9ad179a431c14f81e30a64ce95f625447e182a8be718d45f9ab9723f9b8571dd5c5752daa66feb84938b095805"
      ""

  let bob_address = bob_keys.address

  let register_test_keypairs () =
    List.iter (uncurry register_keypair)
      ["Alice", alice_keys; "Trent", trent_keys; "Bob", bob_keys]

  (* test validity of digital signatures *)

  let%test "register keypairs" =
    register_test_keypairs (); true

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

  (* test that addresses are really last 20 bytes of Keccak256 hash of public keys *)
  let%test "alice_address_from_public_key" =
    Address.to_0x alice_keys.address = "0xc54e86dffb87b9736e2e35dd85c775358f1c31ce"

  let%test "bob_address_from_public_key" =
    expect_string "bob address"
      "0x9ccaed210ce8c0cb49c5ad1c4f583406c264ba69"
      (Address.to_0x bob_address);
    true

  let%test "trent_address_from_public_key" =
    expect_string "trent address"
      "0xf47408143d327e4bc6a87ef4a70a4e0af09b9a1c"
      (Address.to_0x trent_address);
    true

  let%test "trent_of_address" =
    keypair_of_address trent_address = trent_keys
end

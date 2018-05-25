(* legibase.ml -- base module for Legicash *)

open Lib

exception Timeout of string

exception Double_spend of string

type 'output legi_result = ('output, exn) result

type ('input, 'output, 'state) action = 'state * 'input -> 'state * 'output legi_result

(** run the action, with side-effects and all *)
let effect_action action state_ref x =
  match action (!state_ref, x) with
  | s, Ok y ->
      state_ref := s ;
      y
  | s, Error e ->
      state_ref := s ;
      raise e

let no_action (s, a) = (s, Ok a)

let fail_action failure (s, _) = (s, Error failure)

(** compose two actions *)
let compose_actions c_of_b b_of_a (s, a) =
  match b_of_a (s, a) with t, Error e -> (t, Error e) | t, Ok b -> c_of_b (t, b)

(* TODO: Infix syntax for action_seq, etc. *)

(** compose a list of actions *)
let compose_action_list action_list (s, a) =
  let rec loop action_list (s, a) =
    match action_list with
    | [] -> (s, Ok a)
    | action :: more_actions ->
      match action (s, a) with t, Ok b -> loop more_actions (t, b) | (t, Error e) as x -> x
  in
  loop action_list (s, a)

let do_action (state, value) action = action (state, value)

let ( ^|> ) = do_action

(* Same as |> !!!!*)

let action_seq action1 action2 = compose_actions action2 action1

let ( ^>> ) = action_seq

exception Assertion_failed

let action_assert pure_action (state, value) =
  if pure_action (state, value) then (state, Ok value) else (state, Error Assertion_failed)

type ('input, 'output, 'state) pure_action = 'state * 'input -> 'output

let action_of_pure_action f (state, value) = (state, Ok (f (state, value)))

(** compose two pure actions *)
let compose_pure_actions c_of_b b_of_a (s, a) = c_of_b (s, b_of_a (s, a))

let pure_action_seq b_of_a c_of_b (s, a) = compose_pure_actions c_of_b b_of_a (s, a)

(* create context just once, because expensive operation; assumes
   single instantiation of this module
 *)
let secp256k1_ctx = Secp256k1.Context.create [Sign; Verify]

module Address : sig
  type t [@@deriving show]

  val of_public_key : Secp256k1.Key.public Secp256k1.Key.t -> t

  val compare : t -> t -> int

  val of_string : string -> t

  val to_string : t -> string

  val equal : t -> t -> bool
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
end

(** unique identifier for all parties, that is, customers and facilitators *)
type public_key = Secp256k1.Key.public Secp256k1.Key.t

(** private key in public-key cryptography *)
type private_key = Secp256k1.Key.secret Secp256k1.Key.t

(*module Int256 : Int with type t = Z.t : sig
end*)

type 'a signature = Secp256k1.Sign.recoverable Secp256k1.Sign.t

type 'a signed = {payload: 'a; signature: 'a signature}

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

module Digest = struct
  include Data256

  let make v =
    let data_string = Marshal.to_string v [Marshal.Compat_32] in
    let hash = Cryptokit.Hash.keccak 256 in
    of_string (Cryptokit.hash_string hash data_string)
end

type 'a digest = Digest.t

(** Special magic digest for None. A bit ugly. *)
let null_digest = Digest.zero

module DigestSet = struct
  include Set.Make (Digest)

  let lens k = Lens.{get= mem k; set= (fun b -> if b then add k else remove k)}
end

module Revision = Unsigned.UInt64
module Duration = Unsigned.UInt64
module Timestamp = Unsigned.UInt64

type conversation

(** A pure mapping from 'a to 'b suitable for use in interactive merkle proofs
    Let's cheat for now.
    TODO: Tezos must have something we should use.
    probably Tezos_crypto.S.MERKLE_TREE or Tezos_crypto.Blake2B.Make_merkle_tree
 *)

(* maps with lenses and defaults *)

module type MapS = sig
  include Map.S

  val lens : key -> ('a t, 'a) Lens.t

  val find_defaulting : (unit -> 'a) -> key -> 'a t -> 'a
end

module MapMake (Key : Map.OrderedType) = struct
  include Map.Make (Key)

  let lens k = Lens.{get= find k; set= add k}

  let find_defaulting default k m = defaulting default (find_opt k m)
end

let defaulting_lens default lens =
  Lens.{get= (fun x -> try lens.get x with Not_found -> default ()); set= lens.set}

module AddressMap = MapMake (Address)
module RevisionMap = MapMake (Revision)

module Test = struct
  (* test digests *)
  let mk_digest_test data expected =
    let digest = Digest.make data in
    expected = unparse_hex (Digest.to_string digest)

  let%test "digest_1" =
    mk_digest_test "this is a test"
      "d5:02:39:01:b6:e1:b3:fd:03:54:3a:a1:ee:40:3b:77:36:a9:08:5a:b0:4e:71:a0:47:d4:5b:2a:57:7f:72:e8"

  let%test "digest_2" =
    mk_digest_test (Some "nonsense")
      "e2:9d:d9:ae:ca:d9:44:3b:f6:ea:17:3d:70:57:d3:22:1c:97:cb:94:1a:c9:aa:93:86:ab:ed:ac:e7:16:88:d0"

  let%test "digest_3" =
    mk_digest_test Int64.one
      "c6:c6:80:47:7d:5c:20:cd:35:1e:ab:56:54:05:85:3a:9f:09:00:f4:93:d0:3e:c4:e5:72:c6:f5:98:53:41:83"

  let%test "digest_4" =
    mk_digest_test [99.9; 100.4; 22.0; 1033.7]
      "f4:d7:ee:d0:ed:86:14:cf:aa:4c:f1:af:0f:f5:dc:23:45:a4:a6:62:d5:aa:57:ed:7a:9b:f4:75:94:50:65:4a"
end

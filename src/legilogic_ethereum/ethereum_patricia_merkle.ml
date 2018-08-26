(* ethereum_patricial_merkle.ml -- Ethereum Patricia Merkle tries *)
(* references:

   https://github.com/ethereum/wiki/wiki/Patricia-Tree
   https://easythereentropy.wordpress.com/2014/06/04/understanding-the-ethereum-trie/
   https://github.com/ebuchman/understanding_ethereum_trie
   https://github.com/ethereum/pyethereum
   https://github.com/ethereum/go-ethereum/tree/master/trie

*)

open Legilogic_lib
open Lib
open Hex
open Digesting

(* keys are sequences of nybbles, which can be packed into two-nybbles-per-byte form *)
module Key : sig
  module Unpacked : sig
    type t

    val empty_key : t

    val drop : int -> t -> t

    val split : t -> int * t

    val remove_terminator : t -> t

    val add_terminator : t -> t

    val suffix_if_prefix : t -> t -> t option

    val get : t -> int -> char

    val of_string : string -> t

    val to_string : t -> string
  end

  module Packed : sig
    type t

    val to_string : t -> string
  end

  val nybble_terminator_val : int

  val nybble_terminator : char

  val nybble_terminator_string : string

  val pack : Unpacked.t -> Packed.t

  val unpack : Packed.t -> Unpacked.t
end = struct
  (* distinguished from actual nybble, because too big for a nybble *)
  let nybble_terminator_val = 0x10

  let nybble_terminator = Char.chr nybble_terminator_val

  let nybble_terminator_string = String.make 1 nybble_terminator

  module Unpacked = struct
    (* each byte represents a hex digit in the key, notionally a nybble *)
    type t = String.t

    let empty_key = ""

    (* drop n nybbles in key *)
    let drop n key =
      if n > String.length key then
        bork "drop_key: number of nybbles to drop greater than key length" ;
      String.sub key n (String.length key - n)


    (* split key into first nybble, rest *)
    let split key =
      if key = empty_key then bork "next_key: got empty key" ;
      (Char.code key.[0], drop 1 key)


    (* remove terminator, if any, from key *)
    let remove_terminator key =
      let len = String.length key in
      if key.[len - 1] = nybble_terminator then String.sub key 0 (len - 1) else key


    (* add terminator to key, if needed *)
    let add_terminator key =
      if key = empty_key || key.[String.length key - 1] != nybble_terminator then
        key ^ nybble_terminator_string
      else key


    (* if part key a prefix of full key, return remaining part *)
    let suffix_if_prefix full_key part_key =
      let full_len = String.length full_key in
      let part_len = String.length part_key in
      if part_len > full_len then None
      else if String.sub full_key 0 part_len = part_key then
        Some (String.sub full_key part_len (full_len - part_len))
      else None


    let get key ndx = key.[ndx]

    let of_string s =
      let len = String.length s in
      let _ =
        String.iteri
          (fun ndx c ->
             let c_val = Char.code c in
             if ndx = len - 1 then (
               if c_val > nybble_terminator_val then
                 bork "Unpacked.of_string: final character must be less than or equal to nybble terminator")
             else if c_val > 0x0F then
               bork "Unpacked.of_string: character must be less than or equal to 0x0F")
          s
      in
      s


    let to_string s = s
  end

  module Packed = struct
    type t = String.t

    let to_string s = s
  end

  (* switch between unpacked, packed representations *)
  let pack unpacked =
    let len = String.length unpacked in
    let nybbles0, flags0 =
      if len > 0 && unpacked.[len - 1] = nybble_terminator then (String.sub unpacked 0 (len - 1), 2)
      else (unpacked, 0)
    in
    let odd_len = String.length nybbles0 mod 2 in
    let flags = flags0 lor odd_len in
    let nybbles =
      if odd_len = 0 then String.make 1 (Char.chr flags) ^ "\000" ^ nybbles0
      else String.make 1 (Char.chr flags) ^ nybbles0
    in
    String.init
      (String.length nybbles / 2)
      (fun ndx ->
         let nybbles_ndx = ndx * 2 in
         Char.chr (Char.code nybbles.[nybbles_ndx] lsl 4 + Char.code nybbles.[nybbles_ndx + 1]) )


  let unpack packed =
    let unpacked0 =
      String.init
        (2 * String.length packed)
        (fun ndx ->
           let byte = Char.code packed.[ndx / 2] in
           (* take high nybble if even ndx, low nybble if odd *)
           if ndx mod 2 = 0 then Char.chr ((byte land 0xF0) lsr 4) else Char.chr (byte land 0x0F) )
    in
    let flags = Char.code unpacked0.[0] in
    let unpacked1 =
      if flags land 0x02 = 0 then unpacked0 else unpacked0 ^ nybble_terminator_string
    in
    let unpacked =
      let len = String.length unpacked1 in
      let odd_len = flags land 0x01 in
      if odd_len = 1 then String.sub unpacked1 1 (len - 1) else String.sub unpacked1 2 (len - 2)
    in
    unpacked
end

type hash = Digest.t

(* produced with Keccak256 *)

type 'a node =
  | BlankNode
  (* the array of subnodes is of length 16, indexed by key hex chars; 'a is a value *)
  | BranchNode of 'a node_or_hash array * 'a
  | LeafNode of (Key.Packed.t * 'a)
  | ExtensionNode of (Key.Packed.t * 'a node_or_hash)

and 'a node_or_hash = Hash of hash | Node of 'a node

type 'a ethereum_patricia_merkle_trie =
  {root: 'a node; root_hash: hash; db: (hash, 'a node) Hashtbl.t}

(* format a key for LeafOrExtensionNode *)
let pad_key_for_leaf_or_extension terminator_bit key =
  let even = Bytes.length key mod 2 = 0 in
  (* if even is false, we turn ON parity bit, the least significant bit *)
  let nybble0 = if even then 0b10 else 0b11 in
  let nybble = nybble0 lor terminator_bit in
  (* if even, add 0 nybble by left-shifting *)
  let byte_char = Char.chr (if even then nybble lsl 4 else nybble) in
  let byte = Bytes.make 1 byte_char in
  Bytes.cat byte key


(* encode key in for leaf node *)
let key_to_leaf_key key = pad_key_for_leaf_or_extension 0b10 key

(* encode key in for extension node *)
let key_to_extension_key key = pad_key_for_leaf_or_extension 0b00 key

let hash_encoded_string s =
  let open Ethereum_rlp in
  let encoded = to_string (encode_string s) in
  Digest.of_big_endian_bits (Cryptokit.hash_string (Cryptokit.Hash.keccak 256) encoded)


(* hash of RLP-coding of empty string *)
let init_root_hash = hash_encoded_string ""

let create () = {root= BlankNode; root_hash= init_root_hash; db= Hashtbl.create 23}

(* return a node from a node-or-hash *)
let decode_to_node (trie: 'a ethereum_patricia_merkle_trie) (node_or_hash: 'a node_or_hash) =
  match node_or_hash with
  | Node node -> node
  | Hash hash ->
    (* TODO: in Pythereum, there's an RLP-decode step
       there will need to be a way to convert trie to RLPItem(s), and back, probably
    *)
    Hashtbl.find trie.db hash


(* lookup key in a node *)
let get (trie: 'a ethereum_patricia_merkle_trie) (node: 'a node) (key: Key.Unpacked.t) : 'a option =
  let open Key in
  let rec get_loop (node: 'a node) (key: Unpacked.t) : 'a option =
    match node with
    | BlankNode -> None
    | BranchNode (subnodes, v) ->
      if key = Unpacked.empty_key then (* at target node *)
        Some v
      else
        (* get sub node given by first nybble in key *)
        let nybble, subkey = Unpacked.split key in
        let subnode = decode_to_node trie subnodes.(nybble) in
        get_loop subnode subkey
    | LeafNode (k, v) ->
      let curr_key = Unpacked.remove_terminator (unpack k) in
      if key = curr_key then Some v else None
    | ExtensionNode (k, node_or_hash) ->
      let curr_key = Unpacked.remove_terminator (unpack k) in
      match Unpacked.suffix_if_prefix key curr_key with
      | Some key_suffix ->
        let subnode = decode_to_node trie node_or_hash in
        get_loop subnode key_suffix
      | None -> None
  in
  get_loop node key


module Test = struct
  (* tests of hashing of RLP-encoded data *)

  let make_hash_test s hex =
    unparse_coloned_hex_string (Digest.to_big_endian_bits (hash_encoded_string s)) = hex

  let%test "hash_test_1" =
    make_hash_test ""
      "56:e8:1f:17:1b:cc:55:a6:ff:83:45:e6:92:c0:f8:6e:5b:48:e0:1b:99:6c:ad:c0:01:62:2f:b5:e3:63:b4:21"

  let%test "hash_test_2" =
    make_hash_test "this is a test of the hash kind"
      "ef:8e:91:f7:12:bb:3e:d9:24:8b:21:70:11:70:5d:28:0d:aa:6b:8e:9b:93:ce:9b:10:ba:0a:8b:90:15:c7:70"

  let%test "hash_test_3" =
    make_hash_test
      "a long kind of string a long kind of string a long kind of string a long kind of string a long kind of string a long kind of string a long kind of string a long kind of string"
      "61:fd:7f:86:4e:2c:3c:0e:f4:2c:0c:1e:3e:fc:8e:77:9c:c7:97:97:1e:c1:35:f2:85:a0:f7:54:50:53:12:a7"

  (* test that nybble packing and unpacking are inverses *)

  let make_unpack_pack_test s =
    let open Key in
    let unpacked = Unpacked.of_string s in
    let packed = pack unpacked in
    let reunpacked = unpack packed in
    reunpacked = unpacked


  (* N%test.B.: each byte must between 0x00 and 0x0F, except final byte may have nybble terminator *)

  let%test "pack_unpack_1" = make_unpack_pack_test "\000\000\000\000\000\000\000\000\000\000"

  let%test "pack_unpack_2" = make_unpack_pack_test "\005\005\005\005\005\005\005\005\005\005"

  let%test "pack_unpack_3" = make_unpack_pack_test "\n\011\012\002"

  let%test "pack_unpack_4" = make_unpack_pack_test "\n\015\r\000"

  let%test "pack_unpack_5" = make_unpack_pack_test "\n\015\014\015\003\002"

  let%test "pack_unpack_5" = make_unpack_pack_test ("\n\015\014\015\003" ^ Key.nybble_terminator_string)

  let%test "pack_unpack_6" = make_unpack_pack_test ""
end

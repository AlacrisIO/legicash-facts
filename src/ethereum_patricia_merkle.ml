(* ethereum_patricial_merkle.ml -- Ethereum Patricia Merkle trees *)

(* references:

    htpps://github.com/ethereum/wiki/wiki/Patricia-Tree
    https://easythereentropy.wordpress.com/2014/06/04/understanding-the-ethereum-trie/
    https://github.com/ebuchman/understanding_ethereum_trie
    https://github.com/ethereum/pyethereum
    https://github.com/ethereum/go-ethereum/tree/master/trie

 *)

type key = Bytes.t (* each byte represents a hex digit in the key, notionally a nybble *)

type hash = Data256.t (* produced with Keccak256 *)

type 'a hash_or_value =
  | Hash of hash
  | Value of 'a

(* 'a value type *)
type 'a node =
  | BlankNode
  (* key format indicates whether value is hash or value *)
  (* a hash refers to a node in the database *)
  | LeafOrExtensionNode of (key * 'a hash_or_value) list
  (* the key has length 16, representing 16 hex chars in a key *)
  | BranchNode of key * 'a option

type 'a ethereum_patricia_merkle_tree =
  { root : 'a node
  ; root_hash : hash
  ; db : (key,'a node) Hashtbl.t
  }

(* format a key for LeafOrExtensionNode *)
let pad_key_for_leaf_or_extension terminator_bit key =
  let even = Bytes.length key % 2 = 0 in
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

(* hash of RLP-coding of empty string *)
let init_root_hash =
  let rlp_coding = rlp "" in
  Cryptokit.hash_string (Cryptokit.Hash.keccak 256) rlp_coding

let create () =
  { root = BlankNode
  ; root_hash = init_root_hash
  ; db = Hashtbl.create 23
  }

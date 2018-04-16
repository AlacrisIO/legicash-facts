exception Not_implemented

open Z

let bottom : 'a -> 'b = fun _ -> raise Not_implemented

let list_of_option = function None -> [] | Some x -> [x]
(* TODO: find which is canonical according to the style guide between this and
let list_of_option x = match x with None -> [] | Some x -> [x]
  and/or define a new style guide rule with motivation.
 *)

(** A pure mapping from 'a to 'b suitable for use in interactive merkle proofs
    Let's cheat for now.
    TODO: Tezos must have something we should use.
    probably Tezos_crypto.S.MERKLE_TREE or Tezos_crypto.Blake2B.Make_merkle_tree
 *)
type ('a, 'b) patricia_merkle_trie = ('a, 'b) Hashtbl.t

module PublicKeyMap = Map.Make(Key256)
module Int64Map = Map.Make(Int64)
(*Lib_crypto.Blake2B.Make_merkle_tree something?*)

(* module Int64Utils = *)

let is_odd_64 x = (Int64.logand x Int64.one) == Int64.one

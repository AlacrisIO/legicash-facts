exception Not_implemented

let bottom () : 'a = raise Not_implemented

let list_of_option = function None -> [] | Some x -> [x]
(* TODO: find which is canonical according to the style guide between this and
let list_of_option x = match x with None -> [] | Some x -> [x]
  and/or define a new style guide rule with motivation.
 *)

(** Let's cheat for now
    Tezos has something we should use
    *)
type ('a, 'b) patricia_merkle_trie = ('a, 'b) Hashtbl.t

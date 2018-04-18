(* 256-bit keys in Legicash.
   TODO: rename to data256 ?
 *)

type t = char array

exception Invalid_key_length

let key_bytes = 32  (* 8 x 32 = 256 *)
let elt_bound = 256 (* bound on chars *)

let of_list vs =
  let validate () =
    if List.length vs <> key_bytes then
      raise Invalid_key_length
  in
  let _ = validate () in
  Array.of_list vs

let of_array vs =
  let validate () =
    if Array.length vs <> key_bytes then
      raise Invalid_key_length
  in
  let _ = validate () in
  vs

let generate () =
  let rec build_vs n vs =
    if n = 0 then vs
    else
      let v = Char.chr (Random.int elt_bound) in
      build_vs (n - 1) (v::vs)
  in
  let _ = Random.self_init () in
  let vs = build_vs key_bytes [] in
  Array.of_list vs

let compare pk1 pk2 = Pervasives.compare pk1 pk2

let zero = of_array (Array.make key_bytes '\000')

(** TODO: fix me *)
let one = of_array (Array.make key_bytes '\001')

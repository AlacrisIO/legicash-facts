exception Not_implemented

open Z

let bottom : 'a -> 'b = fun _ -> raise Not_implemented

let list_of_option = function None -> [] | Some x -> [x]
(* TODO: find which is canonical according to the style guide between this and
let list_of_option x = match x with None -> [] | Some x -> [x]
  and/or define a new style guide rule with motivation.
 *)

let is_odd_64 x = (Int64.logand x Int64.one) == Int64.one

let constantly x _ = x

let option_map f = function
  | Some x -> Some (f x)
  | None -> None

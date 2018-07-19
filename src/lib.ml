(** Common exceptions *)
exception Not_implemented

exception Internal_error of string

let bottom : 'a -> 'b = fun _ -> raise Not_implemented

let throws exn thunk =
  try ignore (thunk ()) ; false with x -> x = exn

(** SKI TZ combinators *)
let identity x = x

let konstant x _y = x

let schoenfinkel x y z = x z (y z)

let transpose x y z = x z y

let zcompose x y z = x (y z)


(** Options *)
let defaulting default = function None -> default () | Some x -> x

let option_get = function None -> raise Not_found | Some x -> x

let is_option_some = function None -> false | Some _ -> true

let list_of_option = function None -> [] | Some x -> [x]

(** TODO: find which is canonical according to the style guide between this and
    let list_of_option x = match x with None -> [] | Some x -> [x]
    and/or define a new style guide rule with motivation.
*)
let option_map f = function Some x -> Some (f x) | None -> None


(* Hexadecimal *)

(** Char code for lower case a *)
let a_code = Char.code 'a'

(** Char code for upper case A *)
let big_a_code = Char.code 'A'

(** Char code for 0 *)
let zero_code = Char.code '0'

let int_of_hex_char hex =
  let hex_code = Char.code hex in
  match hex with
  | '0'..'9' -> hex_code - zero_code (* assume ASCII / UTF-8 *)
  | 'a'..'f' -> hex_code - a_code + 0xa
  | 'A'..'F' -> hex_code - big_a_code + 0xa
  | _ -> raise (Internal_error (Printf.sprintf "Invalid hex character %c" hex))

let parse_hex_nibble string pos =
  int_of_hex_char string.[pos]

let parse_hex_byte string pos =
  (parse_hex_nibble string pos) lsl 4 + parse_hex_nibble string (pos + 1)

let parse_hex_substring string pos len =
  let start_pos = pos - (len mod 2) in
  String.init
    ((len + 1) / 2)
    (fun i ->
       let p = start_pos + 2 * i in
       let single_digit = p < pos in
       Char.chr
         (if single_digit then
            parse_hex_nibble string (p + 1)
          else
            parse_hex_byte string p))

let parse_hex_string string = parse_hex_substring string 0 (String.length string)

let parse_coloned_hex_string hex_string =
  let invalid () = raise (Internal_error (Printf.sprintf "Not a valid hex string: %s" hex_string)) in
  let hex_len = String.length hex_string in
  if hex_len > 0 && (hex_len + 1) mod 3 <> 0 then invalid () ;
  let len = (hex_len + 1) / 3 in
  let parse_char i =
    let offset = i * 3 in
    if offset > 0 && hex_string.[offset - 1] != ':' then invalid () ;
    Char.chr (parse_hex_byte hex_string offset)
  in
  String.init len parse_char

let hex_char_of_int ?(upper_case = false) digit =
  if (digit < 0 || digit > 16) then
    raise (Internal_error (Printf.sprintf "Invalid hex digit %d" digit))
  else
    Char.chr
      (if digit < 10 then
         zero_code + digit
       else if upper_case then
         big_a_code - 10 + digit
       else
         a_code - 10 + digit)

let hex_digit_of_string string start index =
  let byte = Char.code string.[start+(index lsr 1)] in
  if index mod 2 = 0 then byte lsr 4 else byte mod 16

let unparse_hex_substring string pos len =
  String.init (len lsl 1) (fun i -> hex_char_of_int (hex_digit_of_string string pos i))

let unparse_hex_string string =
  unparse_hex_substring string 0 (String.length string)

let unparse_coloned_hex_string string =
  String.concat ":" (List.init (String.length string) (fun i -> unparse_hex_substring string i 1))


module type T = sig
  type t
end

(** Interface analogous to Map.S from the stdlib, but monomorphic in value *)
module type MapS = sig
  type key
  type value
  type t

  (* Constructing a map *)
  val empty: t
  val add: key -> value -> t -> t
  val remove: key -> t -> t
  val singleton: key -> value -> t

  (* Consulting a map *)
  val is_empty: t -> bool
  val mem: key -> t -> bool
  val find: key -> t -> value
  val find_opt: key -> t -> value option
  val find_first: (key -> bool) -> t -> key * value
  val find_first_opt: (key -> bool) -> t -> (key * value) option
  val find_last: (key -> bool) -> t -> key * value
  val find_last_opt: (key -> bool) -> t -> (key * value) option
  val bindings: t -> (key * value) list
  val of_bindings: (key * value) list -> t
  val min_binding: t -> (key * value)
  val min_binding_opt: t -> (key * value) option
  val max_binding: t -> (key * value)
  val max_binding_opt: t -> (key * value) option
  val choose: t -> (key * value)
  val choose_opt: t -> (key * value) option

  val cardinal: t -> int

  (* Iterating over a map *)
  val iter: (key -> value -> unit) -> t -> unit
  val fold: (key -> value -> 'r -> 'r) -> t -> 'r -> 'r
  val for_all: (key -> value -> bool) -> t -> bool
  val exists: (key -> value -> bool) -> t -> bool
  val filter: (key -> value -> bool) -> t -> t

  (** General variant of fold-left in CPS *)
  val foldlk : (key -> value -> 'acc -> ('acc -> 'res) -> 'res) -> t -> 'acc -> ('acc -> 'res) -> 'res

  (** General variant of fold-right in continuation-passing style *)
  val foldrk : (key -> value -> 'acc -> ('acc -> 'res) -> 'res) -> t -> 'acc -> ('acc -> 'res) -> 'res

  (** fold in reverse order (top index to bottom index), fold right *)
  val fold_right : (key -> value -> 'acc -> 'acc) -> t -> 'acc -> 'acc

  (** Zipping through a Map *)
  type (+'a) step
  val step_map : ('a -> 'b) -> 'a step -> 'b step
  type (+'a) path
  val path_map: ('a -> 'b) -> 'a path -> 'b path

  (* Flesh it out?
     type (+'a) unstep
     type (+'a) costep
     val step_apply : 'a unstep -> 'a step -> ('a * 'a costep) -> ('a * 'a costep)
     val path_apply : 'a unstep -> 'a path -> ('a * 'a costep) -> ('a * 'a costep)
  *)

  exception Inconsistent_path

  (** a zipper is a pair of a focused submap and a path,
      from which to retrieve the complete map *)
  type zipper = t * t path

  (** given a map, return the zipper for the top of map *)
  val zip : t -> zipper

  (** apply a path to a focused submap to retrive the complete map *)
  val unzip : zipper -> t

  (** Given a focus on a subtrie, return focuses on the next level of subtries
      TODO: also return a (t list -> zipper) to reconstruct the zipper from the next submaps?
  *)
  val next: zipper -> zipper list

  (** Focus on the closest sub map of a map that matches given index *)
  val find_path : key -> t -> zipper

  (* Modifying a map
     NB: unlike the corresponding standard library operations, these are not polymorphic
     in the second value types, because that would require more module scaffolding :-/ *)
  val update: key -> (value option -> value option) -> t -> t
  val map: (value -> value) -> t -> t
  val mapi: (key -> value -> value) -> t -> t

  val mapiopt : (key -> value -> value option) -> t -> t
  (** Variant of map that takes key into account and allows for element removal *)

  (* Binary operations on maps.
     NB: unlike the corresponding standard library operations, merge and union are not polymorphic
     in the value types, because that would require more module scaffolding :-/ *)
  val merge: (key -> value option -> value option -> value option) -> t -> t -> t
  val union: (key -> value -> value -> value option) -> t -> t -> t
  val compare: (value -> value -> int) -> t -> t -> int
  val equal: (value -> value -> bool) -> t -> t -> bool

  val iterate_over_tree_pair:
    recursek:(i:key -> treea:t -> treeb:t -> k:('r -> 'o) -> 'o) ->
    branchk:(i:key -> height:int -> leftr:'r -> rightr:'r -> k:('r -> 'o) -> 'o) ->
    skipk:(i:key -> height:int -> length:int -> bits:key -> childr:'r ->
           k:('r -> 'o) -> 'o) ->
    leafk:(i:key -> valuea:value -> valueb:value -> k:('r -> 'o) -> 'o) ->
    onlyak:(i:key -> anode:t -> k:('r -> 'o) -> 'o) ->
    onlybk:(i:key -> bnode:t -> k:('r -> 'o) -> 'o) ->
    i:key -> treea:t -> treeb:t -> k:('r -> 'o) -> 'o

  (* Splitting a map *)
  val partition: (key -> value -> bool) -> t -> t * t
  val split: key -> t -> t * value option * t

  (* 4.07.0 and later
     val to_seq : t -> (key * value) Seq.t
     val to_seq_from : key -> t -> (key * value) Seq.t
     val add_seq : (key * value) Seq.t -> t -> t
     val of_seq : (key * value) Seq.t -> t
  *)

  val lens : key -> (t, value) Lens.t
  val find_defaulting : (unit -> value) -> key -> t -> value
end

let defaulting_lens default lens =
  Lens.{get= (fun x -> try lens.get x with Not_found -> default ()); set= lens.set}

(*let seq_cat a b () = match a () with
  | Nil -> b ()
  | Cons x a' -> Cons x (seq_cat a' b)*)

module type ShowableS = sig
  type t
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

let string_reverse s =
  let len = String.length s in
  String.init len (fun i -> s.[len - i - 1])

module Test = struct
  let%test "hex_string" =
    List.for_all
      (fun (bits, hex) -> unparse_hex_string bits = hex && parse_hex_string hex = bits)
      [("","");("\000","00");("\000\000","0000");
       ("abcd","61626364");("\r\n","0d0a")]
end

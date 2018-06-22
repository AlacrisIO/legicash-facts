(** Common exceptions *)
exception Not_implemented

exception Internal_error of string

let bottom : 'a -> 'b = fun _ -> raise Not_implemented

let throws exn thunk =
  try ignore (thunk ()) ; false with x -> x = exn

(** SKI TZ combinators *)
let identity x = x

let konstant x y = x

let schoenfinkel x y z = x z (y z)

let transpose x y z = x z y

let zcompose x y z = x (y z)


(** Options *)
let defaulting default = function None -> default () | Some x -> x

let unwrap_option = function None -> raise Not_found | Some x -> x

let is_option_some = function None -> false | Some _ -> true

let list_of_option = function None -> [] | Some x -> [x]

(** TODO: find which is canonical according to the style guide between this and
    let list_of_option x = match x with None -> [] | Some x -> [x]
    and/or define a new style guide rule with motivation.
*)
let option_map f = function Some x -> Some (f x) | None -> None

(** Hexadecimal *)
let hex_char_to_int hex =
  let hex_code = Char.code hex in
  match hex with
  | '0'..'9' -> hex_code - Char.code '0' (* assume ASCII / UTF-8 *)
  | 'a'..'f' -> hex_code - Char.code 'a' + Char.code '\n'
  | 'A'..'F' -> hex_code - Char.code 'A' + Char.code '\n'
  | _ -> raise (Internal_error "Invalid hex character")


(* hex strings are of form "nn:nn:...:nn", where nn represents a char as a hex-digit pair *)
let parse_hex hex_string =
  let hex_len = String.length hex_string in
  let _ =
    if hex_len > 0 && (hex_len + 1) mod 3 <> 0 then
      raise (Internal_error (Printf.sprintf "Not a valid hex string: %s" hex_string))
  in
  let len = (hex_len + 1) / 3 in
  let parse_char ndx =
    let offset = ndx * 3 in
    let hi_nybble = hex_char_to_int hex_string.[offset] in
    let lo_nybble = hex_char_to_int hex_string.[offset + 1] in
    Char.chr (hi_nybble lsl 4 + lo_nybble)
  in
  String.init len parse_char


let unparse_hex ?(with_colons=true) s =
  let len = String.length s in
  let rec loop ndx accum =
    if ndx >= len then
      let separator = if with_colons then ":" else "" in
      String.concat separator (List.rev accum)
    else
      let hex = Printf.sprintf "%02x" (Char.code s.[ndx]) in
      loop (ndx + 1) (hex :: accum)
  in
  loop 0 []

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
  val step_apply : (int -> 'a -> 'a -> 'a) -> (int -> int -> key -> 'a -> 'a) ->
    'a step -> ('a*int) -> ('a*int)
  val step_map : ('a -> 'b) -> 'a step -> 'b step
  type (+'a) path
  val path_apply : (int -> 'a -> 'a -> 'a) -> (int -> int -> key -> 'a -> 'a) ->
    'a path -> ('a*int) -> ('a*int)
  val path_map: ('a -> 'b) -> 'a path -> 'b path

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

  (** Generic binary operation on maps, in CPS
      argument: recursek, branchk, skipk, leafk, onlyak, onlybk, base_index, tables, continuation
  *)
  val co_match :
    recursek:(key -> t * t -> ('c -> 'r) -> 'r) ->
    branchk:(key -> int -> 'c -> 'c -> ('c -> 'r) -> 'r) ->
    skipk:(key -> int -> int -> key -> 'c -> ('c -> 'r) -> 'r) ->
    leafk:(key -> value -> value -> ('c -> 'r) -> 'r) ->
    onlyak:(key -> t -> ('c -> 'r) -> 'r) ->
    onlybk:(key -> t -> ('c -> 'r) -> 'r) ->
    key -> t * t -> ('c -> 'r) -> 'r

  (* Splitting a map *)
  val partition: (key -> value -> bool) -> t -> t * t
  val split: key -> t -> t * value option * t

  (* Unimplemented from the standard library's map
     val to_seq : t -> (key * value) Seq.t
     val to_seq_from : key -> t -> (key * value) Seq.t
     val add_seq : (key * value) Seq.t -> t -> t
     val of_seq : (key * value) Seq.t -> t
  *)
end

module StringT = struct
  type t = string
end

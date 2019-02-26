
(** Common exceptions *)
exception Not_implemented

exception Internal_error of string

let () = Printexc.register_printer
           (function
            | Internal_error s -> Some ("Internal_error: " ^ s)
            | _                -> None)

let bork fmt = Printf.ksprintf (fun x -> raise (Internal_error x)) fmt

let bottom : 'a -> 'b = fun _ -> raise Not_implemented

let throws exn thunk =
  try ignore (thunk ()) ; false with x -> x = exn

(** SKI TZ combinators *)
let identity x = x

let konstant x _y = x

let schoenfinkel x y z = x z (y z)

let flip x y z = x z y

let zcompose x y z = x (y z)

let (>>) x y z = y (x z)

let curry f x y = f (x, y)
let uncurry f (x, y) = f x y
let curry3 f x y z = f (x, y, z)
let uncurry3 f (x, y, z) = f x y z
let curry4 f x y z t = f (x, y, z, t)
let uncurry4 f (x, y, z, t) = f x y z t

let pair x y = x, y
let triple x y z = x, y, z
let quadruple x y z t = x, y, z, t

let singleton x = [x]

let make_counter ?(start=0) () =
  let r = ref start in
  fun ?(increment=1) () -> let count = !r in r := count + increment ; count

let int_of_bool b = if b then 1 else 0

(** Options *)
module Option = struct
  type +'a t = 'a option

  let defaulting default = function None -> default () | Some x -> x

  let get = function None -> raise Not_found | Some x -> x

  let is_some = function None -> false | Some _ -> true

  let to_list = function None -> [] | Some x -> [x]

  let return x = Some x
  let bind opt f = match opt with Some x -> f x | None -> None

  (** TODO: find which is canonical according to the style guide between this and
      let list_of_option x = match x with None -> [] | Some x -> [x]
      and/or define a new style guide rule with motivation.
  *)
  let map f = function Some x -> Some (f x) | None -> None

  let iter f = function Some x -> (f x) | None -> ()

  let iter_lwt f = function Some x -> (f x) | None -> Lwt.return_unit
end

let map_fst f (x, y) = (f x, y)

let rec list_foldlk f a l k = match l with
  | [] -> k a
  | h::t -> f a h (fun r -> list_foldlk f r t k)

let list_take n l =
  let rec f a = function
    | (0, _) -> List.rev a
    | (_, []) -> bork "list has fewer than %d items" n
    | (n, h::t) -> f (h::a) (n-1, t) in
  f [] (n, l)

module Result = struct
  type ('ok, 'error) t = ('ok, 'error) result
  let return x = Ok x
  let fail e = Error e
  let bind mx fm = match mx with
    | Ok x -> fm x
    | Error e -> Error e
  let map f = function
    | Ok x -> Ok (f x)
    | Error e -> Error e
  let map_error f = function
    | Ok x -> Ok x
    | Error e -> Error (f e)
  let rec list_map f = function
    | [] -> Ok []
    | x::t -> match f x with
      | Ok y -> map (fun r -> y :: r) (list_map f t)
      | Error e -> Error e
  let get = function
    | Ok x -> x
    | Error _ -> raise Not_found
end

module type TypeS = sig
  type t
end

module type MapS = sig
  type key
  type value
  type t

  val empty: t
  val add: key -> value -> t -> t
  val remove: key -> t -> t
  val singleton: key -> value -> t

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

  val iter: (key -> value -> unit) -> t -> unit
  val fold: (key -> value -> 'r -> 'r) -> t -> 'r -> 'r
  val for_all: (key -> value -> bool) -> t -> bool
  val exists: (key -> value -> bool) -> t -> bool
  val filter: (key -> value -> bool) -> t -> t

  val foldlk : (key -> value -> 'acc -> ('acc -> 'res) -> 'res) -> t -> 'acc -> ('acc -> 'res) -> 'res
  val foldrk : (key -> value -> 'acc -> ('acc -> 'res) -> 'res) -> t -> 'acc -> ('acc -> 'res) -> 'res
  val fold_right : (key -> value -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  type (+'a) step
  val step_map : ('a -> 'b) -> 'a step -> 'b step
  type (+'a) path
  val path_map: ('a -> 'b) -> 'a path -> 'b path

  (* Flesh it out?  Currently this exists in [Trie].
     type ('trunk, -'branch) unstep
     type costep
     val step_apply : ('trunk, 'branch) unstep -> ('trunk * costep) -> 'branch step -> ('trunk * costep)
     val path_apply : ('trunk, 'branch) unstep -> 'trunk -> 'branch path -> ('trunk * costep)
  *)

  exception Inconsistent_path

  type zipper = t * t path
  val zip : t -> zipper
  val unzip : zipper -> t
  val next: zipper -> zipper list
  val find_path : key -> t -> zipper

  val update: key -> (value option -> value option) -> t -> t
  val map: (value -> value) -> t -> t
  val mapi: (key -> value -> value) -> t -> t

  val mapiopt : (key -> value -> value option) -> t -> t
  val merge: (key -> value option -> value option -> value option) -> t -> t -> t

  val union: (key -> value -> value -> value option) -> t -> t -> t
  val compare: (value -> value -> int) -> t -> t -> int
  val equal: (value -> value -> bool) -> t -> t -> bool

  val partition: (key -> value -> bool) -> t -> t * t
  val split: key -> t -> t * value option * t

  val to_seq : t -> (key * value) Seq.t
  val to_seq_from : key -> t -> (key * value) Seq.t
  val add_seq : (key * value) Seq.t -> t -> t
  val of_seq : (key * value) Seq.t -> t

  val lens : key -> (t, value) Lens.t
  val find_defaulting : (unit -> value) -> key -> t -> value
end

let defaulting_lens default lens =
  Lens.{get= (fun x -> try lens.get x with Not_found -> default ()); set= lens.set}

let rec seq_append x y () =
  match x () with
  | Seq.Nil -> y ()
  | Seq.Cons (hd, tl) -> Seq.Cons (hd, fun () -> seq_append tl y ())

module type ShowableS = sig
  type t
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

let string_reverse s =
  let len = String.length s in
  String.init len (fun i -> s.[len - i - 1])

module type WrapTypeS = sig
  type +'a t
end

module type WrapS = sig
  type t
  type value
  val get : t -> value
  val make : value -> t
end

module IdWrapType = struct
  type +'a t = 'a
end

module IdWrap (T: TypeS) = struct
  type t = T.t
  type value = T.t
  let get = identity
  let make = identity
end

let the_global ref maker =
  fun () ->
    match !ref with
    | Some x -> x
    | None ->
      let x = maker () in
      ref := Some x;
      x

let write_file ~path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let b = Bytes.create n in
  really_input ic b 0 n;
  close_in ic;
  Bytes.to_string b

let ignoring_errors default f x =
  try f x with _ -> default

let memoize ?(table=Hashtbl.create 8) f =
  fun i ->
    match Hashtbl.find_opt table i with
    | Some o -> o
    | None -> f i |> fun o -> Hashtbl.replace table i o; o

let bindings_of_hashtbl h =
  Hashtbl.fold (fun k v l -> (k, v)::l) h []

module Test = struct
  let expect_equal description to_string expected computed =
    if not (computed = expected) then
      bork "Expected %s to be %s but instead got %s instead"
        description (to_string expected) (to_string computed)

  let expect_string description expected computed =
    expect_equal description identity expected computed
end

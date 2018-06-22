(* General purpose library
   Stuff here should probably be imported from existing libraries if possible,
   or else upstreamed then imported if possible,
   or else published as their own library.
*)

(* Common exceptions *)

(** exception raised when we haven't implemented the damn thing yet *)
exception Not_implemented

(** internal_error, which should never happen in production, with explanation *)
exception Internal_error of string

(** the bottom function turns anything into anything, by raising an exception *)
val bottom : 'a -> 'b

(** Check that calling a thunk indeed causes exception exn. A useful function for tests. *)
val throws : exn -> (unit -> 'a) -> bool

(* SKI combiantors and their name:
   https://www.johndcook.com/blog/2014/02/06/schonfinkel-combinators/ *)

(** SKI combinators, 1: Identitätsfunktion *)
val identity : 'a -> 'a

(** SKI combinators, 2: Konstanzfunktion, nice to use with defaulting *)
val konstant : 'a -> 'b -> 'a

(** SKI combinators, 3: verSchmelzungsfunktion (amalgamation function; smelting) *)
val schoenfinkel : ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c

(** SKI combinators, 4: verTauschungsfunktion (exchange function) *)
val transpose : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

(** SKI combinators, 5: Zusammensetzungsfunktion (composition function) *)
val zcompose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c


(* Options *)

(** Unwrap an option with a default if None *)
val defaulting : (unit -> 'a) -> 'a option -> 'a

(** Unwrap an option, throwing Not_found if None *)
val unwrap_option : 'a option -> 'a

(** Return true if the option is Some _ *)
val is_option_some : 'a option -> bool

(** Trivial functor from option to list *)
val list_of_option : 'a option -> 'a list

(** Map a function to the content of the option, if any *)
val option_map : ('a -> 'b) -> 'a option -> 'b option

(** Parse a hex string into a binary string,
    where the string is of form "nn:nn:...:nn", where nn represents a char as a hex-digit pair *)
val parse_hex : string -> string

(** Unparse a binary string into a hex string, reversing parse_hex *)
val unparse_hex : ?with_colons:bool -> string -> string


(** Base interface for a type
    NB: same as JaneStreet's Core_kernel.T.T *)
module type T = sig
  type t
end

(** Interface analogous to Map.S from the stdlib, but monomorphic in value *)
module type MapS = sig
  type key
  type value
  type t

  (* Constructing a map *)

  (** The empty map. *)
  val empty: t

  (** [add k v m] returns a map containing the same bindings as
      [m], plus a binding of [k] to [v]. If [k] was already bound
      in [m] to a value that is physically equal to [v],
      [m] is returned unchanged (the result of the function is
      then physically equal to [m]). Otherwise, the previous binding
      of [k] in [m] disappears. *)
  val add: key -> value -> t -> t

  (** [remove k m] returns a map containing the same bindings as
      [m], except for [k] which is unbound in the returned map.
      If [k] was not in [m], [m] is returned unchanged
      (the result of the function is then physically equal to [m]). *)
  val remove: key -> t -> t

  (** [singleton k v] returns the one-element map that contains a binding [k]
      for [v]. *)
  val singleton: key -> value -> t

  (* Consulting a map *)
  (** Test whether a map is empty or not. *)
  val is_empty: t -> bool

  (** [mem k m] returns [true] if [m] contains a binding for [k],
      and [false] otherwise. *)
  val mem: key -> t -> bool

  (** [find k m] returns the current binding of [k] in [m],
      or raises [Not_found] if no such binding exists. *)
  val find: key -> t -> value

  (** [find_opt k m] returns [Some v] if the current binding of [k]
      in [m] is [v], or [None] if no such binding exists. *)
  val find_opt: key -> t -> value option

  (** [find_first f m], where [f] is a monotonically increasing function
      (from false to true) returns the binding of [m] with the lowest key [k]
      such that [f k], or raises [Not_found] if no such key exists.

      For example, [find_first (fun k -> Ord.compare k x >= 0) m] will return
      the first binding [k, v] of [m] where [Ord.compare k x >= 0]
      (intuitively: [k >= x]), or raise [Not_found] if [x] is greater than any
      element of [m]. *)
  val find_first: (key -> bool) -> t -> key * value

  (** [find_first_opt f m], where [f] is a monotonically increasing function,
      returns an option containing the binding of [m] with the lowest key [k]
      such that [f k], or [None] if no such key exists.
  *)
  val find_first_opt: (key -> bool) -> t -> (key * value) option

  (** [find_last f m], where [f] is a monotonically decreasing function,
      returns the binding of [m] with the highest key [k] such that [f k],
      or raises [Not_found] if no such key exists.
  *)
  val find_last: (key -> bool) -> t -> key * value

  (** [find_last_opt f m], where [f] is a monotonically decreasing function,
      returns an option containing the binding of [m] with the highest key [k]
      such that [f k], or [None] if no such key exists.
  *)
  val find_last_opt: (key -> bool) -> t -> (key * value) option

  (** Return the list of all bindings of the given map.
      The returned list is sorted in increasing order with respect
      to the ordering [compare] for keys. *)
  val bindings: t -> (key * value) list

  (** From a list of bindings, return a new map.
      If there are multiple bindings for a key, the first one wins. *)
  val of_bindings: (key * value) list -> t

  (** Return the smallest binding of the given map
      (with respect to the [Ord.compare] ordering), or raise
      [Not_found] if the map is empty.
  *)
  val min_binding: t -> (key * value)

  (** Return the smallest binding of the given map
      (with respect to the [Ord.compare] ordering), or [None]
      if the map is empty. *)
  val min_binding_opt: t -> (key * value) option

  (** Same as {min_binding}, but returns the largest binding
      of the given map.
  *)
  val max_binding: t -> (key * value)

  (** Same as {min_binding_opt}, but returns the largest binding
      of the given map.
  *)
  val max_binding_opt: t -> (key * value) option

  (** Return one binding of the given map, or raise [Not_found] if
      the map is empty. Which binding is chosen is unspecified,
      but equal bindings will be chosen for equal maps.
  *)
  val choose: t -> (key * value)

  (** Return one binding of the given map, or [None] if
      the map is empty. Which binding is chosen is unspecified,
      but equal bindings will be chosen for equal maps.
  *)
  val choose_opt: t -> (key * value) option

  (** Return the number of bindings of a map. *)
  val cardinal: t -> int

  (* Iterating over a map *)

  (** [iter f m] applies [f] to all bindings in map [m].
      [f] receives the key as first argument, and the associated value
      as second argument.  The bindings are passed to [f] in increasing
      order with respect to the ordering over the type of the keys. *)
  val iter: (key -> value -> unit) -> t -> unit

  (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
      where [k1 ... kN] are the keys of all bindings in [m]
      (in increasing order), and [d1 ... dN] are the associated data. *)
  val fold: (key -> value -> 'r -> 'r) -> t -> 'r -> 'r

  (** [for_all p m] checks if all the bindings of the map
      satisfy the predicate [p]. *)
  val for_all: (key -> value -> bool) -> t -> bool

  (** [exists p m] checks if at least one binding of the map
      satisfies the predicate [p]. *)
  val exists: (key -> value -> bool) -> t -> bool

  (** [filter p m] returns the map with all the bindings in [m]
      that satisfy predicate [p]. If [p] satisfies every binding in [m],
      [m] is returned unchanged (the result of the function is then
      physically equal to [m])
  *)
  val filter: (key -> value -> bool) -> t -> t

  (** General variant of fold in CPS, allowing for non-local return *)
  val foldlk : (key -> value -> 'acc -> ('acc -> 'res) -> 'res) -> t -> 'acc -> ('acc -> 'res) -> 'res

  (** General variant of fold_right in CPS *)
  val foldrk : (key -> value -> 'acc -> ('acc -> 'res) -> 'res) -> t -> 'acc -> ('acc -> 'res) -> 'res

  (** [fold_right f m a] computes [(f k1 d1 ... (f kN dN a)...)],
      where [k1 ... kN] are the keys of all bindings in [m]
      (in increasing order), and [d1 ... dN] are the associated data. *)
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

  (** apply a path to a focused submap to retrieve the complete map *)
  val unzip : zipper -> t

  (** Given a focus on a subtrie, return focuses on the next level of subtries.
      If we were focusing on a node with N children, the list will be of length N.
      In particular, the list will be empty if we were already focusing on a leaf,
      and will be of length 2 if we were focusing on a regular branch of a binary tree.
      TODO: also return a (t list -> zipper) to reconstruct the zipper from the next submaps?
  *)
  val next: zipper -> zipper list

  (** Focus on the closest sub map of a map that matches given index *)
  val find_path : key -> t -> zipper

  (* Modifying a map
     NB: unlike the corresponding standard library operations, these are not polymorphic
     in the second value types, because that would require more module scaffolding :-/ *)

  (** [update x f m] returns a map containing the same bindings as
      [m], except for the binding of [x]. Depending on the value of
      [y] where [y] is [f (find_opt x m)], the binding of [x] is
      added, removed or updated. If [y] is [None], the binding is
      removed if it exists; otherwise, if [y] is [Some z] then [x]
      is associated to [z] in the resulting map.  If [x] was already
      bound in [m] to a value that is physically equal to [z], [m]
      is returned unchanged (the result of the function is then
      physically equal to [m]).
  *)
  val update: key -> (value option -> value option) -> t -> t

  (** [map f m] returns a map with same domain as [m], where the
      associated value [a] of all bindings of [m] has been
      replaced by the result of the application of [f] to [a].
      The bindings are passed to [f] in increasing order
      with respect to the ordering over the type of the keys. *)
  val map: (value -> value) -> t -> t

  (** Same as {map}, but the function receives as arguments both the
      key and the associated value for each binding of the map. *)
  val mapi: (key -> value -> value) -> t -> t

  (** Same as {mapi}, but the function can return None,
      at which point the binding is absent in the new map. *)
  val mapiopt : (key -> value -> value option) -> t -> t

  (* Binary operations on maps.
     NB: unlike the corresponding standard library operations, merge and union are not polymorphic
     in the value types, because that would require more module scaffolding :-/ *)

  (** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
      and of [m2]. The presence of each such binding, and the corresponding
      value, is determined with the function [f].
      In terms of the [find_opt] operation, we have
      [find_opt x (merge f m1 m2) = f (find_opt x m1) (find_opt x m2)]
      for any key [x], provided that [f None None = None].
  *)
  val merge: (key -> value option -> value option -> value option) -> t -> t -> t

  (** [union f m1 m2] computes a map whose keys is the union of keys
      of [m1] and of [m2].  When the same binding is defined in both
      arguments, the function [f] is used to combine them.
      This is a special case of [merge]: [union f m1 m2] is equivalent
      to [merge f' m1 m2], where
      - [f' None None = None]
      - [f' (Some v) None = Some v]
      - [f' None (Some v) = Some v]
      - [f' (Some v1) (Some v2) = f v1 v2]
  *)
  val union: (key -> value -> value -> value option) -> t -> t -> t

  (** Total ordering between maps.  The first argument is a total ordering
      used to compare data associated with equal keys in the two maps. *)
  val compare: (value -> value -> int) -> t -> t -> int

  (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
      equal, that is, contain equal keys and associate them with
      equal data.  [cmp] is the equality predicate used to compare
      the data associated with the keys. *)
  val equal: (value -> value -> bool) -> t -> t -> bool

  (** [iterate_over_matching_tree_pair ~recursek ~branchk ~skipk ~leafk ~onlyak ~onlybk ~i ~treea ~treeb ~k]
      Describes a recursive computation over a pair of trees in great generality,
      using continuation-passing style. This style allows the calculation to
      abort and return the calculation result if it has been determined before
      the last node of the tree is visited, or continue processing other nodes
      if necessary.

     The following are caller-specified types:

     - ['c]: type for context needed for continuation of the computation.

     - ['r]: return type of the computation (and for [co_match] itself.)

     Inputs:

     - [recursek ~i ~treea ~treeb ~k]: Result from recursing into the [i]th nodes of
       [treea] and [treeb].

     - [branchk ~i ~height ~cleft ~cright ~k]: Result from recursing down both
       branches from node [i] at specified [height], given results [cleft] and
       [cright] from the two children of that node.

     - [skipk ~i ~height ~depth ~childi ~childres ~k]: Result from recursing
       down [depth] skipped children of node [i], given that the result from
       extant child of the skipped nodes, at index ~childi, is [childres].

     - [leafk ~i ~valuea ~valueb ~k]: Result from comparing two leaf nodes at
       index [i], with values [valuea] and [valueb].

     - [onlyak ~i ~anode ~k]: Result given that [i]th node [anode] is only
       explicitly present in the first tree argument, [treea].

     - [onlybk ~i ~bnode ~k]: Like [onlyak], mutatis mutandis.

     - [i]: Prefix index of [treea], [treeb] nodes.

     - [treea], [treeb]: The two input trees.

     - [k]: Continuation called on the result of the above recursion, to
       (eventually) produce the final result.

  *)
  val iterate_over_matching_tree_pair :
    recursek:(i:key -> treea:t -> treeb:t -> k:('c -> 'r) -> 'r) ->
    branchk:(
      i:key -> height:int -> cleft:'c -> cright:'c -> k:('c -> 'r) -> 'r) ->
    skipk:(i:key -> height:int -> depth:int -> childi:key -> childres:'c ->
           k:('c -> 'r) -> 'r) ->
    leafk:(i:key -> valuea:value -> valueb:value -> k:('c -> 'r) -> 'r) ->
    onlyak:(i:key -> anode:t -> k:('c -> 'r) -> 'r) ->
    onlybk:(i:key -> bnode:t -> k:('c -> 'r) -> 'r) ->
    i:key -> treea:t -> treeb:t -> k:('c -> 'r) -> 'r

  (* Splitting a map *)

  (** [partition p m] returns a pair of maps [(m1, m2)], where
      [m1] contains all the bindings of [s] that satisfy the
      predicate [p], and [m2] is the map with all the bindings of
      [s] that do not satisfy [p].
  *)
  val partition: (key -> value -> bool) -> t -> t * t

  (** [split x m] returns a triple [(l, data, r)], where
      [l] is the map with all the bindings of [m] whose key
      is strictly less than [x];
      [r] is the map with all the bindings of [m] whose key
      is strictly greater than [x];
      [data] is [None] if [m] contains no binding for [x],
      or [Some v] if [m] binds [v] to [x].
  *)
  val split: key -> t -> t * value option * t

  (* Unimplemented from the standard library's map
     val to_seq : t -> (key * value) Seq.t
     val to_seq_from : key -> t -> (key * value) Seq.t
     val add_seq : (key * value) Seq.t -> t -> t
     val of_seq : (key * value) Seq.t -> t
  *)
end

module StringT : sig
  type t = string
end

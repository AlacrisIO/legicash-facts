(* Big Endian Patricia Trees (Tries)
   See article "Fast Mergable Integer Maps" by Chris Okasaki & Andrew Gill, 1998
   http://www.eecs.usma.edu/webs/people/okasaki/ml98maps.ps
*)

(* A big endian patricia tree maps non-negative integers to values.
*)
open Lib
open Crypto
open Lazy
open Integer
open Yojson.Basic.Util

module type TreeSynthS = sig
  type value
  type t
  val empty : t
  val leaf : value -> t
  val branch : int -> t -> t -> t
end

module type TrieSynthS = sig
  include TreeSynthS
  type key
  val skip : int -> int -> key -> t -> t
end

module TrieSynthCardinal (Key : IntS) (Value : T) = struct
  type key = Key.t
  type value = Value.t
  type t = Z.t
  let empty = Z.zero
  let leaf _ = Z.one
  let branch _ x y = Z.add x y
  let skip _ _ _ child = child
end

module TrieSynthComputeSkip (Key : IntS) (Synth: TreeSynthS) = struct
  include Synth
  type key = Key.t
  let [@warning "-32"] skip height length bits synth =
    let rec c len synth =
      if len = length then synth else
        let s = if Key.has_bit bits len then
            branch height empty synth
          else
            branch height synth empty in
        c (len + 1) s
    in
    c 0 synth
end

module type TrieS = sig
  type key
  type value
  type synth

  type t =
    | Empty
    | Leaf of {value: value; synth: synth}
    | Branch of {left: t; right: t; height: int; synth: synth}
    | Skip of {child: t; bits: key; length: int; height: int; synth: synth}

  type (+'a) step =
    | LeftBranch of {right: 'a}
    | RightBranch of {left: 'a}
    | SkipChild of {bits: key; length: int}

  type costep = { height: int ; index: key }

  type (+'a) path = {costep: costep; steps: 'a step list}

  (** Apply a step *)
  type 'a unstep =
    { unstep_left: key -> int -> 'a -> 'a -> 'a
    ; unstep_right: key -> int -> 'a -> 'a -> 'a
    ; unstep_skip: key -> int -> int -> key -> 'a -> 'a }

  val symmetric_unstep:
    branch:(key -> int -> 'a -> 'a -> 'a) ->
    skip:(key -> int -> int -> key -> 'a -> 'a) -> 'a unstep

  val step_apply : 'a unstep -> ('a * costep) -> 'a step -> ('a * costep)
  val path_apply : 'a unstep -> 'a -> 'a path -> ('a * costep)

  include MapS
    with type key := key
     and type value := value
     and type t := t
     and type (+ 'a) step := 'a step
     and type (+'a) path := 'a path

  val trie_height : t -> int
  val ensure_height : int -> t -> t
  val ensure_same_height : t -> t -> t*t
  val get_synth : t -> synth
  val check_invariant : t -> bool
  val verify : t -> t
  val step_length : 'a step -> int
  val check_path_consistency : 'a path -> bool

  val iterate_over_tree: (* See [iterate_over_tree] docstring in [trie.mli] *)
    recursek:(i:key -> tree:t -> k:('r -> 'o) -> 'o) ->
    branchk:(i:key -> height:int -> leftr:'r -> rightr:'r -> k:('r -> 'o) -> 'o) ->
    skipk:(i:key -> height:int -> length:int -> bits:key -> childr:'r ->
           k:('r -> 'o) -> 'o) ->
    leafk:(i:key -> value:value -> k:('r -> 'o) -> 'o) ->
    emptyk:(k:('r -> 'o) -> 'o) ->
    i:key -> tree:t -> k:('r -> 'o) -> 'o

  val iterate_over_tree_pair:
    recursek:(i:key -> treea:t -> treeb:t -> k:('r -> 'o) -> 'o) ->
    branchk:(i:key -> height:int -> leftr:'r -> rightr:'r -> k:('r -> 'o) -> 'o) ->
    skipk:(i:key -> height:int -> length:int -> bits:key -> childr:'r ->
           k:('r -> 'o) -> 'o) ->
    leafk:(i:key -> valuea:value -> valueb:value -> k:('r -> 'o) -> 'o) ->
    onlyak:(i:key -> anode:t -> k:('r -> 'o) -> 'o) ->
    onlybk:(i:key -> bnode:t -> k:('r -> 'o) -> 'o) ->
    i:key -> treea:t -> treeb:t -> k:('r -> 'o) -> 'o
end

(* TODO: an interface to nodes in batch that reduces the amount of unnecessary hashing?
   Or simply make hashing lazy? *)
module Trie (Key : IntS) (Value : T)
    (Synth : TrieSynthS with type key = Key.t and type value = Value.t) = struct
  type key = Key.t
  type value = Value.t
  type synth = Synth.t

  type t =
    | Empty
    | Leaf of {value: value; synth: synth}
    | Branch of {left: t; right: t; height: int; synth: synth}
    | Skip of {child: t; bits: key; length: int; height: int; synth: synth}

  let get_synth = function
    | Empty -> Synth.empty
    | Leaf {synth} -> synth
    | Branch {synth} -> synth
    | Skip {synth} -> synth

  let trie_height = function
    | Empty -> -1
    | Leaf _ -> 0
    | Branch {height} -> height
    | Skip {height} -> height

  let rec check_invariant = function
    | Empty ->
      true (* In a normalized trie, Empty only happens at the toplevel:
              Otherwise, a Branch with an Empty child is normalized to a Skip *)
    | Leaf {value; synth} ->
      synth = Synth.leaf value ||
      raise (Internal_error "Bad leaf synth")
    | Branch {left; right; height; synth} ->
      (synth = Synth.branch height (get_synth left) (get_synth right)
       || raise (Internal_error "Bad branch synth"))
      && (height > 0 || raise (Internal_error "Bad branch height"))
      && (height - 1 = trie_height left (* in particular, left isn't Empty *)
          || raise (Internal_error "Bad left height"))
      && check_invariant left
      && (height - 1 = trie_height right (* in particular, right isn't Empty *)
          || raise (Internal_error "Bad right height"))
      && check_invariant right
    | Skip {child; bits; length; height; synth} ->
      (synth = Synth.skip height length bits (get_synth child)
       || raise (Internal_error "Bad skip synth"))
      && (length > 0
          || raise (Internal_error "Skip length too small"))
      && (height >= length
          || raise (Internal_error "Skip length too large"))
      && (Key.sign bits >= 0
          || raise (Internal_error "Skip bits negative"))
      && (length >= Key.numbits bits
          || raise (Internal_error "Skip bits longer than length"))
      && (not (child = Empty)
          || raise (Internal_error "Skip child empty"))
      && (height - length = trie_height child (* in particular, child isn't Empty *)
          || raise (Internal_error "Skip child height mismatch"))
      && check_invariant child
  let verify x =
    if check_invariant x then
      x
    else
      raise (Internal_error "Invariant failed")

  let empty = Empty

  (* Is this trie empty, i.e. having no mapping from index to value? *)
  let is_empty trie = trie = Empty

  let find_opt key trie =
    let height = trie_height trie in
    if Key.sign key < 0 || Key.numbits key > height then
      None
    else
      let rec f height = function
        | Empty -> None
        | Leaf {value} -> Some value
        | Skip {child; bits; length} ->
          let child_height = height - length in
          if Key.equal bits (Key.extract key child_height length)
          then f child_height child
          else None
        | Branch {left; right} ->
          let child_height = height - 1 in
          let child = if Key.has_bit key child_height then right else left in
          f child_height child
      in
      f height trie

  let find key trie = option_get (find_opt key trie)

  let mem key trie = is_option_some (find_opt key trie)

  (* Lower-level trie constructors, synthesizing the synth attribute *)
  let mk_leaf value =
    Leaf {value; synth=Synth.leaf value}

  let mk_branch height left right =
    Branch {left; right; height; synth=Synth.branch height (get_synth left) (get_synth right)}

  let mk_skip height length bits child =
    Skip {child; height; length; bits; synth=Synth.skip height length bits (get_synth child)}

  (* Higher-level trie constructors, normalizing the skip cases *)
  let make_leaf height key value =
    if height = 0 then
      mk_leaf value
    else
      mk_skip height height (Key.extract key 0 height) (mk_leaf value)

  let make_skip height length bits child =
    if length = 0 then child else
      let bits = Key.extract bits 0 length in
      match child with
      | Empty -> Empty
      | Skip {child=child'; bits=bits'; length=length'} ->
        let length'' = length + length' in
        let bits'' = Key.logor (Key.shift_left bits length') bits' in
        mk_skip height length'' bits'' child'
      | _ -> mk_skip height length bits child


  let make_branch height left right =
    if is_empty right then
      make_skip height 1 Key.zero left
    else if is_empty left then
      make_skip height 1 Key.one right
    else mk_branch height left right

  (** Normalize the head of a trie, removing unneeded skipping of zeroes *)
  let make_head = function
    | Skip {child; bits; length; height} as x ->
      let n = Key.numbits bits in
      if n < length then
        if n = 0 then
          child
        else
          let h = height + n - length in
          make_skip h n bits child
      else x
    | x -> x

  let singleton key value = make_leaf (Key.numbits key) key value

  let add key value trie =
    if is_empty trie then
      singleton key value
    else
      let height = trie_height trie in
      let key_height = Key.numbits key in
      if key_height > height then
        make_branch key_height
          (make_skip (key_height - 1) (key_height - height - 1) Key.zero trie)
          (make_leaf (key_height - 1) key value)
      else
        let rec ins height t k = match t with
          | Empty -> k (make_leaf height key value)
          | Leaf {value=old} -> if value==old then trie else k (mk_leaf value)
          | Branch {left; right} ->
            let branch_height = height - 1 in
            if Key.has_bit key branch_height then
              ins branch_height right (fun t -> k (make_branch height left t))
            else
              ins branch_height left (fun t -> k (make_branch height t right))
          | Skip {child; bits; length} ->
            let child_height = height - length in
            let key_bits = Key.extract key child_height length in
            if Key.equal bits key_bits then
              ins child_height child (fun t -> k (make_skip height length bits t))
            else
              (* new structure:
                 skip for the length that is the same (if not 0),
                 then a branch_node with on one side a new_branch with a new leaf,
                 and on the other, the old_branch with the child of the original skip *)
              let diff_length = Key.numbits (Key.logxor bits key_bits) in
              let same_length = length - diff_length in
              let branch_node_height = height - same_length (* height of the branch node *) in
              let branch_height = branch_node_height - 1 (* height of the two new branches *) in
              let old_branch_length = branch_height - child_height in
              let old_branch = if old_branch_length > 0 then
                  make_skip branch_height old_branch_length (Key.extract bits 0 old_branch_length) child
                else
                  child in
              let new_branch = make_leaf branch_height key value in
              (* Let's look whether the old branch goes right or left *)
              let branch_node = if Key.has_bit bits old_branch_length then
                  make_branch branch_node_height new_branch old_branch
                else
                  make_branch branch_node_height old_branch new_branch in
              k (make_skip height same_length (Key.shift_right bits diff_length) branch_node) in
        ins height trie identity

  let remove key trie =
    let rec r height t k =
      match t with
      | Empty -> trie (* key was absent; return unchanged trie *)
      | Leaf _ -> k Empty
      | Branch {left; right} ->
        let child_height = height - 1 in
        if Key.has_bit key child_height then
          r child_height right (fun t -> k (make_branch height left t))
        else
          r child_height left (fun t -> k (make_branch height t right))
      | Skip {child; bits; length} ->
        r (height - length) child (fun t -> k (make_skip height length bits t))
    in
    r (trie_height trie) trie make_head

  (** Given a Skip at given height, with given length and bits,
      and given the index for the lowest binding it covers,
      return the index for the lowest binding covered by its child. *)
  let skip_index index bits length height =
    (Key.add index (Key.shift_left bits (height - length)))

  (** Given a Branch at given height and index for its lowest binding,
      return the index for the lowest binding of its right branch
      (for its left branch, the index is the same) *)
  let right_index index height =
    (Key.add index (Key.shift_left Key.one (height - 1)))

  let rec map f = function
    | Empty -> Empty
    | Leaf {value} -> mk_leaf (f value)
    | Branch {left; right; height} ->
      mk_branch height (map f left) (map f right)
    | Skip {child; bits; length; height} ->
      mk_skip height length bits (map f child)

  let mapi f t =
    let rec m index = function
      | Empty -> Empty
      | Leaf {value} -> mk_leaf (f index value)
      | Branch {left; right; height} ->
        mk_branch height (m index left) (m (right_index index height) right)
      | Skip {child; bits; length; height} ->
        mk_skip height length bits (m (skip_index index bits length height) child)
    in
    m Key.zero t

  let mapiopt f t =
    let rec m i = function
      | Empty -> Empty
      | Leaf {value} -> (match (f i value) with None -> Empty | Some v -> mk_leaf v)
      | Branch {left; right; height} ->
        make_branch height (m i left) (m (right_index i height) right)
      | Skip {child; bits; length; height} ->
        make_skip height length bits (m (skip_index i bits length height) child)
    in
    m Key.zero t

  let iterate_over_tree (* See [iterate_over_tree] docstring in [trie.mli] *)
        ~recursek ~branchk ~skipk ~leafk ~emptyk ~i ~tree ~k =
    match tree with
    | Empty -> emptyk ~k
    | Leaf {value} -> leafk ~i ~value:value ~k
    | Branch {left; right; height} ->
      recursek ~i ~tree:left
        ~k:(fun left ->
          recursek ~i:(right_index i height) ~tree:right
            ~k:(fun right ->
              branchk ~i ~height ~leftr:left ~rightr:right ~k))
    | Skip {child;bits;length;height} ->
      let child_index = skip_index i bits length height in
      recursek ~i:child_index ~tree:child
        ~k:(fun child -> skipk ~i ~height ~length ~bits ~childr:child ~k)

  let foldlk f trie acc k =
    let rec frec index acc k = function
      | Empty -> k acc
      | Leaf {value} -> (f index value acc k)
      | Branch {left; right; height} ->
        frec index acc (fun acc -> frec (right_index index height) acc k right) left
      | Skip {child; bits; length; height} ->
        frec (skip_index index bits length height) acc k child
    in
    frec Key.zero acc k trie

  let foldrk f t acc k =
    let rec frec index acc k = function
      | Empty -> k acc
      | Leaf {value} -> (f index value acc k)
      | Branch {left; right; height} ->
        frec (right_index index height) acc (fun acc -> frec index acc k left) right
      | Skip {child; bits; length; height} ->
        frec (skip_index index bits length height) acc k child
    in
    frec Key.zero acc k t

  let fold f t acc =
    foldlk (fun i v acc k -> k (f i v acc)) t acc identity

  let iter f t =
    foldlk (fun i v () k -> f i v; k ()) t () identity

  let fold_right f t acc =
    foldrk (fun i v acc k -> k (f i v acc)) t acc identity

  let for_all p t =
    foldlk (fun i v b k -> if p i v then k b else false) t true identity

  let exists p t =
    foldlk (fun i v b k -> if p i v then true else k b) t false identity

  let filter p t =
    let rec filterrec index = function
      | Empty -> (Empty, true)
      | Leaf {value} as x -> if (p index value) then (x, true) else (Empty, false)
      | Branch {left; right; height} as x ->
        let newleft, sameleft = filterrec index left in
        let newright, sameright = filterrec (right_index index height) right in
        if sameleft && sameright then (x, true) else (make_branch height newleft newright, false)
      | Skip {child; bits; length; height} as x ->
        let newchild, samechild = filterrec (skip_index index bits length height) child in
        if samechild then (x, true) else (make_skip height length bits newchild, false)
    in
    fst (filterrec Key.zero t)

  let rec cardinal = function
    | Empty -> 0
    | Leaf _ -> 1
    | Branch {left; right} -> (cardinal left) + (cardinal right)
    | Skip {child} -> cardinal child

  let bindings t = fold_right (fun i v acc -> (i, v) :: acc) t []

  let of_bindings bindings = List.fold_right (fun (k, v) m -> add k v m) bindings empty

  let min_binding_opt t =
    foldlk (fun i v _acc _k -> Some (i, v)) t None identity

  let min_binding t = option_get (min_binding_opt t)

  let max_binding_opt t =
    foldrk (fun i v _acc _k -> Some (i, v)) t None identity

  let max_binding t = option_get (max_binding_opt t)

  let choose_opt t = min_binding_opt t

  let choose t = min_binding t

  let find_first_opt f trie =
    let rec divide (index: key) (default: (key*value) option)
        (leftward: (key*t) list) (rightward: (key*t) list) : t -> (key*value) option = function
      | Empty -> default
      | Leaf {value} -> conquer index value default leftward rightward
      | Branch {left; right; height} ->
        (match leftward with
         | [] -> divide (right_index index height) default [(index, left)] rightward right
         | _ -> divide index default leftward ((right_index index height, right)::rightward) left)
      | Skip {child;height;length;bits} ->
        divide (skip_index index bits length height) default leftward rightward child
    and conquer index value default leftward rightward =
      let (new_default, tries) =
        if f index then (Some (index, value), leftward) else (default, List.rev rightward) in
      match tries with
      | [] -> new_default
      | (index, trie) :: leftward -> divide index default leftward [] trie
    in
    divide Key.zero None [] [] trie

  let find_first f t = option_get (find_first_opt f t)

  let find_last_opt f trie =
    let rec flo index default trie = match trie with
      | Empty -> default
      | Leaf {value} -> if f index then Some (index, value) else default
      | Branch {left;right;height} ->
        let b = max_binding left in
        if f (fst b) then
          flo (right_index index height) (Some b) right
        else
          flo index default left
      | Skip {child;height;length;bits} ->
        flo (skip_index index bits length height) default child
    in
    flo Key.zero None trie

  let find_last f t = option_get (find_last_opt f t)

  let partition p t =
    let rec prec index = function
      | Empty -> (Empty, Empty)
      | Leaf {value} as x -> if p index value then (x, Empty) else (Empty, x)
      | Branch {left; right; height} ->
        let (ly, ln) = prec index left in
        let (ry, rn) = prec (right_index index height) right in
        (make_branch height ly ry, make_branch height ln rn)
      | Skip {child; bits; length; height} ->
        let (l, r) = prec (skip_index index bits length height) child in
        let f = make_skip height length bits in
        (f l, f r)
    in
    prec Key.zero t

  let split k t =
    if Key.sign k < 0 then
      (Empty, None, t)
    else if Key.numbits k > trie_height t then
      (t, None, Empty)
    else
      let rec srec index = function
        | Empty -> (Empty, None, Empty)
        | Leaf {value} -> (Empty, Some value, Empty)
        | Branch {left; right; height} ->
          if Key.has_bit k (height - 1) then
            let (l, x, r) = srec (right_index index height) right in
            (make_branch height left l, x, make_skip height 1 Key.one r)
          else
            let (_l, x, r) = srec index left in
            (make_skip height 1 Key.zero left, x, make_branch height r right)
        | Skip {child; bits; length; height} ->
          let (l, x, r) = srec (skip_index index bits length height) child in
          let f = make_skip height length bits in
          (f l, x, f r)
      in
      srec Key.zero t

  let ensure_height h t =
    let th = trie_height t in
    if th < h then
      make_skip h (h - th) Key.zero t
    else
      t

  let ensure_same_height ta tb =
    (ensure_height (trie_height tb) ta, ensure_height (trie_height ta) tb)

  type 'a step =
    | LeftBranch of {right: 'a}
    | RightBranch of {left: 'a}
    | SkipChild of {bits: key; length: int}

  type 'a unstep =
    { unstep_left: key -> int -> 'a -> 'a -> 'a
    ; unstep_right: key -> int -> 'a -> 'a -> 'a
    ; unstep_skip: key -> int -> int -> key -> 'a -> 'a }

  let symmetric_unstep ~branch ~skip =
    { unstep_left=branch
    ; unstep_right=branch
    ; unstep_skip=skip }

  type costep = { height: int ; index: key }

  let step_apply unstep (trie, {height; index}) step =
    match step with
    | LeftBranch {right} ->
      let h = height + 1 in
      (unstep.unstep_right index h trie right, {height=h; index})
    | RightBranch {left} ->
      let h = height + 1 in
      let i = Key.sub index (Key.shift_left Key.one height) in
      (unstep.unstep_left i h left trie, {height=h; index=i})
    | SkipChild {bits; length} ->
      let h = height + length in
      let i = Key.sub index (Key.extract index 0 h) in
      (unstep.unstep_skip i h length bits trie, {height=h; index=i})

  let step_map f = function
    | LeftBranch {right} -> LeftBranch {right= f right}
    | RightBranch {left} -> RightBranch {left= f left}
    | SkipChild {bits;length} -> SkipChild {bits;length}

  let step_length = function
    | LeftBranch _ -> 1
    | RightBranch _ -> 1
    | SkipChild {length} -> length

  type 'a path = {costep: costep; steps: 'a step list}

  let path_apply unstep t {costep; steps} =
    List.fold_left (step_apply unstep) (t, costep) steps

  let path_map f {costep;steps} =
    {costep;steps= List.map (step_map f) steps}

  exception Inconsistent_path

  let check_path_consistency {costep={index;height};steps} =
    Key.sign index >= 0 &&
    let rec c index height = function
      | [] -> true
      | step :: steps ->
        (match step with
         | LeftBranch _ -> not (Key.has_bit index height)
         | RightBranch _ -> Key.has_bit index height
         | SkipChild {bits; length} -> Key.equal bits (Key.extract index height length))
        && c index (height + step_length step) steps in
    c index height steps

  type zipper = t * (t path)

  let zip t = (t, {costep={index=Key.zero; height=trie_height t}; steps=[]})

  let unzip =
    let unstep = symmetric_unstep ~branch:(konstant make_branch) ~skip:(konstant make_skip) in
    fun (t, path) -> fst (path_apply unstep t path)

  let next = function
    | (Empty, _up) -> []
    | (Leaf _, _up) -> []
    | (Branch {left; right}, {costep={index; height}; steps}) ->
      let height = height - 1 in
      let rindex = right_index index height in
      [(left, {costep={index; height}; steps= LeftBranch {right} :: steps});
       (right, {costep={index=rindex; height}; steps= RightBranch {left} :: steps})]
    | (Skip {child; bits; length}, {costep={index; height}; steps}) ->
      let index = skip_index index bits length height in
      [(child, {costep={index; height=height-length}; steps= SkipChild {bits; length} :: steps})]

  let find_path l t =
    let h = trie_height t in
    if Key.sign l < 0 then
      raise (Internal_error "find_path negative index")
    else
      let height = Key.numbits l in
      if Key.numbits l > h then
        let hh = height - 1 in
        let left = make_skip hh (hh-h) Key.zero t in
        (Empty, {costep={index=Key.shift_left Key.zero hh; height=hh}; steps=[RightBranch{left}]})
      else
        let rec f x = match x with
          | (Empty, _) -> x
          | (Leaf _, _) -> x
          | (Skip {child; bits; length; height}, {costep={index}; steps}) ->
            let child_height = height - length in
            if Key.equal bits (Key.extract l child_height length) then
              let index=skip_index index bits length height in
              f (child, {costep={index;height=child_height};
                         steps=SkipChild{bits; length}::steps})
            else x
          | (Branch {left; right; height}, {costep={index}; steps}) ->
            let child_height = height - 1 in
            if Key.has_bit l child_height then
              f (right, {costep={index= right_index index height; height=child_height};
                         steps=RightBranch{left}::steps})
            else
              f (left, {costep={index; height=child_height};
                        steps=LeftBranch{right}::steps})
        in
        f (zip t)

  (* update, using paths *)
  let update l f t =
    let (sub, up) = find_path l t in
    let o = match t with Leaf {value} -> Some value | _ -> None in
    let u = f o in
    if o = u then t else
      let ll = Key.extract l 0 up.costep.height in
      let newsub =
        match u with
        | None -> remove ll sub
        | Some v -> add ll v sub in
      unzip (newsub, up)

  (* update, totally not optimized for tries
     let update l f t =
     let o = find_opt l t in
     let u = f o in
     if o = u then t else
     match u with
     | None -> remove l t
     | Some v -> add l v t
  *)

  let skip_choice height length bits child sublength =
    if Key.has_bit bits sublength then
      (Empty, make_skip (height - length + sublength) sublength (Key.extract bits 0 sublength) child)
    else
      (make_skip (height - length + sublength) sublength (Key.extract bits 0 sublength) child, Empty)

  let iterate_over_tree_pair (* See [iterate_over_tree_pair] docstring in [trie.mli] *)
        ~recursek ~branchk ~skipk ~leafk ~onlyak ~onlybk ~i ~treea ~treeb ~k =
    match (treea, treeb) with
    | (_, Empty) -> onlyak ~i ~anode:treea ~k
    | (Empty, _) -> onlybk ~i ~bnode:treeb ~k
    | (Leaf {value=va}, Leaf {value=vb}) -> leafk ~i ~valuea:va ~valueb:vb ~k
    | (Branch {left=aleft; right=aright; height}, Branch {left=bleft; right=bright}) ->
      recursek ~i ~treea:aleft ~treeb:bleft
        ~k:(fun left ->
            recursek ~i:(right_index i height) ~treea:aright ~treeb:bright
              ~k:(fun right ->
                  branchk ~i ~height ~leftr:left ~rightr:right ~k))
    | (Branch {left=left; right=right}, Skip {child; bits; length; height}) ->
      let l1 = length - 1 in
      let ri = right_index i height in
      if Key.has_bit bits l1 then
        onlyak ~i ~anode:left ~k:(fun left ->
            recursek ~i:ri ~treea:right ~treeb:(make_skip height l1 bits child)
              ~k:(fun right ->
                  branchk ~i ~height ~leftr:left ~rightr:right ~k))
      else
        recursek ~i ~treea:left ~treeb:(make_skip height l1 bits child)
          ~k:(fun left ->
              onlyak ~i:ri ~anode:right ~k:(fun right ->
                  branchk ~i ~height ~leftr:left ~rightr:right ~k))
    | (Skip {child;bits;length;height}, Branch {left;right}) ->
      let l1 = length - 1 in
      let ri = right_index i height in
      if Key.has_bit bits l1 then
        onlybk ~i ~bnode:left ~k:(fun left ->
            recursek ~i:ri ~treea:(make_skip height l1 bits child) ~treeb:right
              ~k:(fun right -> branchk ~i ~height ~leftr:left ~rightr:right ~k))
      else
        recursek ~i ~treea:(make_skip height l1 bits child) ~treeb:left
          ~k:(fun left -> onlybk ~i:ri ~bnode:right ~k:(fun right ->
              branchk ~i ~height ~leftr:left ~rightr:right ~k))
    | (Skip {child=achild;bits=abits;length=alength;height}, Skip {child=bchild;bits=bbits;length=blength}) ->
      let length = min alength blength in
      let ahighbits = Key.extract abits (alength - length) length in
      let bhighbits = Key.extract bbits (blength - length) length in
      let difflength = Key.numbits (Key.logxor ahighbits bhighbits) in
      let samelength = length - difflength in
      let sameheight = height - samelength in
      let (_samebits, isame, samek) =
        if samelength = 0 then
          (Key.zero, i, k)
        else
          let samebits = Key.extract ahighbits (length - samelength) samelength in
          let isame = Key.logor i (Key.shift_left samebits sameheight) in
          let samek = fun child ->
            skipk ~i:isame ~height ~length:samelength ~bits:samebits ~childr:child ~k in
          (samebits, isame, samek) in
      let adifflength = alength - samelength in
      let bdifflength = blength - samelength in
      if difflength = 0 then
        recursek ~i:isame
          ~treea:(make_skip sameheight adifflength abits achild)
          ~treeb:(make_skip sameheight bdifflength bbits bchild) ~k:samek
      else
        let (aleft, aright) = skip_choice height alength abits achild (adifflength - 1) in
        let (bleft, bright) = skip_choice height blength bbits bchild (bdifflength - 1) in
        recursek ~i:isame ~treea:aleft ~treeb:bleft ~k:(fun left ->
            recursek ~i:(right_index isame (sameheight - 1)) ~treea:aright ~treeb:bright
              ~k:(fun right ->
                  branchk ~i:isame ~height:sameheight ~leftr:left ~rightr:right ~k:samek))
    | _ -> raise (Internal_error "iterate_over_tree_pair")

  let merge f a b =
    let (a, b) = ensure_same_height a b in
    let rec m ~i ~treea:_ ~treeb:_ ~k =
      iterate_over_tree_pair
        ~recursek:m
        ~branchk:(fun ~i:_ ~height ~leftr ~rightr ~k -> k (make_branch height leftr rightr))
        ~skipk:(fun ~i:_ ~height ~length ~bits ~childr ~k -> k (make_skip height length bits childr))
        ~leafk:(fun ~i ~valuea ~valueb ~k ->
            k (match (f i (Some valuea) (Some valueb)) with None -> Empty | Some v -> mk_leaf v))
        ~onlyak:(fun ~i ~anode:ta ~k -> k (mapiopt (fun _ v -> f i (Some v) None) ta))
        ~onlybk:(fun ~i ~bnode:tb ~k -> k (mapiopt (fun _ v -> f i None (Some v)) tb))
        ~i ~treea:a ~treeb:b ~k in
    m ~i:Key.zero ~treea:a ~treeb:b ~k:identity

  let compare cmp a b =
    let (a, b) = ensure_same_height a b in
    let rec m ~i ~treea:_ ~treeb:_ ~k =
      iterate_over_tree_pair
        ~recursek:m
        ~branchk:(fun ~i:_ ~height:_ ~leftr:_ ~rightr:_ ~k -> k ())
        ~skipk:(fun ~i:_ ~height:_ ~length:_ ~bits:_ ~childr:_ ~k -> k ())
        ~leafk:(fun ~i:_ ~valuea ~valueb ~k ->
            let r = cmp valuea valueb in if r = 0 then k () else r)
        ~onlyak:(fun ~i:_ ~anode:_ ~k:_ -> 1)
        ~onlybk:(fun ~i:_ ~bnode:_ ~k:_ -> -1)
        ~i ~treea:a ~treeb:b ~k in
    m ~i:Key.zero ~treea:a ~treeb:b ~k:(konstant 0)

  let equal eq a b =
    let rec loop ~i ~treea:a ~treeb:b ~k =
      if a == b then k () else
        iterate_over_tree_pair
          ~recursek:loop
          ~branchk:(fun ~i:_ ~height:_ ~leftr:_ ~rightr:_ ~k -> k ())
          ~skipk:(fun ~i:_ ~height:_ ~length:_ ~bits:_ ~childr:_ ~k -> k ())
          ~leafk:(fun ~i:_ ~valuea ~valueb ~k ->
              if eq valuea valueb then k () else false)
          ~onlyak:(fun ~i:_ ~anode:_ ~k:_ -> false)
          ~onlybk:(fun ~i:_ ~bnode:_ ~k:_ -> false)
          ~i ~treea:a ~treeb:b ~k in
    loop ~i:Key.zero ~treea:a ~treeb:b ~k:(konstant true)

  let union f a b =
    let f' i a b = match (a, b) with
      | None, None -> None
      | (Some v), None -> Some v
      | None, (Some v) -> Some v
      | (Some va), (Some vb) -> f i va vb in
    merge f' a b

  (*let to_seq t = foldlk (fun i v n k () -> (Seq.Cons (i, v) (fun () -> k n))) t Nil identity
    let to_seq_from k t =
    let (focus, {costep, steps}) = find_path k t in
    path_apply
    { unstep_left = (fun _ _ a b -> seq_cat a b)
    ; unstep_right = (fun _ _ _ b -> b)
    ; unstep_skip = (fun _ _ _ _ a -> a) }
    steps (focus, costep)
    let add_seq s t = Seq.fold_left (fun t (k, v) -> add k v t) t s
    val of_seq s = add_seq s empty*)

  let lens k = Lens.{get= find k; set= add k}

  let find_defaulting default k m = defaulting default (find_opt k m)
end

module type TrieSynthMerkleS = sig
  include TrieSynthS with type t = Digest.t
  val leaf_digest : t -> t
end

module TrieSynthMerkle (Key : IntS) (Value : DigestibleS) =
struct
  type key = Key.t
  type value = Value.t
  type t = Digest.t
  let empty = Digest.zero
  (* TODO: have a global table of non-clashing constants instead of all these numbers.
     Or better: make sure these constants are actually themselves the digests of a descriptor
     for the type of data being digested.
  *)
  let leaf_digest digest = digest_of_string ("\001" ^ Digest.to_big_endian_bits digest)
  let leaf v = leaf_digest (Value.digest v)
  let branch h x y =
    digest_of_string ("\002" ^ big_endian_bits_of_nat 16 (Nat.of_int h)
                      ^ Digest.to_big_endian_bits x
                      ^ Digest.to_big_endian_bits y)
  let skip height length bits child =
    digest_of_string ("\002" ^ big_endian_bits_of_nat 16 (Nat.of_int height)
                      ^ big_endian_bits_of_nat 16 (Nat.of_int length)
                      ^ Key.to_big_endian_bits bits
                      ^ Digest.to_big_endian_bits child)
end

module type MerkleTrieS = sig
  type key
  type value
  module Synth : TrieSynthMerkleS with type key = key and type value = value and type t = Digest.t
  include TrieS
    with type key := key
     and type value := value
     and type synth = Synth.t

  type proof =
    { key : key
    ; trie : Digest.t
    ; value : Digest.t
    ; steps : (Digest.t step) list
    }

  val trie_digest : t -> Digest.t
  val path_digest : t path -> Digest.t path
  val get_proof : key -> t -> proof option
  val check_proof_consistency : proof -> bool
  val json_of_proof : proof -> Yojson.Basic.json
end

module MerkleTrie (Key : IntS) (Value : DigestibleS) = struct
  module Synth = TrieSynthMerkle (Key) (Value)
  include Trie (Key) (Value) (Synth)

  let trie_digest = get_synth

  let path_digest = path_map trie_digest

  type proof =
    { key : key
    ; trie : Digest.t
    ; value : Digest.t
    ; steps : (Digest.t step) list
    }

  let get_proof (key: key) (trie: t) : proof option =
    match find_path key trie with
    | Leaf {value}, up -> Some { key
                               ; trie = trie_digest trie
                               ; value = Value.digest value
                               ; steps = (path_digest up).steps
                               }
    | _ -> None

  (** Check the consistency of a Proof.
      1- starting from the leaf_digest of the value's digest and applying the path,
      we should arrive at the top trie's hash.
      2- starting from the key, the path should follow the key's bits.
  *)
  let check_proof_consistency proof =
    let path_d = {costep={index=proof.key;height=0};steps=proof.steps} in
    check_path_consistency path_d
    && let (top_d, {height; index}) =
         path_apply
           (symmetric_unstep ~branch:(konstant Synth.branch) ~skip:(konstant Synth.skip))
           (Synth.leaf_digest proof.value)
           path_d in
    proof.trie = top_d
    && height >= Key.numbits proof.key
    && Key.sign index = 0

  let skip_bit_string length bits =
    String.concat "" (List.init length (fun i -> if Key.has_bit bits i then "1" else "0"))

  let step_to_json = function
    | LeftBranch {right} ->
      `Assoc [ ("type", `String "Left")
             ; ("digest", `String (Digest.to_hex_string right)) ]
    | RightBranch {left} ->
      `Assoc [ ("type", `String "Right")
             ; ("digest", `String (Digest.to_hex_string left)) ]
    | SkipChild {bits; length} ->
      `Assoc [ ("type", `String "Skip")
             ; ("bits", `String (skip_bit_string length bits)) ]

  let [@warning "-32"] proof_to_json {key; trie; value; steps} =
    `Assoc
      [ ("key", `String (Key.to_hex_string key))
      ; ("trie", `String (Digest.to_hex_string trie))
      ; ("value", `String (Digest.to_hex_string value))
      ; ("steps", `List (List.map step_to_json steps)) ]

  let [@warning "-32"] proof_to_json_string = zcompose Yojson.to_string proof_to_json

  let bit_string_to_skip s =
    let length = String.length s in
    let f i bits =
      if i < length then
        Key.logor bits (Key.shift_left Key.one i)
      else
        bits
    in
    SkipChild {bits=f 0 Key.zero; length}

  let step_of_json json =
    let t = json |> member "type" |> to_string in
    if t = "Left" then
      LeftBranch {right=json |> member "digest" |> to_string |> Digest.of_hex_string}
    else if t = "Right" then
      RightBranch {left=json |> member "digest" |> to_string |> Digest.of_hex_string}
    else if t = "Skip" then
      json |> member "bits" |> to_string |> bit_string_to_skip
    else raise (Internal_error "Bad json")

  let json_of_step step =
    match step with
    | LeftBranch { right } -> `Assoc [("left",`String ("0x" ^ (Digest.to_hex_string right)))]
    | RightBranch { left } -> `Assoc [("right",`String ("0x" ^ (Digest.to_hex_string left)))]
    | SkipChild { bits; length } -> `Assoc [ ("bits",`String ("0x" ^ (Key.to_hex_string bits)))
                                           ; ("length",`Int length)
                                           ]

  let proof_of_json json =
    { key = json |> member "key" |> to_string |> Key.of_hex_string
    ; trie = json |> member "trie" |> to_string |> Digest.of_hex_string
    ; value = json |> member "value" |> to_string |> Digest.of_hex_string
    ; steps = json |> member "steps" |> to_list |> List.map step_of_json
    }

  let json_of_proof proof =
    `Assoc
      [ ("key",`String ("0x" ^ (Key.to_hex_string proof.key)))
      ; ("trie",`String ("0x" ^ (Digest.to_hex_string proof.trie)))
      ; ("value",`String ("0x" ^ (Digest.to_hex_string proof.value)))
      ; ("steps",`List (List.map json_of_step proof.steps))
      ]

  (* let proof_of_json_string = zcompose proof_of_json Yojson.Basic.from_string *)
end

module type MerkleTrieSetS = sig
  type elt
  module T : MerkleTrieS with type key = elt and type value = unit
  type t = T.t
  include Set.S with type elt := elt and type t := t

  type proof =
    { elt : elt
    ; trie : Digest.t
    ; steps : (Digest.t T.step) list
    }

  val trie_digest : t -> Digest.t
  val get_proof : elt -> t -> proof option
  val check_proof_consistency : proof -> bool

  val lens : elt -> (t, bool) Lens.t
end

module MerkleTrieSet (Elt : IntS) = struct
  type elt = Elt.t
  module T = MerkleTrie (Elt) (Unit)
  type t = T.t

  let wrap f elt _ = f elt

  let trie_digest = T.trie_digest

  let empty = T.empty
  let is_empty = T.is_empty
  let mem elt t = is_option_some (T.find_opt elt t)
  let add elt t = T.add elt () t
  let singleton elt = T.singleton elt ()
  let remove = T.remove
  let iter f = T.iter (wrap f)
  let fold f = T.fold (wrap f)
  let map _f t = fold add t empty
  let for_all f = T.for_all (wrap f)
  let exists f = T.exists (wrap f)
  let filter f = T.filter (wrap f)
  let partition f = T.partition (wrap f)
  let cardinal = T.cardinal
  let elements t = List.map fst (T.bindings t)
  let min_elt t = fst (T.min_binding t)
  let min_elt_opt t = option_map fst (T.min_binding_opt t)
  let max_elt t = fst (T.max_binding t)
  let max_elt_opt t = option_map fst (T.max_binding_opt t)
  let choose = min_elt
  let choose_opt = min_elt_opt
  let split elt t = match T.split elt t with (a, v, b) -> (a, is_option_some v, b)
  let find_opt elt t = if (mem elt t) then Some elt else None
  let find elt t = option_get (find_opt elt t)
  let find_first_opt f t = option_map fst (T.find_first_opt f t)
  let find_first f t = option_get (find_first_opt f t)
  let find_last_opt f t = option_map fst (T.find_last_opt f t)
  let find_last f t = option_get (find_last_opt f t)
  let of_list l = List.fold_right add l empty
  (*
     let to_seq t = Seq.map fst (T.to_seq t)
     let to_seq_from k t = Seq.map fst (T.to_seq_from k t)
     let add_seq s t = T.add_seq (Seq.map (fun x -> (x, ())) s) t
     let of_seq s = add_seq s empty
  *)

  (* TODO: for union, inter, diff, compare, equal, subset, optimize for full subtries, by keeping cardinality as well as digest as synthetic data ? *)
  let union a b = T.merge (fun _ _ _ -> Some ()) a b
  let inter a b = T.merge (fun _ a b -> match (a, b) with Some _, Some _ -> Some () | _ -> None) a b
  let diff a b = T.merge (fun _ a b -> match (a, b) with Some _, None -> Some () | _ -> None) a b

  let compare_unit_options () () = 0

  let compare a b = T.compare compare_unit_options a b
  let equal a b = (trie_digest a) = (trie_digest b)

  let subset a b =
    let (a, b) = T.ensure_same_height a b in
    let rec m ~i ~treea ~treeb:_ ~k =
      if (trie_digest treea) = (trie_digest treea) then k () else
        T.iterate_over_tree_pair
          ~recursek:m
          ~branchk:(fun ~i:_ ~height:_ ~leftr:_ ~rightr:_ ~k -> k ())
          ~skipk:(fun ~i:_ ~height:_ ~length:_ ~bits:_ ~childr:_ ~k -> k ())
          ~leafk:(fun ~i:_ ~valuea:_ ~valueb:_ ~k -> k ())
          ~onlyak:(fun ~i:_ ~anode:_ ~k:_ -> k ())
          ~onlybk:(fun ~i:_ ~bnode:_ ~k:_ -> false)
          ~i ~treea:a ~treeb:b ~k in
    m ~i:Elt.zero ~treea:a ~treeb:b ~k:(konstant true)

  type proof =
    { elt : elt
    ; trie : Digest.t
    ; steps : (Digest.t T.step) list
    }
  let get_proof elt t =
    option_map (fun {T.key; T.trie; T.steps} -> {elt=key; trie; steps}) (T.get_proof elt t)

  let check_proof_consistency {elt; trie; steps} =
    T.check_proof_consistency {key=elt; value=Unit.digest (); trie; steps}

  let lens k = Lens.{get= mem k; set= (fun b -> if b then add k else remove k)}
end

module DigestSet = MerkleTrieSet (Digest)

module Test = struct
  let generic_compare = compare
  module SimpleTrie = Trie (UInt256) (StringT) (TrieSynthCardinal (UInt256) (StringT))
  module MyTrie = MerkleTrie (UInt256) (StringT)
  open MyTrie
  let [@warning "-32"] nat_of_key : MyTrie.key -> Crypto.UInt256.t = fun x -> x
  let [@warning "-32"] key_of_nat : Crypto.UInt256.t -> MyTrie.key = fun x -> x

  let [@warning "-32"] rec print_trie out_channel = function
    | Empty ->
      Printf.fprintf out_channel "Empty"
    | Leaf {value} ->
      Printf.fprintf out_channel "Leaf{value=%S}" value
    | Branch {height; left; right} ->
      Printf.fprintf out_channel "Branch{height=%d;left=" height ;
      print_trie out_channel left ; Printf.fprintf out_channel ";right=" ;
      print_trie out_channel right ; Printf.fprintf out_channel "}"
    | Skip {height; length; bits; child} ->
      Printf.fprintf
        out_channel "Skip{height=%d;length=%d;bits=%s;child="
        height length (UInt256.to_string bits) ;
      print_trie out_channel child ; Printf.fprintf out_channel "}"

  let n = UInt256.of_int
  let s = UInt256.to_string
  (* let println s = Printf.printf "%s\n" s *)
  (* let showln x = print_trie stdout x ; Printf.printf "\n" *)

  let sort_bindings bindings = List.sort generic_compare bindings
  let make_bindings n f = List.init n (fun i -> let j = i + 1 in (UInt256.of_int j, f j))
  (* let bindings_equal x y = (sort_bindings x) = (sort_bindings y) *)

  let knuth_shuffle array =
    let n = Array.length array in
    let a = Array.copy array in
    for i = n - 1 downto 1 do
      let k = Random.int (i+1) in
      let x = a.(k) in
      a.(k) <- a.(i);
      a.(i) <- x
    done;
    a

  let shuffle_list l = Array.to_list (knuth_shuffle (Array.of_list l))

  let is_even z = UInt256.equal (UInt256.extract z 0 1) UInt256.zero

  let trie_of_bindings b = lazy (verify (of_bindings (force b)))

  let bindings_4 : (UInt256.t * string) list lazy_t = lazy (make_bindings 4 string_of_int)
  let trie_4 = trie_of_bindings bindings_4

  let bindings_10 : (UInt256.t * string) list lazy_t = lazy (make_bindings 10 string_of_int)
  let trie_10 = trie_of_bindings bindings_10

  let bindings_100 : (UInt256.t * string) list lazy_t = lazy (make_bindings 100 string_of_int)
  let trie_100 = trie_of_bindings bindings_100

  let bindings_1 = lazy (shuffle_list (force bindings_100))
  let trie_1 = trie_of_bindings bindings_1

  let bindings_2 = lazy (List.filter (zcompose is_even fst) (force bindings_100))
  let trie_2 = trie_of_bindings bindings_2

  let bindings_3 = lazy (List.filter (zcompose (fun s -> String.get s 0 = '6') snd) (force bindings_100))
  let trie_3 = trie_of_bindings bindings_3

  let bindings_5 = lazy (List.filter (fun (i,s) -> is_even i && String.get s 0 = '6') (force bindings_100))
  let trie_5 = trie_of_bindings bindings_5

  let bindings_10_12_57 = lazy [(n 57, "57");(n 10, "10");(n 12, "12")]
  let trie_10_12_57 = trie_of_bindings bindings_10_12_57

  let test_bindings = [lazy []; lazy [(n 42, "x")]; bindings_10_12_57; bindings_4; bindings_10; bindings_1; bindings_2; bindings_3; bindings_5]

  let timeit s l =
    let tm0 = Unix.gettimeofday () in
    ignore (force l) ;
    let tm1 = Unix.gettimeofday () in
    let diff = tm1 -. tm0 in
    Printf.printf "%s: %0.04f\n%!" s diff

  let [@warning "-32"] init_timing () =
    timeit "bindings_4" bindings_4 ;
    timeit "bindings_10" bindings_10 ;
    timeit "bindings_100" bindings_100 ;
    timeit "bindings_1" bindings_1 ;
    timeit "bindings_2" bindings_2 ;
    timeit "bindings_3" bindings_3 ;
    timeit "bindings_5" bindings_5 ;
    timeit "trie_4" trie_4 ;
    timeit "trie_10" trie_10 ;
    timeit "trie_100" trie_100 ;
    timeit "trie_1" trie_1 ;
    timeit "trie_2" trie_2 ;
    timeit "trie_3" trie_3 ;
    timeit "trie_5" trie_5 ;
    true

  let%test "empty" =
    (bindings empty = []) && (of_bindings [] = Empty)

  let%test "find_opt_some" =
    Some "12" = find_opt (n 12) (force trie_10_12_57)

  let%test "find_opt_none" =
    None = find_opt (n 13) (force trie_10_12_57)

  (* let rec intersperse separator = function
     [] -> [] | [a] -> [a] | a :: l -> a :: separator :: intersperse separator l
  *)

  let [@warning "-32"] string_of_value_option = function None -> "None" | Some v -> Printf.sprintf "Some %S" v
  let [@warning "-32"] string_of_binding (i, v) = Printf.sprintf "(%s,%S)" (s i) v
  let [@warning "-32"] string_of_bindings (l : (key*value) list) =
    Printf.sprintf "[%s]" (String.concat ";" (List.map string_of_binding l))

  let for_all_bindings name p =
    List.for_all
      (fun b ->
         let trie = trie_of_bindings b in
         p (force b, force trie) ||
         raise (Internal_error (Printf.sprintf "Bad %s" name)))
      test_bindings

  let%test "find_opt_all" =
    for_all_bindings "find_opt_all"
      (fun (b, trie) -> List.for_all (fun (i, v) -> Some v = find_opt i trie) b)

  let%test "find_found" =
    "12" = find (n 12) (force trie_10_12_57)

  let%test "find_not_found" =
    throws Not_found (fun _ -> find (n 13) (force trie_10_12_57))

  let%test "find_all" =
    for_all_bindings "find_all"
      (fun (b, trie) -> List.for_all (fun (i, v) -> v = find i trie) b)

  let%test "mem_true" =
    true = mem (n 12) (force trie_10_12_57)

  let%test "mem_false" =
    false = mem (n 13) (force trie_10_12_57)

  let%test "mem_all" =
    for_all_bindings "mem_all"
      (fun (b, trie) -> List.for_all (fun (i, _) -> mem i trie) b)

  let%test "bindings of_bindings" =
    for_all_bindings "bindings of_bindings"
      (fun (b, trie) -> bindings trie = sort_bindings b)

  let%test "min_binding" =
    throws Not_found (fun _ -> min_binding empty)
    && min_binding (singleton (n 42) "x") = ((n 42), "x")
    && min_binding (force trie_1) = ((n 1), "1")
    && min_binding (force trie_2) = ((n 2), "2")
    && min_binding (force trie_3) = ((n 6), "6")
    && min_binding (force trie_4) = ((n 1), "1")
    && min_binding (force trie_5) = ((n 6), "6")
    && min_binding (force trie_10_12_57) = ((n 10), "10")

  let%test "max_binding" =
    throws Not_found (fun _ -> max_binding empty)
    && max_binding (singleton (n 42) "x") = ((n 42), "x")
    && max_binding (force trie_1) = ((n 100), "100")
    && max_binding (force trie_2) = ((n 100), "100")
    && max_binding (force trie_3) = ((n 69), "69")
    && max_binding (force trie_4) = ((n 4), "4")
    && max_binding (force trie_5) = ((n 68), "68")
    && max_binding (force trie_10_12_57) = ((n 57), "57")

  let%test "cardinal" =
    for_all_bindings "cardinal"
      (fun (b, t) -> cardinal t = List.length b)

  let%test "add 0" =
    for_all_bindings "add"
      (fun (b, trie) ->
         let trie0 = verify (add (n 0) "0" trie) in
         None = find_opt (n 0) trie
         && "0" = find (n 0) trie0
         && cardinal trie0 = 1 + List.length b)

  let%test "add 2" =
    bindings (verify (add (n 2) "2" (of_bindings [((n 1), "1");((n 3), "3")])))
    = make_bindings 3 string_of_int

  let%test "remove" =
    bindings (verify (remove (n 12) (force trie_10_12_57))) = [((n 10), "10");((n 57),"57")]

  let%test "remove_all" =
    for_all_bindings "remove_all"
      (fun (b, trie) -> empty = List.fold_right remove (List.map fst b) trie)

  let%test "equal shuffle" =
    for_all_bindings "equal shuffle"
      (fun (b, trie) -> equal (=) trie (of_bindings (shuffle_list b)))

  let%test "unequal" =
    not (equal (=) (force trie_4) (force trie_1))

  let make_step direction digest =
    `Assoc [ ("type",`String direction)
           ; ("digest",`String digest)
           ]
  let make_left_step = make_step "Left"
  let make_right_step = make_step "Right"

  let proof_42_in_trie_100 =
    lazy (proof_of_json
            (`Assoc [ ("key",`String "2a")
                    ; ("trie",`String "1fac8e232b535a615c42d929507b86e5c5869e3ead1cdd25441c56deb2dd5c51")
                    ; ("value", `String "ccb1f717aa77602faf03a594761a36956b1c4cf44c6b336d1db57da799b331b8")
                    ; ("steps",
                       `List [ make_left_step "f2d009d08b6ccf098f0144bd6c60deb2e2f90c1d535081fe8583e84f0a57648c"
                             ; make_right_step "040bce26cfff0c2a04a53d601105c74340136630178b6ba3068646dadd04b463"
                             ; make_left_step "0ebcfde2bc1f1dbd006b1c61c0db408331d0a9c7f2e64624f42d0f9d573438ba"
                             ; make_right_step "7e82d31d9aa3e780051bef9c72851e03293c13d2dea72f8180e543f7d1c0a864"
                             ; make_left_step "7e4f446333427a39e10841f29f88aeab908225a90b1d12e396124ca94992774f"
                             ; make_right_step "5afc63f18942c3d103b1517efd60cd053cd744031c9bf56756038af6ad728a2c"
                             ; make_left_step "22c05957bb11ec9ac4b41662a658e560c2a32a3b70ea47a2bc182f4a2522ca05"
                             ])
                    ]))

  let bad_proof = lazy (match force proof_42_in_trie_100 with
      | { key
        ; trie
        ; value
        ; steps = [s1;s2;s3;s4;s5;s6;s7]
        } ->
        { key
        ; trie
        ; value
        ; steps = [s1;s2;s5;s4;s3;s6;s7] (* steps 3 and 5 are swapped *)
        }
      | _ -> raise (Internal_error "Bad proof"))

  let%test "proof" =
    get_proof (n 42) (force trie_100) = Some (force proof_42_in_trie_100)

  let%test "proof_consistent" =
    check_proof_consistency (force proof_42_in_trie_100)

  let%test "proof_inconsistent" =
    not (check_proof_consistency (force bad_proof))

end

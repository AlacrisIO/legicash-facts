(* Big Endian Patricia Trees (Tries)
   See article "Fast Mergable Integer Maps" by Chris Okasaki & Andrew Gill, 1998
   http://www.eecs.usma.edu/webs/people/okasaki/ml98maps.ps
*)

(* A big endian patricia tree maps non-negative integers to values.
*)
open Lib
open Yojsoning
open Integer


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

module TrieSynthUnit (Key : IntS) (Value : TypeS) = struct
  type key = Key.t
  type value = Value.t
  type t = unit
  let empty = ()
  let leaf _ = ()
  let branch _ _ _ = ()
  let skip _ _ _ _ = ()
end

module TrieSynthCardinal (Key : IntS) (Value : TypeS) = struct
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

module type TrieTypeS = sig
  type key
  type value
  type synth
  type +'a wrap
  type t = trie wrap
  and trie =
    | Empty
    | Leaf of {value: value; synth: synth}
    | Branch of {left: t; right: t; height: int; synth: synth}
    | Skip of {child: t; bits: key; length: int; height: int; synth: synth}
  val trie_synth : trie -> synth
  val trie_leaf : value -> trie
  val trie_branch : (t -> trie) -> int -> t -> t -> trie
  val trie_skip : (t -> trie) -> int -> int -> key -> t -> trie
end

module TrieType
    (Key : IntS) (Value : TypeS) (WrapType : WrapTypeS)
    (Synth : TrieSynthS with type key = Key.t and type value = Value.t) = struct
  type key = Key.t
  type value = Value.t
  type synth = Synth.t
  type +'a wrap = 'a WrapType.t
  type t = trie wrap
  and trie =
    | Empty
    | Leaf of {value: value; synth: synth}
    | Branch of {left: t; right: t; height: int; synth: synth}
    | Skip of {child: t; bits: key; length: int; height: int; synth: synth}
  let trie_synth = function
    | Empty -> Synth.empty
    | Leaf {synth} -> synth
    | Branch {synth} -> synth
    | Skip {synth} -> synth
  let get_synth get x = trie_synth (get x)
  let trie_leaf value = Leaf {value; synth=Synth.leaf value}
  let trie_branch get height left right =
    Branch {left; right; height; synth=Synth.branch height (get_synth get left) (get_synth get right)}
  let trie_skip get height length bits child =
    Skip {child; height; length; bits;
          synth=Synth.skip height length bits (get_synth get child)}
end

module type TrieS = sig
  include TrieTypeS

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
    branchk:(i:key -> height:int -> leftr:'r -> rightr:'r -> synth:synth ->
             k:('r -> 'o) -> 'o) ->
    skipk:(i:key -> height:int -> length:int -> bits:key -> childr:'r -> synth:synth ->
           k:('r -> 'o) -> 'o) ->
    leafk:(i:key -> value:value -> synth:synth -> k:('r -> 'o) -> 'o) ->
    emptyk:(k:('r -> 'o) -> 'o) ->
    i:key -> tree:t -> k:('r -> 'o) -> 'o

  val iterate_over_tree_pair: (* See [iterate_over_tree_pair] docstring in [trie.mli] *)
    recursek:(i:key -> treea:t -> treeb:t -> k:('r -> 'o) -> 'o) ->
    branchk:(i:key -> height:int -> leftr:'r -> rightr:'r -> k:('r -> 'o) -> 'o) ->
    skipk:(i:key -> height:int -> length:int -> bits:key -> childr:'r ->
           k:('r -> 'o) -> 'o) ->
    leafk:(i:key -> valuea:value -> valueb:value -> k:('r -> 'o) -> 'o) ->
    onlyak:(i:key -> anode:t -> k:('r -> 'o) -> 'o) ->
    onlybk:(i:key -> bnode:t -> k:('r -> 'o) -> 'o) ->
    i:key -> treea:t -> treeb:t -> k:('r -> 'o) -> 'o

  include YojsonableS with type t := t
end

(* TODO: an interface to nodes in batch that reduces the amount of unnecessary hashing?
   Or simply make hashing lazy? *)
module Trie
    (Key : IntS) (Value : YojsonableS) (WrapType : WrapTypeS)
    (Synth : TrieSynthS with type key = Key.t and type value = Value.t)
    (TrieType : TrieTypeS with type key = Key.t
                           and type value = Value.t
                           and type +'a wrap = 'a WrapType.t
                           and type synth = Synth.t)
    (Wrap : WrapS with type value = TrieType.trie and type t = TrieType.t) = struct
  include TrieType

  let wrap_fun f x = f (Wrap.get x)

  let get_synth = wrap_fun trie_synth

  let trie_height =
    wrap_fun
      (function
        | Empty -> -1
        | Leaf _ -> 0
        | Branch {height} -> height
        | Skip {height} -> height)

  let empty = Wrap.make Empty

  (* Is this trie empty, i.e. having no mapping from index to value? *)
  let is_empty = wrap_fun (fun trie -> trie = Empty)

  let rec check_invariant trie = match Wrap.get trie with
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
      && (not (is_empty child)
          || raise (Internal_error "Skip child empty"))
      && (height - length = trie_height child (* in particular, child isn't Empty *)
          || raise (Internal_error "Skip child height mismatch"))
      && check_invariant child

  let verify x =
    if check_invariant x then
      x
    else
      raise (Internal_error "Invariant failed")

  let find_opt key trie =
    let height = trie_height trie in
    if Key.sign key < 0 || Key.numbits key > height then
      None
    else
      let rec f height =
        wrap_fun
          (function
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
              f child_height child)
      in
      f height trie

  let find key trie = Option.get (find_opt key trie)

  let mem key trie = Option.is_some (find_opt key trie)

  (* Lower-level trie constructors, synthesizing the synth attribute *)
  let mk_leaf value = trie_leaf value |> Wrap.make

  let mk_branch height left right =
    trie_branch Wrap.get height left right |> Wrap.make

  let mk_skip height length bits child =
    trie_skip Wrap.get height length bits child |> Wrap.make

  (* Higher-level trie constructors, normalizing the skip cases *)
  let make_leaf height key value =
    if height = 0 then
      mk_leaf value
    else
      mk_skip height height (Key.extract key 0 height) (mk_leaf value)

  let make_skip height length bits child =
    if length = 0 then child else
      let bits = Key.extract bits 0 length in
      match Wrap.get child with
      | Empty -> empty
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
  let make_head trie = match Wrap.get trie with
    | Skip {child; bits; length; height} ->
      let n = Key.numbits bits in
      if n < length then
        if n = 0 then
          child
        else
          let h = height + n - length in
          make_skip h n bits child
      else trie
    | _ -> trie

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
        let rec ins height t k = match Wrap.get t with
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
      match Wrap.get t with
      | Empty -> trie (* key was absent; return unchanged trie *)
      | Leaf _ -> k empty
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

  let rec map f =
    wrap_fun
      (function
        | Empty -> empty
        | Leaf {value} -> mk_leaf (f value)
        | Branch {left; right; height} ->
          mk_branch height (map f left) (map f right)
        | Skip {child; bits; length; height} ->
          mk_skip height length bits (map f child))

  let mapi f t =
    let rec m index =
      wrap_fun
        (function
          | Empty -> empty
          | Leaf {value} -> mk_leaf (f index value)
          | Branch {left; right; height} ->
            mk_branch height (m index left) (m (right_index index height) right)
          | Skip {child; bits; length; height} ->
            mk_skip height length bits (m (skip_index index bits length height) child))
    in
    m Key.zero t

  let mapiopt f t =
    let rec m i =
      wrap_fun
        (function
          | Empty -> empty
          | Leaf {value} -> (match (f i value) with None -> empty | Some v -> mk_leaf v)
          | Branch {left; right; height} ->
            make_branch height (m i left) (m (right_index i height) right)
          | Skip {child; bits; length; height} ->
            make_skip height length bits (m (skip_index i bits length height) child))
    in
    m Key.zero t

  let iterate_over_tree (* See [iterate_over_tree] docstring in [trie.mli] *)
        ~recursek ~branchk ~skipk ~leafk ~emptyk ~i ~tree ~k =
    match Wrap.get tree with
    | Empty -> emptyk ~k
    | Leaf {value; synth} -> leafk ~i ~value ~synth ~k
    | Branch {left; right; height; synth} ->
      recursek ~i ~tree:left
        ~k:(fun left ->
          recursek ~i:(right_index i height) ~tree:right
            ~k:(fun right ->
              branchk ~i ~height ~leftr:left ~rightr:right ~synth ~k))
    | Skip {child;bits;length;height; synth} ->
      let child_index = skip_index i bits length height in
      recursek ~i:child_index ~tree:child
        ~k:(fun child -> skipk ~i ~height ~length ~bits ~childr:child ~synth ~k)

  let foldlk f trie acc k =
    let rec frec index trie acc k = match Wrap.get trie with
      | Empty -> k acc
      | Leaf {value} -> (f index value acc k)
      | Branch {left; right; height} ->
        frec index left acc (fun acc -> frec (right_index index height) right acc k)
      | Skip {child; bits; length; height} ->
        frec (skip_index index bits length height) child acc k
    in
    frec Key.zero trie acc k

  let foldrk f t acc k =
    let rec frec index trie acc k = match Wrap.get trie with
      | Empty -> k acc
      | Leaf {value} -> (f index value acc k)
      | Branch {left; right; height} ->
        frec (right_index index height) right acc (fun acc -> frec index left acc k)
      | Skip {child; bits; length; height} ->
        frec (skip_index index bits length height) child acc k
    in
    frec Key.zero t acc k

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
    let rec filterrec index trie = match Wrap.get trie with
      | Empty -> (empty, true)
      | Leaf {value} -> if (p index value) then (trie, true) else (empty, false)
      | Branch {left; right; height} ->
        let newleft, sameleft = filterrec index left in
        let newright, sameright = filterrec (right_index index height) right in
        if sameleft && sameright then (trie, true) else (make_branch height newleft newright, false)
      | Skip {child; bits; length; height} ->
        let newchild, samechild = filterrec (skip_index index bits length height) child in
        if samechild then (trie, true) else (make_skip height length bits newchild, false)
    in
    fst (filterrec Key.zero t)

  let rec cardinal trie = match Wrap.get trie with
    | Empty -> 0
    | Leaf _ -> 1
    | Branch {left; right} -> (cardinal left) + (cardinal right)
    | Skip {child} -> cardinal child

  let bindings t = fold_right (fun i v acc -> (i, v) :: acc) t []

  let of_bindings bindings = List.fold_right (fun (k, v) m -> add k v m) bindings empty

  let min_binding_opt t =
    foldlk (fun i v _acc _k -> Some (i, v)) t None identity

  let min_binding t = Option.get (min_binding_opt t)

  let max_binding_opt t =
    foldrk (fun i v _acc _k -> Some (i, v)) t None identity

  let max_binding t = Option.get (max_binding_opt t)

  let choose_opt t = min_binding_opt t

  let choose t = min_binding t

  let find_first_opt f trie =
    let rec divide (index: key) (default: (key*value) option)
              (leftward: (key*t) list) (rightward: (key*t) list) : t -> (key*value) option =
      wrap_fun
        (function
          | Empty -> default
          | Leaf {value} -> conquer index value default leftward rightward
          | Branch {left; right; height} ->
            (match leftward with
             | [] -> divide (right_index index height) default [(index, left)] rightward right
             | _ -> divide index default leftward ((right_index index height, right)::rightward) left)
          | Skip {child;height;length;bits} ->
            divide (skip_index index bits length height) default leftward rightward child)
    and conquer index value default leftward rightward =
      let (new_default, tries) =
        if f index then (Some (index, value), leftward) else (default, List.rev rightward) in
      match tries with
      | [] -> new_default
      | (index, trie) :: leftward -> divide index default leftward [] trie
    in
    divide Key.zero None [] [] trie

  let find_first f t = Option.get (find_first_opt f t)

  let find_last_opt f trie =
    let rec flo index default trie = match Wrap.get trie with
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

  let find_last f t = Option.get (find_last_opt f t)

  let partition p t =
    let rec prec index trie = match Wrap.get trie with
      | Empty -> (empty, empty)
      | Leaf {value} -> if p index value then (trie, empty) else (empty, trie)
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
      (empty, None, t)
    else if Key.numbits k > trie_height t then
      (t, None, empty)
    else
      let rec srec index trie = match Wrap.get trie with
        | Empty -> (empty, None, empty)
        | Leaf {value} -> (empty, Some value, empty)
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
    let rec c height = function
      | [] -> true
      | step :: steps ->
        (match step with
         | LeftBranch _ -> not (Key.has_bit index height)
         | RightBranch _ -> Key.has_bit index height
         | SkipChild {bits; length} -> Key.equal bits (Key.extract index height length))
        && c (height + step_length step) steps in
    c height steps

  type zipper = t * (t path)

  let zip t = (t, {costep={index=Key.zero; height=trie_height t}; steps=[]})

  let unzip =
    let unstep = symmetric_unstep ~branch:(konstant make_branch) ~skip:(konstant make_skip) in
    fun (t, path) -> fst (path_apply unstep t path)

  let next (trie, path) = match (Wrap.get trie, path) with
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
        (empty, {costep={index=Key.shift_left Key.zero hh; height=hh}; steps=[RightBranch{left}]})
      else
        let rec f (trie, path) = match (Wrap.get trie, path) with
          | (Empty, _) -> (trie, path)
          | (Leaf _, _) -> (trie, path)
          | (Skip {child; bits; length; height}, {costep={index}; steps}) ->
            let child_height = height - length in
            if Key.equal bits (Key.extract l child_height length) then
              let index=skip_index index bits length height in
              f (child, {costep={index;height=child_height};
                         steps=SkipChild{bits; length}::steps})
            else (trie, path)
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
    let o = match Wrap.get t with Leaf {value} -> Some value | _ -> None in
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
      (empty, make_skip (height - length + sublength) sublength (Key.extract bits 0 sublength) child)
    else
      (make_skip (height - length + sublength) sublength (Key.extract bits 0 sublength) child, empty)

  let iterate_over_tree_pair (* See [iterate_over_tree_pair] docstring in [trie.mli] *)
        ~recursek ~branchk ~skipk ~leafk ~onlyak ~onlybk ~i ~treea ~treeb ~k =
    match (Wrap.get treea, Wrap.get treeb) with
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
          k (match (f i (Some valuea) (Some valueb)) with None -> empty | Some v -> mk_leaf v))
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

  let find_defaulting default k m = Option.defaulting default (find_opt k m)

  let to_yojson t =
    foldrk (fun i v l k -> k (`List [Key.to_yojson i; Value.to_yojson v] :: l)) t [] (fun x -> `List x)

  let of_yojson = function
    | `List l ->
      list_foldlk (fun t x k ->
        match x with
        | `List [ij; vj] ->
          Result.bind (Key.of_yojson ij)
            (fun i -> Result.bind (Value.of_yojson vj)
                        (fun v -> k (add i v t)))
        | _ -> Error "bad trie json")
        empty
        l
        (fun r -> Ok r)
    | _ -> Error "bad trie json"

  include (Yojsonable(struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : YojsonableS with type t := t)
end

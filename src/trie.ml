(* Big Endian Patricia Trees (Tries)
   See article "Fast Mergable Integer Maps" by Chris Okasaki & Andrew Gill, 1998
   http://www.eecs.usma.edu/webs/people/okasaki/ml98maps.ps
*)

(* A big endian patricia tree maps non-negative integers to values.
*)
open Lib
open Integer
open Crypto

module type TrieSynthS = sig
  type value
  type t
  val empty : t
  val leaf : value -> t
  val branch : int -> t -> t -> t
end

module type TrieSkipSynthS = sig
  include TrieSynthS
  type key
  val skip : int -> int -> key -> t -> t
end

module TrieSynthCardinal (Key : UnsignedS) (Value : T) = struct
  type key = Key.t
  type value = Value.t
  type t = Z.t
  let empty = Z.zero
  let leaf _ = Z.one
  let branch _ x y = Z.add x y
  let skip _ _ _ child = child
end

module TrieSynthComputeSkip (Key : UnsignedS) (Synth: TrieSynthS) = struct
  include Synth
  type key = Key.t
  let skip height length bits synth =
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
  type trie_key
  type trie_value
  type synth

  type trie =
    | Empty
    | Leaf of {value: trie_value; synth: synth}
    | Branch of {left: trie; right: trie; height: int; synth: synth}
    | Skip of {child: trie; bits: trie_key; length: int; height: int; synth: synth}

  type (+'a) trie_step =
    | LeftBranch of {right: 'a}
    | RightBranch of {left: 'a}
    | SkipChild of {bits: trie_key; length: int}

  type (+'a) trie_path = {index: trie_key; height: int; steps: 'a trie_step list}

  include MapS
    with type key = trie_key
     and type value = trie_value
     and type t = trie
     and type (+ 'a) step = 'a trie_step
     and type (+'a) path = 'a trie_path

  val trie_height : t -> int
  val ensure_height : int -> t -> t
  val ensure_same_height : t -> t -> t*t
  val get_synth : t -> synth
  val check_invariant : t -> bool
  val verify : t -> t
  val step_length : 'a step -> int
  val check_path_consistency : 'a path -> bool

end

module Trie (Key : UnsignedS) (Value : T)
    (Synth : TrieSkipSynthS with type key = Key.t and type value = Value.t) = struct

  type trie_key = Key.t
  type trie_value = Value.t

  type key = Key.t
  type value = Value.t
  type synth = Synth.t

  type trie =
    | Empty
    | Leaf of {value: value; synth: synth}
    | Branch of {left: trie; right: trie; height: int; synth: synth}
    | Skip of {child: trie; bits: key; length: int; height: int; synth: synth}

  type t = trie

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
      raise (Internal_error "bad leaf synth")
    | Branch {left; right; height; synth} ->
      (synth = Synth.branch height (get_synth left) (get_synth right)
       || raise (Internal_error "bad branch synth"))
      && (height > 0 || raise (Internal_error "bad branch height"))
      && (height - 1 = trie_height left (* in particular, left isn't Empty *)
          || raise (Internal_error "bad left height"))
      && check_invariant left
      && (height - 1 = trie_height right (* in particular, right isn't Empty *)
          || raise (Internal_error "bad right height"))
      && check_invariant right
    | Skip {child; bits; length; height; synth} ->
      (synth = Synth.skip height length bits (get_synth child)
       || raise (Internal_error "bad skip synth"))
      && (length > 0
          || raise (Internal_error "skip length too small"))
      && (height >= length
          || raise (Internal_error "skip length too large"))
      && (Key.sign bits >= 0
          || raise (Internal_error "skip bits negative"))
      && (length >= Key.numbits bits
          || raise (Internal_error "skip bits longer than length"))
      && (not (child = Empty)
          || raise (Internal_error "skip child empty"))
      && (height - length = trie_height child (* in particular, child isn't Empty *)
          || raise (Internal_error "skip child height mismatch"))
      && check_invariant child
  let verify x =
    if check_invariant x then
      x
    else
      raise (Internal_error "invariant failed")

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

  let find key trie = unwrap_option (find_opt key trie)

  let mem key trie = is_option_some (find_opt key trie)

  (* Lower-level trie constructors, synthesizing the synth attribute *)
  let mk_leaf value =
    verify (Leaf {value; synth=Synth.leaf value})

  let mk_branch height left right =
    verify (Branch {left; right; height; synth=Synth.branch height (get_synth left) (get_synth right)})

  let mk_skip height length bits child =
    Skip {child; height; length; bits; synth=Synth.skip height length bits (get_synth child)}

  (* Higher-level trie constructors, normalizing the skip cases *)
  let make_leaf height key value =
    if height = 0 then
      mk_leaf value
    else
      mk_skip height height (Key.extract key 0 height) (mk_leaf value)

  let make_skip height length bits child =
    if length = 0 then verify child else
      let bits = Key.extract bits 0 length in
      match child with
      | Empty -> verify Empty
      | Skip {child=child'; bits=bits'; length=length'; synth=synth'} ->
        let length'' = length + length' in
        let bits'' = Key.logor (Key.shift_left bits length') bits' in
        mk_skip height length'' bits'' child'
      | _ -> mk_skip height length bits child


  let make_branch height left right =
    if is_empty right then
      (make_skip height 1 Key.zero left)
    else if is_empty left then
      (make_skip height 1 Key.one right)
    else verify (mk_branch height left right)

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
              k (make_skip height same_length (Key.shift_right bits diff_length) branch_node)
          | Branch {left; right} ->
            let branch_height = height - 1 in
            if Key.has_bit key branch_height then
              ins branch_height right (fun t -> k (verify (make_branch height left t)))
            else
              ins branch_height left (fun t -> k (verify (make_branch height t right))) in
        ins height trie identity

  let remove key trie =
    let rec r height t k =
      match t with
      | Empty -> trie (* key was absent; return unchanged trie *)
      | Leaf _ -> k Empty
      | Branch {left; right} ->
        let child_height = height - 1 in
        if Key.has_bit key child_height then
          r child_height right (fun t -> k (verify (make_branch height left t)))
        else
          r child_height left (fun t -> k (verify (make_branch height t right)))
      | Skip {child; bits; length} ->
        r (height - length) child (fun t -> k (verify (make_skip height length bits (verify t))))
    in
    r (trie_height trie) (verify trie) make_head

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

  let rec fold_right f t acc =
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
    | Branch {left; right; height} -> (cardinal left) + (cardinal right)
    | Skip {child} -> cardinal child

  let bindings t = fold_right (fun i v acc -> (i, v) :: acc) t []

  let of_bindings bindings = List.fold_right (fun (k, v) m -> add k v m) bindings empty

  let min_binding_opt t =
    foldlk (fun i v acc k -> Some (i, v)) t None identity

  let min_binding t = unwrap_option (min_binding_opt t)

  let max_binding_opt t =
    foldrk (fun i v acc k -> Some (i, v)) t None identity

  let max_binding t = unwrap_option (max_binding_opt t)

  let choose_opt t = min_binding_opt t

  let choose t = min_binding t

  let find_first_opt_k f i v acc k = if f i then Some (i, v) else k acc

  (* dichotomy on increasing function f.
     TODO: use zippers to avoid computing f twice on the same node *)
  let find_first_opt__ f trie =
    let rec ffo index default trie = match trie with
      | Empty -> default
      | Leaf {value} -> if f index then Some (index, value) else default
      | Branch {left;right;height} ->
        let (i, v) = min_binding right in
        if f i then
          ffo index (Some (i, v)) left
        else
          ffo (right_index index height) default right
      | Skip {child;height;length;bits} ->
        ffo (skip_index index bits length height) default child
    in
    ffo Key.zero None trie

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

  let find_first f t = unwrap_option (find_first_opt f t)

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

  let find_last f t = unwrap_option (find_last_opt f t)

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
            let (l, x, r) = srec index left in
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

  type 'a trie_step =
    | LeftBranch of {right: 'a}
    | RightBranch of {left: 'a}
    | SkipChild of {bits: key; length: int}
  type 'a step = 'a trie_step

  let step_apply make_branch make_skip step (trie, height) =
    match step with
    | LeftBranch {right} -> let h = height + 1 in (make_branch h trie right, h)
    | RightBranch {left} -> let h = height + 1 in (make_branch h left trie, h)
    | SkipChild {bits; length} -> let h = height + length in (make_skip h length bits trie, h)

  let step_map f = function
    | LeftBranch {right} -> LeftBranch {right= f right}
    | RightBranch {left} -> RightBranch {left= f left}
    | SkipChild {bits;length} -> SkipChild {bits;length}

  let step_length = function
    | LeftBranch _ -> 1
    | RightBranch _ -> 1
    | SkipChild {length} -> length

  type 'a trie_path = {index: key; height: int; steps: 'a step list}
  type 'a path = 'a trie_path

  let path_apply make_branch make_skip {height;steps} (t, h) =
    List.fold_left (fun th s -> step_apply make_branch make_skip s th) (t, h) steps

  let path_map f {index;height;steps} =
    {index;height;steps= List.map (step_map f) steps}

  exception Inconsistent_path

  let check_path_consistency {index;height;steps} =
    Key.sign (Key.extract index 0 height) = 0 &&
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

  let zip t = (t, {index=Key.zero; height=trie_height t; steps=[]})

  let unzip (t, path) = match path with
    | {height;steps} ->
      if not (t = Empty || trie_height t = height) then raise Inconsistent_path else
        fst (path_apply make_branch make_skip path (t, trie_height t))

  let next = function
    | (Empty, up) -> []
    | (Leaf _, up) -> []
    | (Branch {left; right}, {index; height; steps}) ->
      let height = height - 1 in
      let rindex = right_index index height in
      [(left, {index; height; steps= LeftBranch {right} :: steps});
       (right, {index= rindex; height; steps= RightBranch {left} :: steps})]
    | (Skip {child; bits; length}, {index; height; steps}) ->
      let index = skip_index index bits length height in
      [(child, {index; height=height-length; steps= SkipChild {bits; length} :: steps})]

  let find_path l t =
    let h = trie_height t in
    if Key.sign l < 0 then
      raise (Internal_error "find_path negative index")
    else
      let height = Key.numbits l in
      if Key.numbits l > h then
        let hh = height - 1 in
        let left = make_skip hh (hh-h) Key.zero t in
        (Empty, {index=Key.shift_left Key.zero hh; height=hh; steps=[RightBranch{left}]})
      else
        let rec f x = match x with
          | (Empty, _) -> x
          | (Leaf _, _) -> x
          | (Skip {child; bits; length; height}, {index; steps}) ->
            let child_height = height - length in
            if Key.equal bits (Key.extract l child_height length) then
              let index=skip_index index bits length height in
              f (child, {index;height=child_height;steps=SkipChild{bits; length}::steps})
            else x
          | (Branch {left; right; height}, {index; steps}) ->
            let child_height = height - 1 in
            if Key.has_bit l child_height then
              f (right, {index= right_index index height; height=child_height; steps=RightBranch{left}::steps})
            else
              f (left, {index; height=child_height; steps=LeftBranch{right}::steps})
        in
        f (zip t)

  (* update, using paths *)
  let update l f t =
    let (sub, up) = find_path l t in
    let o = match t with Leaf {value} -> Some value | _ -> None in
    let u = f o in
    if o = u then t else
      let ll = Key.extract l 0 up.height in
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

  let co_match
        (recursek: Key.t -> t * t -> ('c -> 'r) -> 'r)
        (branchk: Key.t -> int -> 'c -> 'c -> ('c -> 'r) -> 'r)
        (skipk: Key.t -> int -> int -> Key.t -> 'c -> ('c -> 'r) -> 'r)
        (leafk: Key.t -> Value.t -> Value.t -> ('c -> 'r) -> 'r)
        (onlyak: Key.t -> t -> ('c -> 'r) -> 'r)
        (onlybk: Key.t -> t -> ('c -> 'r) -> 'r)
        (i : Key.t)
        ((a : t), (b : t))
        (k : 'c -> 'r) = match (a, b) with
    | (_, Empty) -> onlyak i a k
    | (Empty, _) -> onlybk i b k
    | (Leaf {value=va}, Leaf {value=vb}) -> leafk i va vb k
    | (Branch {left=aleft; right=aright; height}, Branch {left=bleft; right=bright}) ->
      recursek i (aleft, bleft) (fun left ->
        recursek (right_index i height) (aright, bright) (fun right ->
          branchk i height left right k))
    | (Branch {left=left; right=right}, Skip {child; bits; length; height}) ->
      let l1 = length - 1 in
      let ri = right_index i height in
      if Key.has_bit bits l1 then
        onlyak i left (fun left ->
          recursek ri (right, make_skip height l1 bits child) (fun right ->
            branchk i height left right k))
      else
        recursek i (left, make_skip height l1 bits child) (fun left ->
          onlyak ri right (fun right ->
            branchk i height left right k))
    | (Skip {child;bits;length;height}, Branch {left;right}) ->
      let l1 = length - 1 in
      let ri = right_index i height in
      if Key.has_bit bits l1 then
        onlybk i left (fun left ->
          recursek ri (make_skip height l1 bits child, right) (fun right ->
            branchk i height left right k))
      else
        recursek i (make_skip height l1 bits child, left) (fun left ->
          onlybk ri right (fun right ->
            branchk i height left right k))
    | (Skip {child=achild;bits=abits;length=alength;height}, Skip {child=bchild;bits=bbits;length=blength}) ->
      let length = min alength blength in
      let ahighbits = Key.extract abits (alength - length) length in
      let bhighbits = Key.extract bbits (blength - length) length in
      let difflength = Key.numbits (Key.logxor ahighbits bhighbits) in
      let samelength = length - difflength in
      let sameheight = height - samelength in
      let (samebits, isame, samek) =
        if samelength = 0 then
          (Key.zero, i, k)
        else
          let samebits = Key.extract ahighbits (length - samelength) samelength in
          let isame = Key.logor i (Key.shift_left samebits sameheight) in
          let samek = fun child -> skipk isame height samelength samebits child k in
          (samebits, isame, samek) in
      let adifflength = alength - samelength in
      let bdifflength = blength - samelength in
      if difflength = 0 then
        recursek isame
          ((make_skip sameheight adifflength abits achild),
           (make_skip sameheight bdifflength bbits bchild)) samek
      else
        let (aleft, aright) = skip_choice height alength abits achild (adifflength - 1) in
        let (bleft, bright) = skip_choice height blength bbits bchild (bdifflength - 1) in
        recursek isame (aleft, bleft) (fun left ->
          recursek (right_index isame (sameheight - 1)) (aright, bright) (fun right ->
            branchk isame sameheight left right samek))
    | _ -> raise (Internal_error "co_match")

  let merge f a b =
    let (a, b) = ensure_same_height a b in
    let rec m i (a, b) k =
      co_match
        m
        (fun _ height left right k -> k (make_branch height left right))
        (fun _ height length bits child k -> k (make_skip height length bits child))
        (fun i va vb k -> k (match (f i (Some va) (Some vb)) with None -> Empty | Some v -> mk_leaf v))
        (fun i ta k -> k (mapiopt (fun _ v -> f i (Some v) None) ta))
        (fun i tb k -> k (mapiopt (fun _ v -> f i None (Some v)) tb))
        i (a, b) k in
    m Key.zero (a, b) identity

  let compare cmp a b =
    let (a, b) = ensure_same_height a b in
    let rec m i (a, b) k =
      co_match
        m
        (fun _ _ _ _ k -> k ())
        (fun _ _ _ _ _ k -> k ())
        (fun _ va vb k -> let r = cmp va vb in if r = 0 then k () else r)
        (fun _ ta k -> 1)
        (fun _ tb k -> -1)
        i (a, b) k in
    m Key.zero (a, b) (konstant 0)

  let equal eq a b =
    let rec loop i (a, b) k =
      if a == b then k () else
        co_match
          loop
          (fun _ _ _ _ k -> k ())
          (fun _ _ _ _ _ k -> k ())
          (fun _ va vb k -> if eq va vb then k () else false)
          (fun _ ta k -> false)
          (fun _ tb k -> false)
          i (a, b) k in
    loop Key.zero (a, b) (konstant true)

  let union f a b =
    let f' i a b = match (a, b) with
      | None, None -> None
      | (Some v), None -> Some v
      | None, (Some v) -> Some v
      | (Some va), (Some vb) -> f i va vb in
    merge f' a b
end

module type TrieSynthMerkleS = sig
  include TrieSkipSynthS
  val leaf_digest : Digest.t -> t
end

module TrieSynthMerkle (Key : UnsignedS) (Value : DigestibleS) =
struct
  type key = Key.t
  type value = Value.t
  type t = Digest.t
  let empty = Digest.zero
  let leaf_digest digest = Digest.make (1, digest)
  let leaf v = leaf_digest (Digest.make v)
  let branch h x y = Digest.make (2, h, x, y)
  let skip height length bits child = Digest.make (3, height, length, bits, child)
end
module MerkleTrie (Key : UnsignedS) (Value : DigestibleS) = struct
  module Synth = TrieSynthMerkle (Key) (Value)
  include Trie (Key) (Value) (Synth)

  let trie_digest = get_synth

  let path_digest = path_map trie_digest

  let get_proof (key: key) (trie: trie) : (key*Digest.t*Digest.t*(Digest.t step list)) option =
    match find_path key trie with
    | Leaf {value}, up -> Some (key, trie_digest trie, Digest.make value, (path_digest up).steps)
    | _ -> None

  (** Check the consistency of a Proof.
      1- starting from the leaf_digest of the value's digest and applying the path,
      we should arrive at the top trie's hash.
      2- starting from the key, the path should follow the key's bits.
  *)
  let check_proof_consistency (key, trie_d, value_d, steps_d) =
    let path_d = {index=key;height=0;steps=steps_d} in
    check_path_consistency path_d
    && let (top_d, height) = path_apply Synth.branch Synth.skip path_d (Synth.leaf_digest value_d, 0) in
    trie_d = top_d
    && height >= Key.numbits key
end

module Test = struct
  let generic_compare = compare
  module MyTrie = Trie (Nat) (StringT) (TrieSynthCardinal (Nat) (StringT))
  include MyTrie

  let rec print_trie out_channel = function
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
        height length (Nat.to_string bits) ;
      print_trie out_channel child ; Printf.fprintf out_channel "}"

  let n = Nat.of_int
  let s = Nat.to_string
  let println s = Printf.printf "%s\n" s
  let showln x = print_trie stdout x ; Printf.printf "\n"

  let verify x =
    if check_invariant x then
      x
    else
      (showln x ;
       (raise (Internal_error "invariant failed")))

  let sort_bindings bindings = List.sort generic_compare bindings
  let make_bindings n f = List.init n (fun i -> let j = i + 1 in (Nat.of_int j, f j))
  let bindings_equal x y = (sort_bindings x) = (sort_bindings y)

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

  let is_even z = Nat.equal (Nat.extract z 0 1) Nat.zero

  let bindings_4 : (Nat.t * string) list = make_bindings 4 string_of_int
  let trie_4 = of_bindings bindings_4

  let bindings_10 : (Nat.t * string) list = make_bindings 10 string_of_int
  let bindings_100 : (Nat.t * string) list = make_bindings 100 string_of_int

  let bindings_1 = shuffle_list bindings_100
  let trie_1 = verify (of_bindings bindings_1)

  let bindings_2 = List.filter (zcompose is_even fst) bindings_100
  let trie_2 = verify (of_bindings bindings_2)

  let bindings_3 = List.filter (zcompose (fun s -> String.get s 0 = '6') snd) bindings_100
  let trie_3 = verify (of_bindings bindings_3)

  let bindings_5 = List.filter (fun (i,s) -> is_even i && String.get s 0 = '6') bindings_100
  let trie_5 = verify (of_bindings bindings_5)

  let bindings_10_12_57 = [(n 57, "57");(n 10, "10");(n 12, "12")]
  let trie_10_12_57 = verify (of_bindings bindings_10_12_57)

  let test_bindings = [[]; [(n 42, "x")]; bindings_10_12_57; bindings_4; bindings_10; bindings_1; bindings_2; bindings_3; bindings_5]

  let%test "empty" =
    (bindings empty = []) && (of_bindings [] = Empty)

  let%test "find_opt_some" =
    Some "12" = find_opt (n 12) trie_10_12_57

  let%test "find_opt_none" =
    None = find_opt (n 13) trie_10_12_57

  let rec intersperse separator = function
      [] -> [] | [a] -> [a] | a :: l -> a :: separator :: intersperse separator l

  let string_of_value_option = function None -> "None" | Some v -> Printf.sprintf "Some %S" v
  let string_of_binding (i, v) = Printf.sprintf "(%s,%S)" (s i) v
  let string_of_bindings (l : (key*value) list) =
    Printf.sprintf "[%s]" (String.concat ";" (List.map string_of_binding l))

  let for_all_bindings name p =
    List.for_all
      (fun b ->
         let trie = of_bindings b in
         p (b, trie) ||
         (Printf.printf "BAD %s b=%s trie=" name (string_of_bindings b) ;
          showln trie ; raise (Internal_error (Printf.sprintf "bad %s" name))))
      test_bindings

  let%test "find_opt_all" =
    for_all_bindings "find_opt_all"
      (fun (b, trie) -> List.for_all (fun (i, v) -> Some v = find_opt i trie) b)

  let%test "find_found" =
    "12" = find (n 12) trie_10_12_57

  let%test "find_not_found" =
    throws Not_found (fun _ -> find (n 13) trie_10_12_57)

  let%test "find_all" =
    for_all_bindings "find_all"
      (fun (b, trie) -> List.for_all (fun (i, v) -> v = find i trie) b)

  let%test "mem_true" =
    true = mem (n 12) trie_10_12_57

  let%test "mem_false" =
    false = mem (n 13) trie_10_12_57

  let%test "mem_all" =
    for_all_bindings "mem_all"
      (fun (b, trie) -> List.for_all (fun (i, _) -> mem i trie) b)

  let%test "bindings of_bindings" =
    for_all_bindings "bindings of_bindings"
      (fun (b, trie) -> bindings trie = sort_bindings b)

  let%test "min_binding" =
    throws Not_found (fun _ -> min_binding empty)
    && min_binding (singleton (n 42) "x") = ((n 42), "x")
    && min_binding trie_1 = ((n 1), "1")
    && min_binding trie_2 = ((n 2), "2")
    && min_binding trie_3 = ((n 6), "6")
    && min_binding trie_4 = ((n 1), "1")
    && min_binding trie_5 = ((n 6), "6")
    && min_binding trie_10_12_57 = ((n 10), "10")

  let%test "max_binding" =
    throws Not_found (fun _ -> max_binding empty)
    && max_binding (singleton (n 42) "x") = ((n 42), "x")
    && max_binding trie_1 = ((n 100), "100")
    && max_binding trie_2 = ((n 100), "100")
    && max_binding trie_3 = ((n 69), "69")
    && max_binding trie_4 = ((n 4), "4")
    && max_binding trie_5 = ((n 68), "68")
    && max_binding trie_10_12_57 = ((n 57), "57")

  let%test "cardinal" =
    for_all_bindings "cardinal"
      (fun (b, t) -> cardinal t = List.length b)

  let%test "add 0" =
    for_all_bindings "add"
      (fun (b, trie) ->
         let trie0 = add (n 0) "0" trie in
         None = find_opt (n 0) trie
         && "0" = find (n 0) trie0
         && cardinal trie0 = 1 + List.length b)

  let%test "add 2" =
    bindings (add (n 2) "2" (of_bindings [((n 1), "1");((n 3), "3")])) = make_bindings 3 string_of_int

  let%test "remove" =
    bindings (remove (n 12) trie_10_12_57) = [((n 10), "10");((n 57),"57")]

  let%test "remove_all" =
    for_all_bindings "remove_all"
      (fun (b, trie) -> empty = List.fold_right remove (List.map fst b) trie)

  let%test "equal shuffle" =
    for_all_bindings "equal shuffle"
      (fun (b, trie) -> equal (=) trie (of_bindings (shuffle_list b)))

  let%test "unequal" =
    not (equal (=) trie_4 trie_1)

end

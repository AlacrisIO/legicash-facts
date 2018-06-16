(* Big Endian Patricia Trees (Tries)
   See article "Fast Mergable Integer Maps" by Chris Okasaki & Andrew Gill, 1998
   http://www.eecs.usma.edu/webs/people/okasaki/ml98maps.ps
*)

(* A big endian patricia tree maps non-negative integers to values.
*)
open Lib

type key = Z.t

type (+'a) t =
  | Empty
  | Leaf of 'a
  | Branch of {left: 'a t; right: 'a t; is_full: bool; height: int}
  | Skip of {child: 'a t; bits: Z.t; length: int; height: int}

let trie_height = function
  | Empty -> -1
  | Leaf _ -> 0
  | Branch {height} -> height
  | Skip {height} -> height

let rec check_invariant must_be_full = function
  | Empty -> (not must_be_full)
  | Leaf _ -> true
  | Branch {left; right; is_full; height} ->
    (not must_be_full || is_full)
    && height - 1 = trie_height left
    && check_invariant is_full left
    && height - 1 = trie_height right
    && check_invariant is_full right
  | Skip {child; bits; length; height} ->
    not must_be_full
    && length > 0
    && height >= length
    && Z.sign bits >= 0
    && length >= Z.numbits bits
    && height - length = trie_height child
    && check_invariant false child

let empty = Empty

(* Is this trie empty, i.e. having no mapping from index to value? *)
let is_empty x = x = Empty

(* Is this trie full, i.e. non-empty and mapping every index to a value? *)
let is_full = function
  | Empty -> false
  | Leaf _ -> true
  | Skip _ -> false
  | Branch {is_full} -> is_full

let z_has_bit l position = Z.equal Z.one (Z.extract l position 1)

let find_opt l t =
  let h = trie_height t in
  if Z.sign l < 0 || Z.numbits l > h then
    None
  else
    let rec f h = function
      | Empty -> None
      | Leaf v -> Some v
      | Skip {child; bits; length} ->
        let g = h - length in
        if Z.equal bits (Z.extract l g length)
        then f g child
        else None
      | Branch {left; right; is_full; height} ->
        let g = h - 1 in
        let child = if z_has_bit l g then right else left in
        f g child
    in
    f h t

let find l t = unwrap_option (find_opt l t)

let mem l t = is_option_some (find_opt l t)

let make_leaf height l v =
  if height = 0 then
    Leaf v
  else
    Skip {child= Leaf v; bits= Z.extract l 0 height; length= height; height= height}

let make_skip height length bits child =
  if length = 0 then child else
    match child with
    | Empty -> Empty
    | Skip {child; bits; length} ->
      let new_length = length + length in
      Skip {child;
            bits= Z.logor (Z.shift_left bits length) bits;
            length= new_length;
            height}
    | _ -> Skip {child; bits= bits; length= length; height}

let make_branch height left right =
  if is_empty right then
    make_skip height 1 Z.zero left
  else if is_empty left then
    make_skip height 1 Z.one right
  else Branch {left; right; is_full= is_full left && is_full right; height}

let make_head = function
  | Skip {child; bits; length; height} as x ->
    let n = Z.numbits bits in
    if n = 0 then
      child
    else if n < length then
      Skip {child; bits; length= n; height= height + n - length}
    else x
  | x -> x

let singleton l v = make_leaf (Z.numbits l) l v

let add l v t =
  assert (0 <= Z.sign l); (* we only support non-negative keys (for now) *)
  let n = Z.numbits l in
  if is_empty t then
    make_leaf n l v (* singleton *)
  else
    let h = trie_height t in
    if n > h then
      make_branch n (make_skip (n - 1) (n - h - 1) Z.zero t) (make_leaf (n - 1) l v)
    else
      let rec ins height = function
        | Empty -> make_skip height height l (Leaf v)
        | Leaf _ -> Leaf v
        | Skip {child; bits; length} ->
          let g = height - length in
          let l_bits = Z.extract l g length in
          if Z.equal bits l_bits then
            make_skip height length bits (ins g child)
          else
            let diff_length = Z.numbits (Z.logxor bits l_bits) in
            let same_length = length - diff_length in
            let len = diff_length - 1 in
            let h = g + len in
            let t1 = make_skip h diff_length (Z.extract bits 0 diff_length) child in
            let leaf = make_leaf h l v in
            let branch = if z_has_bit bits len then
                make_branch (h + 1) leaf t1
              else
                make_branch (h + 1) t1 leaf in
            make_skip height same_length (Z.shift_right bits diff_length) branch
        | Branch {left; right} ->
          let h = height - 1 in
          if z_has_bit l h then
            make_branch height left (ins h right)
          else
            make_branch height (ins h left) right in
      ins h t

let remove l t =
  if not (mem l t) then t else
    let rec drop h = function
      | Empty -> Empty
      | Leaf _ -> Empty
      | Branch {left; right} ->
        let g = h - 1 in
        if z_has_bit l g then
          make_branch h left (drop g right)
        else
          make_branch h (drop g left) right
      | Skip {child; bits; length} ->
        make_skip h length bits (drop (h - length) child)
    in drop (trie_height t) t

let skip_base base bits length height =
  (Z.add base (Z.shift_left bits (height - length)))

let right_base base height =
  (Z.add base (Z.shift_left Z.one (height - 1)))

let rec map f = function
  | Empty -> Empty
  | Leaf v -> Leaf (f v)
  | Branch {left; right; is_full; height} ->
    Branch {left= map f left; right= map f right; is_full; height}
  | Skip {child; bits; length; height} ->
    Skip {child= map f child; bits; length; height}

let mapi f t =
  let rec maprec base = function
    | Empty -> Empty
    | Leaf v -> Leaf (f base v)
    | Branch {left; right; is_full; height} ->
      Branch {left= maprec base left;
              right= maprec (right_base base height) right;
              is_full; height}
    | Skip {child; bits; length; height} ->
      Skip {child= maprec (skip_base base bits length height) child;
            bits; length; height}
  in
  maprec Z.zero t

let mapiopt f t =
  let rec m i = function
    | Empty -> Empty
    | Leaf v -> (match (f i v) with None -> Empty | Some w -> Leaf w)
    | Branch {left; right; height} ->
      make_branch height (m i left) (m (right_base i height) right)
    | Skip {child; bits; length; height} ->
      make_skip height length bits (m (skip_base i bits length height) child)
  in
  m Z.zero t


(* A fold left in continuation-passing style, that allows for functional escapes.
   It trivializes fold, iter, for_all, exists, {{min,max}_binding,find_first}{,_opt} *)
let foldlk f t acc k =
  let rec frec base acc k = function
    | Empty -> k acc
    | Leaf v -> (f base v acc k)
    | Branch {left; right; is_full; height} ->
      frec base acc (fun acc -> frec (right_base base height) acc k right) left
    | Skip {child; bits; length; height} ->
      frec (skip_base base bits length height) acc k child
  in
  frec Z.zero acc k t

(* A fold right in continuation-passing style, that allows for functional escapes *)
let foldrk f t acc k =
  let rec frec base acc k = function
    | Empty -> k acc
    | Leaf v -> (f base v acc k)
    | Branch {left; right; is_full; height} ->
      frec (right_base base height) acc (fun acc -> frec base acc k left) right
    | Skip {child; bits; length; height} ->
      frec (skip_base base bits length height) acc k child
  in
  frec Z.zero acc k t

let fold f t acc =
  foldlk (fun i v acc k -> k (f i v acc)) t acc identity

let iter f t =
  foldlk (fun i v () k -> f i v; k ()) t () identity

let rec fold_reverse f t acc =
  foldrk (fun i v acc k -> k (f i v acc)) t acc identity

let for_all p t =
  foldlk (fun i v b k -> if p i v then k b else false) t true identity

let exists p t =
  foldlk (fun i v b k -> if p i v then true else k b) t false identity

let filter p t =
  let rec filterrec base = function
    | Empty -> (Empty, true)
    | Leaf v as x -> if (p base v) then (x, true) else (Empty, false)
    | Branch {left; right; is_full; height} as x ->
      let newleft, sameleft = filterrec base left in
      let newright, sameright = filterrec (right_base base height) right in
      if sameleft && sameright then (x, true) else (make_branch height newleft newright, false)
    | Skip {child; bits; length; height} as x ->
      let newchild, samechild = filterrec (skip_base base bits length height) child in
      if samechild then (x, true) else (make_skip height length bits newchild, false)
  in
  fst (filterrec Z.zero t)

let numbits_max_int = Z.numbits (Z.of_int max_int)

let rec cardinal = function
  | Empty -> 0
  | Leaf _ -> 1
  | Branch {left; right; is_full; height} ->
    if is_full then
      (assert (height < numbits_max_int) ; 1 lsl height)
    else
      (cardinal left) + (cardinal right)
  | Skip {child} -> cardinal child

let bindings t = fold_reverse (fun i v acc -> (i, v) :: acc) t []

let min_binding_opt t =
  foldlk (fun i v acc k -> Some (i, v)) t None identity

let min_binding t = unwrap_option (min_binding_opt t)

let max_binding_opt t =
  foldrk (fun i v acc k -> Some (i, v)) t None identity

let max_binding t = unwrap_option (max_binding_opt t)

let choose t = min_binding t

let choose_opt t = min_binding_opt t

let find_first_opt_k f i v acc k = if f i then Some (i, v) else k acc

let find_first_opt f t = foldlk (find_first_opt_k f) t None identity

let find_first f t = unwrap_option (find_first_opt f t)

let find_last_opt f t = foldrk (find_first_opt_k f) t None identity

let find_last f t = unwrap_option (find_last_opt f t)

let partition p t =
  let rec prec base = function
    | Empty -> (Empty, Empty)
    | Leaf v as x -> if p base v then (x, Empty) else (Empty, x)
    | Branch {left; right; height} ->
      let (ly, ln) = prec base left in
      let (ry, rn) = prec (right_base base height) right in
      (make_branch height ly ry, make_branch height ln rn)
    | Skip {child; bits; length; height} ->
      let (l, r) = prec (skip_base base bits length height) child in
      let f = make_skip height length bits in
      (f l, f r)
  in
  prec Z.zero t

let split k t =
  if Z.sign k < 0 then
    (Empty, None, t)
  else if Z.numbits k > trie_height t then
    (t, None, Empty)
  else
    let rec srec base = function
      | Empty -> (Empty, None, Empty)
      | Leaf v -> (Empty, Some v, Empty)
      | Branch {left; right; height} ->
        if z_has_bit k (height - 1) then
          let (l, x, r) = srec (right_base base height) right in
          (make_branch height left l, x, make_skip height 1 Z.one r)
        else
          let (l, x, r) = srec base left in
          (make_skip height 1 Z.zero left, x, make_branch height r right)
      | Skip {child; bits; length; height} ->
        let (l, x, r) = srec (skip_base base bits length height) child in
        let f = make_skip height length bits in
        (f l, x, f r)
    in
    srec Z.zero t

let ensure_height h t =
  let th = trie_height t in
  if th < h then
    make_skip h (h - th) Z.zero t
  else
    t

let ensure_same_height ta tb =
  (ensure_height (trie_height tb) ta, ensure_height (trie_height ta) tb)

type 'a trie = 'a t
let trie_map = map

module Path : sig
  module Step : sig
    type (+'a) t =
      | LeftBranch of {right: 'a trie}
      | RightBranch of {left: 'a trie}
      | SkipChild of {bits: Z.t; length: int}
    val apply : int -> 'a t -> 'a trie -> 'a trie
    val map : ('a -> 'b) -> 'a t -> 'b t
  end

  (** 'a Path.t describes how to focus on a subtrie of a trie *)
  type (+'a) t = {base: Z.t; height: int; steps: 'a Step.t list}

  (** a Path.zipper is a pair of a focused subtrie and a Path.t,
      from which to retrieve the complete trie *)
  type (+'a) zipper = 'a trie * 'a t

  (** given a trie, return the zipper for the top of trie *)
  val zip : 'a trie -> 'a zipper

  (** apply a path to a focused subtrie to retrive the complete trie *)
  val unzip : 'a zipper -> 'a trie

  (** paths are a functor *)
  val map: ('a -> 'b) -> 'a t -> 'b t

  (** Given a focus on a subtrie, return focuses on the next level of subtries *)
  val next: 'a zipper -> 'a zipper list
end = struct
  module Step = struct
    type (+'a) t =
      | LeftBranch of {right: 'a trie}
      | RightBranch of {left: 'a trie}
      | SkipChild of {bits: Z.t; length: int}
    let apply height step trie =
      match step with
      | LeftBranch {right} -> (make_branch height trie right)
      | RightBranch {left} -> (make_branch height left trie)
      | SkipChild {bits; length} -> (make_skip height length bits trie)
    let map f = function
      | LeftBranch {right} -> LeftBranch {right= trie_map f right}
      | RightBranch {left} -> RightBranch {left= trie_map f left}
      | SkipChild _ as s -> s
  end
  type (+'a) t = {base: Z.t; height: int; steps: 'a Step.t list}
  type (+'a) zipper = 'a trie * 'a t

  let zip t = (t, {base=Z.zero; height=trie_height t; steps=[]})

  let unzip (t, {height;steps}) =
    fst (List.fold_left
           (fun (t, h) s -> (Step.apply h s t, h + 1))
           (ensure_height height t, height)
           steps)

  let map f {base;height;steps} =
    {base;height;steps= List.map (Step.map f) steps}

  let next = function
    | (Empty, up) -> []
    | (Leaf _, up) -> []
    | (Branch {left; right}, {base; height; steps}) ->
      let height = height - 1 in
      let rbase = right_base base height in
      [(left, {base; height; steps= LeftBranch {right} :: steps});
       (right, {base= rbase; height; steps= RightBranch {left} :: steps})]
    | (Skip {child; bits; length}, {base; height; steps}) ->
      let base = skip_base base bits length height in
      [(child, {base; height=height-length; steps= SkipChild {bits; length} :: steps})]

(*
     let traversek f t k =
     let rec trec k = xxx in
     trec k

     let rec traversek kempty kleaf kbranch kskip k base = function
     | Empty -> kempty base k
     | Leaf v -> kleaf v base k
     | Branch {left; right; is_full; height} ->
     let rbase = right_base base height in
     kbranch
     (fun l r -> make_branch height l r)
     (fun k -> traversek kempty kleaf kbranch kskip k base left)
     (fun k -> traversek kempty kleaf kbranch kskip k rbase right)
     left right is_full height base rbase k
     (* The Skip case is really ugly. TODO: make it nice? *)
     | Skip {child; bits; length; height} ->
     let sbase = (skip_base base bits length height) in
     kskip
     (fun c -> make_skip height length bits c)
     (fun k -> traversek kempty kleaf kbranch kskip k sbase child)
     child bits length height base sbase k
  *)
end

let find_path l t =
  let h = trie_height t in
  if Z.sign l < 0 then
    raise (Internal_error "find_path negative index")
  else
    let height = Z.numbits l in
    if Z.numbits l > h then
      let hh = height - 1 in
      let left = Skip {child=t; height=hh; bits=Z.zero; length= hh-h} in
      (Empty, {Path.base=Z.shift_left Z.zero hh; Path.height=hh; Path.steps=[RightBranch{left}]})
    else
      let rec f x = match x with
        | (Empty, _) -> x
        | (Leaf _, _) -> x
        | (Skip {child; bits; length; height}, {Path.base; Path.steps}) ->
          let height = height - length in
          if Z.equal bits (Z.extract l height length) then
            let base=skip_base base bits length height in
            f (child, {base;height;steps=SkipChild{bits; length}::steps})
          else x
        | (Branch {left; right; height}, {Path.base; Path.steps}) ->
          let height = height - 1 in
          if z_has_bit l height then
            f (right, {base= right_base base height; height; steps=RightBranch{left}::steps})
          else
            f (left, {base; height; steps=LeftBranch{right}::steps})
      in
      f (Path.zip t)

(* update, using paths *)
let update l f t =
  let (sub, up) = find_path l t in
  let o = match t with Leaf v -> Some v | _ -> None in
  let u = f o in
  if o = u then t else
    let ll = Z.extract l 0 up.height in
    let newsub =
      match u with
      | None -> remove ll sub
      | Some v -> add ll v sub in
    Path.unzip (newsub, up)

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
  if z_has_bit bits sublength then
    (Empty, make_skip (height - length + sublength) sublength (Z.extract bits 0 sublength) child)
  else
    (make_skip (height - length + sublength) sublength (Z.extract bits 0 sublength) child, Empty)

let comatch
      (recursek: Z.t -> 'a t * 'b t -> ('c -> 'r) -> 'r)
      (branchk: Z.t -> int -> 'c -> 'c -> ('c -> 'r) -> 'r)
      (skipk: Z.t -> int -> int -> Z.t -> 'c -> ('c -> 'r) -> 'r)
      (leafk: Z.t -> 'a -> 'b -> ('c -> 'r) -> 'r)
      (onlyak: Z.t -> 'a t -> ('c -> 'r) -> 'r)
      (onlybk: Z.t -> 'b t -> ('c -> 'r) -> 'r)
      (i : Z.t)
      ((a : 'a t), (b : 'b t))
      (k : 'c -> 'r) = match (a, b) with
  | (_, Empty) -> onlyak i a k
  | (Empty, _) -> onlybk i b k
  | (Leaf va, Leaf vb) -> leafk i va vb k
  | (Branch {left=aleft; right=aright; height}, Branch {left=bleft; right=bright}) ->
    recursek i (aleft, bleft) (fun left ->
      recursek (right_base i height) (aright, bright) (fun right ->
        branchk i height left right k))
  | (Branch {left=left; right=right}, Skip {child; bits; length; height}) ->
    let l1 = length - 1 in
    let bits1 = Z.extract bits 0 l1 in
    let ri = right_base i height in
    if z_has_bit bits l1 then
      onlyak i left (fun left ->
        recursek ri (right, make_skip height l1 bits1 child) (fun right ->
          branchk i height left right k))
    else
      recursek i (left, make_skip height l1 bits1 child) (fun left ->
        onlyak ri right (fun right ->
          branchk i height left right k))
  | (Skip {child;bits;length;height}, Branch {left;right}) ->
    let l1 = length - 1 in
    let bits1 = Z.extract bits 0 l1 in
    let ri = right_base i height in
    if z_has_bit bits l1 then
      onlybk i left (fun left ->
        recursek ri (make_skip height l1 bits1 child, right) (fun right ->
          branchk i height left right k))
    else
      recursek i (make_skip height l1 bits1 child, left) (fun left ->
        onlybk ri right (fun right ->
          branchk i height left right k))
  | (Skip {child=achild;bits=abits;length=alength;height}, Skip {child=bchild;bits=bbits;length=blength}) ->
    let length = min alength blength in
    let ahighbits = Z.extract abits (alength - length) length in
    let bhighbits = Z.extract bbits (blength - length) length in
    let difflength = Z.numbits (Z.logxor ahighbits bhighbits) in
    let samelength = length - difflength in
    let sameheight = height - samelength in
    let samebits = Z.extract abits (alength - samelength) samelength in
    let isame = Z.logor i (Z.shift_left samebits (height - samelength)) in
    let samek = if samelength = 0 then k else
        fun child -> skipk isame height samelength samebits child k in
    let adifflength = alength - samelength in
    let bdifflength = blength - samelength in
    if difflength = 0 then
      recursek isame
        ((make_skip sameheight adifflength (Z.extract abits 0 adifflength) achild),
         (make_skip sameheight bdifflength (Z.extract bbits 0 bdifflength) bchild)) samek
    else
      let (aleft, aright) = skip_choice height alength abits achild (adifflength - 1) in
      let (bleft, bright) = skip_choice height blength bbits bchild (bdifflength - 1) in
      recursek isame (aleft, bleft) (fun left ->
        recursek (right_base isame (sameheight - 1)) (aright, bright) (fun right ->
          branchk isame sameheight left right samek))
  | _ -> raise (Internal_error "comatch")

let merge f a b =
  let (a, b) = ensure_same_height a b in
  let rec m i (a, b) k =
    comatch
      m
      (fun _ height left right k -> k (make_branch height left right))
      (fun _ height length bits child k -> k (make_skip height length bits child))
      (fun i va vb k -> k (match (f i (Some va) (Some vb)) with None -> Empty | Some v -> Leaf v))
      (fun i ta k -> k (mapiopt (fun _ v -> f i (Some v) None) ta))
      (fun i tb k -> k (mapiopt (fun _ v -> f i None (Some v)) tb))
      i (a, b) k in
  m Z.zero (a, b) identity

let compare cmp a b =
  let (a, b) = ensure_same_height a b in
  let rec m i (a, b) k =
    comatch
      m
      (fun _ _ _ _ k -> k ())
      (fun _ _ _ _ _ k -> k ())
      (fun _ va vb k -> let r = cmp va vb in if r = 0 then k () else r)
      (fun _ ta k -> 1)
      (fun _ tb k -> -1)
      i (a, b) k in
  m Z.zero (a, b) (konstant 0)

let equal eq a b =
  let (a, b) = ensure_same_height a b in
  let rec m i (a, b) k =
    if a == b then k () else
      comatch
        m
        (fun _ _ _ _ k -> k ())
        (fun _ _ _ _ _ k -> k ())
        (fun _ va vb k -> if eq va vb then k () else false)
        (fun _ ta k -> false)
        (fun _ tb k -> false)
        i (a, b) k in
  m Z.zero (a, b) (konstant true)

let union f a b =
  let f' i a b = match (a, b) with
    | None, None -> None
    | (Some v), None -> Some v
    | None, (Some v) -> Some v
    | (Some va), (Some vb) -> f i va vb in
  merge f' a b

(* Big Endian Patricia Trees (Tries)
   See article "Fast Mergable Integer Maps" by Chris Okasaki & Andrew Gill, 1998
   http://www.eecs.usma.edu/webs/people/okasaki/ml98maps.ps
*)

(* TODO:
   1- don't use Synth for digest anymore, since we use a Wrap-per instead.
   2- if we use Synth for anything else, it should be marshaled in and out.
*)

(* A big endian patricia tree maps non-negative integers to values.
*)
open Lwt.Infix

open Lib
open Lazy
open Yojsoning
open Marshaling
open Tag
open Digesting
open Persisting
open Types
open Trie

module type TrieSynthMerkleS = sig
  include TrieSynthS with type t = digest
  val marshal_empty : unit marshaler
  val marshal_leaf : value marshaler
  val marshal_branch : (int*digest*digest) marshaler
  val marshal_skip : (int*int*key*digest) marshaler
end

module TrieSynthMerkle (Key : UIntS) (Value : PersistableS) = struct
  type key = Key.t
  type value = Value.t
  type t = digest
  let marshal_empty buffer _ =
    Tag.marshal buffer Tag.empty
  let marshal_leaf buffer value =
    Tag.marshal buffer Tag.leaf;
    Value.marshal buffer value
  let marshal_branch buffer (height, left, right) =
    Tag.marshal buffer Tag.branch;
    UInt16int.marshal buffer height;
    Digest.marshal buffer left;
    Digest.marshal buffer right
  let marshal_skip buffer (height, length, bits, child) =
    Tag.marshal buffer Tag.skip;
    UInt16int.marshal buffer height;
    UInt16int.marshal buffer length;
    Key.marshal buffer bits;
    Digest.marshal buffer child

  let empty = digest_of_marshal marshal_empty ()
  let leaf value = digest_of_marshal marshal_leaf value
  let branch height left right = digest_of_marshal marshal_branch (height, left, right)
  let skip height length key child = digest_of_marshal marshal_skip (height, length, key, child)
end

module type MerkleTrieProofS = sig
  type key
  type value
  type mtrie
  type 'a pstep
  type t =
    { key : key
    ; trie : Digest.t
    ; leaf : Digest.t
    ; steps : (Digest.t pstep) list
    }
  val get : key -> mtrie -> t option
  val check : t -> mtrie -> key -> value -> bool
  include YojsonableS with type t := t
end

module type MerkleTrieS = sig
  type key
  type value
  (* [Synth.t = unit] because we want to be able to use the [TrieS] tree-walking
     functionality for lazy DB access, here, without invoking its potentially
     expensive recursive computation capabilities. Merkle digests are computed
     using [SynthMerkle], instead.

     This arrangement is potentially faster when batching updates to the DB.
     Instead of computing the merkle root for each constituent update during a
     DB transaction involving many batched updates, we can ask for the digest
     computation at the end, potentially avoiding hundreds of digests which
     would only be over-written during intermediate states. *)
  module Synth : TrieSynthS with type t = unit and type key = key and type value = value
  module SynthMerkle : TrieSynthMerkleS with type key = key and type value = value
  module Type : TrieTypeS with type key = key and type value = value
  include TrieS
    with type key := key
     and type value := value
     and type 'a wrap = 'a dv
  module Wrap : WrapS with type value = trie and type t = t
  include PersistableS
    with type t := t

  val trie_digest : t -> digest
  val path_digest : t path -> digest path

  module Proof : MerkleTrieProofS
    with type key = key and type value = value and type mtrie = t and type 'a pstep = 'a step
end

module type MerkleTrieTypeS = sig
  include TrieTypeS
  module Trie : PersistableS with type t = trie
  module T : PersistableS with type t = t
end

module MerkleTrieType (Key : UIntS) (Value : PersistableS)
    (Synth : TrieSynthS with type key = Key.t and type value = Value.t) = struct
  include TrieType (Key) (Value) (DigestValueType) (Synth)

  let trie_tag = function
    | Leaf _ -> Tag.leaf
    | Branch _ -> Tag.branch
    | Skip _ -> Tag.skip
    | Empty -> Tag.empty

  (* Ugly: to achieve mutual definition between marshaling or trie and t,
     we side-effects that array to close the loop. *)
  let marshaling_case_table = new_marshaling_cases 4
  let t_dependency_walking = ref dependency_walking_not_implemented

  module PreTrie = struct
    type t = trie
    let marshaling = marshaling_cases trie_tag Tag.base_trie marshaling_case_table
    let make_persistent x y = normal_persistent x y
    let walk_dependencies _methods context = function
      | Leaf {value} ->
        walk_dependency Value.dependency_walking context value
      | Empty -> Lwt.return_unit
      | Branch {left; right} ->
        walk_dependency !t_dependency_walking context left
        >>= (fun () -> walk_dependency !t_dependency_walking context right)
      | Skip {child} ->
        walk_dependency !t_dependency_walking context child
    let yojsoning = {to_yojson=bottom;of_yojson=bottom}
  end
  module Trie = Persistable(PreTrie)
  module T = DigestValue(Trie)

  let _init = (* close the fixpoints *)
    init_marshaling_cases Tag.base_trie marshaling_case_table
      [(Tag.leaf,
        marshaling_map
          (function
            | Leaf {value} -> value
            | _ -> bottom ())
          trie_leaf
          Value.marshaling);
       (Tag.branch,
        marshaling3
          (function
            | Branch {left; right; height} -> (height, left, right)
            | _ -> bottom ())
          (trie_branch dv_get)
          UInt16int.marshaling T.marshaling T.marshaling);
       (Tag.skip,
        marshaling4
          (function
            | Skip {height; length; bits; child} -> (height, length, bits, child)
            | _ -> bottom ())
          (trie_skip dv_get)
          UInt16int.marshaling UInt16int.marshaling Key.marshaling T.marshaling);
       (Tag.empty,
        marshaling_map
          (fun _ -> ())
          (fun () -> Empty)
          Unit.marshaling)];
    t_dependency_walking := T.dependency_walking

  include (T : PersistableS with type t := t)
end

module MerkleTrie (Key : UIntS) (Value : PersistableS) = struct
  module Synth = TrieSynthUnit (Key) (Value)
  module SynthMerkle = TrieSynthMerkle (Key) (Value)
  module Type = MerkleTrieType (Key) (Value) (Synth)
  module Wrap = DigestValue (Type.Trie)
  module Trie = Trie (Key) (Value) (DigestValueType) (Synth) (Type) (Wrap)
  include Trie
  include (Type.T : PersistableS with type t := t)
  let to_yojson = Trie.to_yojson
  let of_yojson = Trie.of_yojson

  let check_invariant t =
    check_invariant t &&
    let c t =
      iterate_over_tree
        ~recursek:(fun ~i ~tree:t ~k ->
          if not (dv_digest t = digest t) then
            bork "Bad digest at key %s height %d digest=%s dv_digest=%s"
              (Key.to_0x_string i)
              (trie_height t)
              (Digest.to_0x_string (digest t))
              (Digest.to_0x_string (dv_digest t))
          else k())
        ~branchk:(fun ~i:_ ~height:_ ~leftr:_ ~rightr:_ ~synth:_ ~k -> k())
        ~skipk:(fun ~i:_ ~height:_ ~length:_ ~bits:_ ~childr:_ ~synth:_ ~k -> k())
        ~leafk:(fun ~i:_ ~value:_ ~synth:_ ~k -> k())
        ~emptyk:(fun ~k -> k())
        ~i:Key.zero ~tree:t ~k:identity in
    c t; true

  let trie_digest = dv_digest

  let path_digest = path_map trie_digest

  let step_to_yojson to_yojson = function
    | LeftBranch {right} ->
      `Assoc [ ("type", `String "Left")
             ; ("right", to_yojson right) ]
    | RightBranch {left} ->
      `Assoc [ ("type", `String "Right")
             ; ("left", to_yojson left) ]
    | SkipChild {bits; length} ->
      `Assoc [ ("type", `String "Skip")
             ; ("bits", Key.to_yojson bits)
             ; ("length", UInt16int.to_yojson length) ]

  let step_of_yojson_exn of_yojson_exn yojson =
    let t = yojson |> YoJson.member "type" |> YoJson.to_string in
    if t = "Left" then
      LeftBranch {right= yojson |> YoJson.member "right" |> of_yojson_exn}
    else if t = "Right" then
      RightBranch {left= yojson |> YoJson.member "left" |> of_yojson_exn}
    else if t = "Skip" then
      let bits = yojson |> YoJson.member "bits" |> Key.of_yojson_exn in
      let length = yojson |> YoJson.member "length" |> UInt16int.of_yojson_exn in
      SkipChild {bits; length}
    else bork "Bad json"

  module Proof = struct
    type nonrec key = key
    type nonrec value = value
    type mtrie = t
    type 'a pstep = 'a step
    type t =
      { key : key
      ; trie : Digest.t
      ; leaf : Digest.t
      ; steps : (Digest.t pstep) list }

    let get (key: key) (mt: mtrie) : t option =
      match map_fst Wrap.get (find_path key mt) with
      | Leaf {value}, up -> Some { key
                                 ; trie = dv_digest mt
                                 ; leaf = SynthMerkle.leaf value
                                 ; steps = (path_digest up).steps }
      | _ -> None

    (** Check a proof.
        1- starting from the digest of the leaf and applying the path,
        we    should arrive at the top trie's hash.
        2- st arting from the key, the path should follow the key's bits.
    *)
    let check proof mtrie key value =
      (proof.leaf = SynthMerkle.leaf value)
      && (proof.trie = dv_digest mtrie)
      && (Key.compare key proof.key = 0)
      && let path_d = {costep={index=proof.key;height=0};steps=proof.steps} in
      (check_path_consistency path_d)
      && let (top_d, {height; index}) =
           path_apply
             (symmetric_unstep ~branch:(konstant SynthMerkle.branch) ~skip:(konstant SynthMerkle.skip))
             proof.leaf
             path_d in
      (proof.trie = top_d)
      && (height >= Key.numbits proof.key)
      && (Key.sign index = 0)

    include (Yojsonable(struct
               type nonrec t = t
               let to_yojson {key; trie; leaf; steps} =
                 `Assoc
                   [ ("key", Key.to_yojson key)
                   ; ("trie", Digest.to_yojson trie)
                   ; ("leaf", Digest.to_yojson leaf)
                   ; ("steps", `List (List.map (step_to_yojson Digest.to_yojson) steps)) ]
               let of_yojson_exn yojson =
                 { key = yojson |> YoJson.member "key" |> Key.of_yojson_exn
                 ; trie = yojson |> YoJson.member "trie" |> Digest.of_yojson_exn
                 ; leaf = yojson |> YoJson.member "leaf" |> Digest.of_yojson_exn
                 ; steps = yojson |> YoJson.member "steps" |> YoJson.to_list |>
                           List.map (step_of_yojson_exn Digest.of_yojson_exn) }
               let of_yojson = of_yojson_of_of_yojson_exn of_yojson_exn
               let yojsoning = {to_yojson;of_yojson}
             end) : YojsonableS with type t := t)
  end
end

module type MerkleTrieSetProofS = sig
  type elt
  type mts
  type 'a pstep
  type t =
    { elt : elt
    ; trie : Digest.t
    ; steps : (Digest.t pstep) list }
  val get : elt -> mts -> t option
  val check : t -> mts -> elt -> bool
  include YojsonableS with type t := t
end

module type MerkleTrieSetS = sig
  type elt
  module T : MerkleTrieS with type key = elt and type value = unit
  include PersistableS with type t = T.t
  include Set.S with type elt := elt and type t := t
  module Proof : MerkleTrieSetProofS
    with type elt = elt and type mts = t and type 'a pstep = 'a T.step
  val trie_digest : t -> Digest.t
  val lens : elt -> (t, bool) Lens.t
end

module MerkleTrieSet (Elt : UIntS) = struct
  type elt = Elt.t
  module T = MerkleTrie (Elt) (Unit)
  include (T : PersistableS with type t = T.t)

  let wrap f elt _ = f elt

  let trie_digest = T.trie_digest

  let empty = T.empty
  let is_empty = T.is_empty
  let mem elt t = Option.is_some (T.find_opt elt t)
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
  let min_elt_opt t = Option.map fst (T.min_binding_opt t)
  let max_elt t = fst (T.max_binding t)
  let max_elt_opt t = Option.map fst (T.max_binding_opt t)
  let choose = min_elt
  let choose_opt = min_elt_opt
  let split elt t = match T.split elt t with (a, v, b) -> (a, Option.is_some v, b)
  let find_opt elt t = if (mem elt t) then Some elt else None
  let find elt t = Option.get (find_opt elt t)
  let find_first_opt f t = Option.map fst (T.find_first_opt f t)
  let find_first f t = Option.get (find_first_opt f t)
  let find_last_opt f t = Option.map fst (T.find_last_opt f t)
  let find_last f t = Option.get (find_last_opt f t)
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

  let lens k = Lens.{get= mem k; set= (fun b -> if b then add k else remove k)}

  module Proof = struct
    type nonrec elt = elt
    type mts = t
    type 'a pstep = 'a T.step
    type t = { elt : elt
             ; trie : Digest.t
             ; steps : (Digest.t pstep) list }
    let get elt t =
      Option.map (fun T.Proof.{key; trie; steps} -> {elt=key; trie; steps}) (T.Proof.get elt t)
    let check {elt; trie; steps} t l =
      T.Proof.check {key=elt; trie; leaf=T.SynthMerkle.leaf (); steps} t l ()
    include (Yojsonable(struct
               type nonrec t = t
               let to_yojson {elt; trie; steps} =
                 `Assoc
                   [ ("elt", Elt.to_yojson elt)
                   ; ("trie", Digest.to_yojson trie)
                   ; ("steps", `List (List.map (T.step_to_yojson Digest.to_yojson) steps)) ]
               let of_yojson_exn yojson =
                 { elt = yojson |> YoJson.member "elt" |> Elt.of_yojson_exn
                 ; trie = yojson |> YoJson.member "trie" |> Digest.of_yojson_exn
                 ; steps = yojson |> YoJson.member "steps" |> YoJson.to_list |>
                           List.map (T.step_of_yojson_exn Digest.of_yojson_exn) }
               let of_yojson = of_yojson_of_of_yojson_exn of_yojson_exn
               let yojsoning = {to_yojson;of_yojson}
             end) : YojsonableS with type t := t)
  end
end

module DigestSet = MerkleTrieSet (Digest)

module Test = struct
  let generic_compare = compare
  module SimpleTrieSynthCardinal = TrieSynthCardinal (UInt256) (StringT)
  module SimpleTrie = Trie (UInt256) (StringT) (DigestValueType) (SimpleTrieSynthCardinal)
      (TrieType (UInt256) (StringT) (DigestValueType) (SimpleTrieSynthCardinal))
  module MyTrie = MerkleTrie (UInt256) (StringT)
  open MyTrie
  let [@warning "-32"] nat_of_key : MyTrie.key -> UInt256.t = fun x -> x
  let [@warning "-32"] key_of_nat : UInt256.t -> MyTrie.key = fun x -> x

  let [@warning "-32"] rec print_trie out_channel t = match Wrap.get t with
    | Empty ->
      Printf.fprintf out_channel "Empty"
    | Leaf {value} ->
      Printf.fprintf out_channel "Leaf{value=%S}" value
    | Branch {left; right; height} ->
      Printf.fprintf out_channel "Branch{left=" ;
      print_trie out_channel left ; Printf.fprintf out_channel ";right=" ;
      print_trie out_channel right ; Printf.fprintf out_channel ";height=%d}" height
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

  let%test "simple_proof_consistent_100" =
    let (k, v) = (n 100), "100" in
    let t = singleton k v in
    Proof.check (Option.get (Proof.get k t)) t k v

  let%test "simple_proof_consistent_1" =
    let (k, v) = (n 0), "0" in
    let t = add (n 1) "1" (singleton k v) in
    Proof.check (Option.get (Proof.get k t)) t k v

  let%test "empty" =
    (bindings empty = []) && is_empty (of_bindings [])

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
         bork "Bad %s" name)
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
      (fun (b, trie) -> is_empty (List.fold_right remove (List.map fst b) trie))

  let%test "equal shuffle" =
    for_all_bindings "equal shuffle"
      (fun (b, trie) -> equal (=) trie (of_bindings (shuffle_list b)))

  let%test "unequal" =
    not (equal (=) (force trie_4) (force trie_1))

  let make_step direction other_direction digest =
    `Assoc [ ("type",`String direction)
           ; (other_direction,`String digest)
           ]
  let make_left_step = make_step "Left" "right"
  let make_right_step = make_step "Right" "left"

  let proof_42_in_trie_100 =
    lazy (Proof.of_yojson_exn
            (`Assoc [ ("key",`String "0x2a")
                    ; ("trie",`String "0x1b0247d6f6ca6e1411806a9f74fc3d833916fdc2401ed161e6b5ce601ad84185")
                    ; ("leaf", `String "0x70f3a8f4b17c16dfd0aed8dcb3f6b0d33e1052cf994f75e657d2d322a501bb50")
                    ; ("steps",
                       `List [ make_left_step "0x825549a7bd7501bb0ba616c4fc70cbbebefaea2d84f6b952fe398fda5d1462db"
                             ; make_right_step "0xa56429299f24894faf768ae69ed9615c40ed65f30ed15c2e148501d1310903ec"
                             ; make_left_step "0x346bfa238000fb06f04f4979b47486a4257d3259177782530fc26b353fbb81be"
                             ; make_right_step "0x3ff5f6bd2d1bfc6b2abe7696903c496c5b42d9da98e0c338cac8f5c8a6beca64"
                             ; make_left_step "0x0fd0e9496ba3fa5afb2293ca2531969c89ccb07b3cba20bdcdb6bdc14b92b590"
                             ; make_right_step "0xb8f1bc4ef8830fee1b2c823f9c2c89846692a15152f7f5a8265238d19f9b7b0d"
                             ; make_left_step "0xb914f8a6dda6b8a172da0361c6cb3e1c6d386629bdb76aae83d2d66bd374af6b"
                             ])
                    ]))

  let bad_proof = lazy (match force proof_42_in_trie_100 with
    | Proof.{ key ; trie ; leaf ; steps = [s1;s2;s3;s4;s5;s6;s7] } ->
      Proof.{ key ; trie ; leaf ; steps = [s1;s2;s5;s4;s3;s6;s7] } (* steps 3 and 5 are swapped *)
    | _ -> bork "Bad proof")

  let%test "proof" =
    Proof.get (n 42) (force trie_100) = Some (force proof_42_in_trie_100)
    || (Printf.printf "%s\n%!"
          (Proof.get (n 42) (force trie_100) |> Option.get |> Proof.to_yojson_string) ;
        false)

  let%test "simple_proof_consistent" =
    Proof.check
      (Option.get (Proof.get (n 0) (singleton (n 0) "0")))
      (singleton (n 0) "0")
      (n 0)
      "0"

  let%test "simple_proof_consistent_4" =
    Proof.check
      (Option.get (Proof.get (n 4) (singleton (n 4) "4")))
      (singleton (n 4) "4")
      (n 4)
      "4"

  let%test "simple_proof_consistent_10" =
    Proof.check
      (Option.get (Proof.get (n 57) (force trie_10_12_57)))
      (force trie_10_12_57)
      (n 57)
      "57"

  let%test "simple_proof_consistent_24" =
    Proof.check
      (Option.get (Proof.get (n 2) (force trie_4)))
      (force trie_4)
      (n 2)
      "2"

  let%test "proof_consistent" =
    Proof.check (force proof_42_in_trie_100) (force trie_100) (n 42) "42"

  let%test "proof_inconsistent" =
    not (Proof.check (force bad_proof) (force trie_100) (n 42) "42")

end

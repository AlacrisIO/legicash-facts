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
open Marshaling
open Db
open Trie

let _ = get_db "foo"

module type TrieSynthMerkleS = sig
  include TrieSynthS with type t = Digest.t
  val leaf_digest : t -> t
  module Marshalable : MarshalableS
  include DigestibleS with type t := t
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
  module Marshalable = struct
    type nonrec t = t

    let marshal buffer t = Digest.marshal buffer t
    let unmarshal ?(start=0) bytes = Digest.unmarshal ~start bytes
  end
  include (DigestibleOfMarshalable (Marshalable) : DigestibleS with type t := t)
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

  val empty_key : key
  val trie_digest : t -> Digest.t
  val path_digest : t path -> Digest.t path
  val get_proof : key -> t -> proof option
  val check_proof_consistency : proof -> bool
  val json_of_proof : proof -> Yojson.Basic.json

  module type MarshalNodeS = sig
    type trie = t

    (* like TrieS.t, except subtrees are of type synth *)
    type t =
      | NodeEmpty
      | NodeLeaf of { value: value; synth: synth }
      | NodeBranch of { left: synth; right: synth; height: int; synth: synth }
      | NodeSkip of { child: synth; bits: key; length: int; height: int; synth: synth }

    val marshal_empty : Buffer.t -> unit
    val marshal_leaf : Buffer.t -> value -> synth -> unit
    val marshal_branch : Buffer.t -> synth -> synth -> int -> synth -> unit
    val marshal_skip : Buffer.t -> synth -> key -> int -> int -> synth -> unit
    val marshal_trie : Buffer.t -> trie -> unit
    val unmarshal_to_node : ?start:int -> Bytes.t -> t * int
  end
  module MarshalNode : MarshalNodeS
end

module MerkleTrie (Key : IntS) (Value : DigestibleS) = struct
  module Synth = TrieSynthMerkle (Key) (Value)
  include Trie (Key) (Value) (Synth)

  let empty_key = Key.zero
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

  module type MarshalNodeS = sig
    type trie = t

    (* like TrieS.t, except subtrees are of type synth *)
    type t =
      | NodeEmpty
      | NodeLeaf of { value: value; synth: synth }
      | NodeBranch of { left: synth; right: synth; height: int; synth: synth }
      | NodeSkip of { child: synth; bits: key; length: int; height: int; synth: synth }

    val marshal_empty : Buffer.t -> unit
    val marshal_leaf : Buffer.t -> value -> synth -> unit
    val marshal_branch : Buffer.t -> synth -> synth -> int -> synth -> unit
    val marshal_skip : Buffer.t -> synth -> key -> int -> int -> synth -> unit
    val marshal_trie : Buffer.t -> trie -> unit
    val unmarshal_to_node : ?start:int -> Bytes.t -> t * int
  end

  module MarshalNode = struct
    type trie = t

    type t =
      | NodeEmpty
      | NodeLeaf of {value: value; synth: synth}
      | NodeBranch of {left: synth; right: synth; height: int; synth: synth}
      | NodeSkip of { child: synth; bits: key; length: int; height: int; synth: synth }

    module UInt64 = Crypto.UInt64 (* not Integer.UInt64, which lacks marshaling *)

    let empty_tag = 'E'
    let leaf_tag = 'L'
    let branch_tag = 'B'
    let skip_tag = 'S'

    let marshal_empty buffer =
      Marshaling.marshal_char buffer empty_tag

    let marshal_leaf buffer value synth =
      Marshaling.marshal_char buffer leaf_tag;
      Value.marshal buffer value;
      Synth.marshal buffer synth

    let marshal_branch buffer left right height synth =
      Marshaling.marshal_char buffer branch_tag;
      Synth.marshal buffer left;
      Synth.marshal buffer right;
      UInt64.marshal buffer (UInt64.of_int height);
      Synth.marshal buffer synth

    let marshal_skip buffer child bits length height synth =
      Marshaling.marshal_char buffer skip_tag;
      Synth.marshal buffer child;
      let bits_string = Key.to_big_endian_bits bits in
      (* we don't know statically the length of bits here, so record it *)
      UInt64.marshal buffer (UInt64.of_int (String.length bits_string));
      Buffer.add_string buffer bits_string;
      UInt64.marshal buffer (UInt64.of_int length);
      UInt64.marshal buffer (UInt64.of_int height);
      Synth.marshal buffer synth

    let marshal_trie buffer t =
      match t with
      | Empty -> marshal_empty buffer
      | Leaf {value; synth} -> marshal_leaf buffer value synth
      | Branch {left; right; height; synth} ->
        let synth_left = get_synth left in
        let synth_right = get_synth right in
        marshal_branch buffer synth_left synth_right height synth
      | Skip {child; bits; length; height; synth} ->
        let synth_child = get_synth child in
        marshal_skip buffer synth_child bits length height synth

    let unmarshal_leaf start bytes =
      let value,value_offset = Value.unmarshal ~start bytes in
      let synth,final_offset = Synth.unmarshal ~start:value_offset bytes in
      ( NodeLeaf { value; synth }
      , final_offset
      )

    let unmarshal_branch start bytes =
      let left,left_offset = Synth.unmarshal ~start bytes in
      let right,right_offset = Synth.unmarshal ~start:left_offset bytes in
      let height64, height64_offset = UInt64.unmarshal ~start:right_offset bytes in
      let height = UInt64.to_int height64 in
      let synth,final_offset = Synth.unmarshal ~start:height64_offset bytes in
      ( NodeBranch { left; right; height; synth }
      , final_offset
      )

    let unmarshal_skip start bytes =
      let child,child_offset = Synth.unmarshal ~start bytes in
      let bits_length64,bits_length64_offset = UInt64.unmarshal ~start:child_offset bytes in
      let bits_len = UInt64.to_int bits_length64 in
      let bits_string = Bytes.sub_string bytes bits_length64_offset bits_len in
      let bits = Key.of_big_endian_bits bits_string in
      let length64, length64_offset = UInt64.unmarshal ~start:(bits_length64_offset + bits_len) bytes in
      let length = UInt64.to_int length64 in
      let height64, height64_offset = UInt64.unmarshal ~start:length64_offset bytes in
      let height = UInt64.to_int height64 in
      let synth,final_offset = Synth.unmarshal ~start:height64_offset bytes in
      ( NodeSkip { child; bits; length; height; synth }
      , final_offset
      )

    let unmarshal_to_node ?(start=0) bytes =
      let node_tag,next = unmarshal_char ~start bytes in
      if node_tag == empty_tag then
        (NodeEmpty,next)
      else if node_tag == leaf_tag then
        unmarshal_leaf next bytes
      else if node_tag == branch_tag then
        unmarshal_branch next bytes
      else if node_tag == skip_tag then
        unmarshal_skip next bytes
      else
        raise (Internal_error "Unexpected tag when unmarshaling node")
  end
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

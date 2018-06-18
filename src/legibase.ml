(* legibase.ml -- base module for Legicash *)

open Lib
open Integer

exception Timeout of string

exception Double_spend of string

module Revision = UInt64
module Duration = UInt64
module Timestamp = UInt64

type conversation

(** A pure mapping from 'a to 'b suitable for use in interactive merkle proofs
    Let's cheat for now.
    TODO: Tezos must have something we should use.
    probably Tezos_crypto.S.MERKLE_TREE or Tezos_crypto.Blake2B.Make_merkle_tree
*)

(* maps with lenses and defaults *)

module type MapS = sig
  include Map.S

  val lens : key -> ('a t, 'a) Lens.t

  val find_defaulting : (unit -> 'a) -> key -> 'a t -> 'a
end

module MapMake (Key : Map.OrderedType) = struct
  include Map.Make (Key)

  let lens k = Lens.{get= find k; set= add k}

  let find_defaulting default k m = defaulting default (find_opt k m)
end

let defaulting_lens default lens =
  Lens.{get= (fun x -> try lens.get x with Not_found -> default ()); set= lens.set}

module RevisionMap = MapMake (Revision)


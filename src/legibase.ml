(* legibase.ml -- base module for Legicash *)

open Lib
open Integer

exception Timeout of string

exception Double_spend of string

type conversation

(** A pure mapping from 'a to 'b suitable for use in interactive merkle proofs
    Let's cheat for now.
    TODO: Tezos must have something we should use.
    probably Tezos_crypto.S.MERKLE_TREE or Tezos_crypto.Blake2B.Make_merkle_tree
*)

(*open Lib
  open Integer

  module type TrieSynthS = sig
  module Key : UnsignedS
  module Value : T
  type t

  val empty : t
  val leaf : Value.t -> t
  val branch : t -> t -> t
  end
  module type TrieSkipSynthS = sig
  include TrieSynthS
  val skip : int -> int -> Key.t -> t -> t
  end

  module TrieSynthComputeSkip (Synth: TrieSynthS) : TrieSkipSynthS

  module Trie (Key: UnsignedS) (Value : T)
  (Synth : TrieSkipSynthS with module Key = Key and module Value = Value) : sig
  type synth = Synth.t
  include MapS
  with type key = Key.t
  and type value = Value.t

  val trie_height : t -> int
  val check_invariant : t -> bool
  end
*)

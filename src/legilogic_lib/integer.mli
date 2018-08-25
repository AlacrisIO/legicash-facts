(* integer.mli -- various integer modules with common interface *)
open Lib
open Yojsoning
open Marshaling

(** TODO: before we go to production, robustify the shit out of all
    the type conversions, arithmetic operations, etc.,
    least it be used as an attack surface.
*)

(* Same as Unsigned.S from the integers library, minus the Infix module *)
module type UIntBaseS = sig
  type t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val max_int : t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val of_int : int -> t
  val to_int : t -> int
  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val of_string : string -> t
  val to_string : t -> string
  val zero : t
  val one : t
  val lognot : t -> t
  val succ : t -> t
  val pred : t -> t
  val compare : t -> t -> int
  val max : t -> t -> t
  val min : t -> t -> t
end

module type UIntMoreS = sig
  include UIntBaseS
  val module_name : string
  val size_in_bits : int (* size in bits, or -1 if dynamically allocated *)
  val size_in_bytes : int (* size in bits, or -1 if dynamically allocated *)
  val check_invariant : t -> bool
  val is_non_negative : t -> bool
  val z_of: t -> Z.t
  val of_z: Z.t -> t
  val equal : t -> t -> bool
  val sign : t -> int
  val extract : t -> int -> int -> t
  val numbits : t -> int
  val has_bit : t -> int -> bool
  val is_numbits : int -> t -> bool
  val of_bits : string -> t
  val of_hex_string : string -> t
  val to_hex_string : t -> string

  (** Convert from Ethereum-style 0x syntax *)
  val of_0x_string : string -> t

  (** Convert to Ethereum-style 0x syntax *)
  val to_0x_string : t -> string

  val of_big_endian_bits : string -> t
  val to_big_endian_bits : t -> string

  val is_add_valid : t -> t -> bool (* TODO: add an explicit upper bound as third argument? *)

  val is_sum : t -> t -> t -> bool

  val is_mul_valid : t -> t -> bool

  val is_product : t -> t -> t -> bool

  include PreYojsonableS with type t := t
  include PreMarshalableS with type t := t
  include ShowableS with type t := t
end

(* Same as the Infix sub-module of Unsigned.S from the integers library, plus the type t *)
module type UIntInfixS = sig
  type t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val (/) : t -> t -> t
  val (mod) : t -> t -> t
  val (land) : t -> t -> t
  val (lor) : t -> t -> t
  val (lxor) : t -> t -> t
  val (lsl) : t -> int -> t
  val (lsr) : t -> int -> t
end

module type UIntS = sig
  include UIntMoreS
  module Infix : UIntInfixS with type t := t
end

module Int : UIntS with type t = Z.t

module Nat : UIntS with type t = Z.t

module UInt256 : UIntS (* with type t = Nat.t *)
module Data256 : UIntS (* with type t = UInt256.t *)

module Data160 : UIntS (* with type t = Nat.t *)

module UInt128 : UIntS (* with type t = Nat.t *)
module UInt64 : UIntS (* with type t = Unsigned.UInt64.t *)
module UInt32 : UIntS (* with type t = Unsigned.UInt32.t *)
module UInt16 : UIntS (* with type t = Unsigned.UInt16.t *)


module type PreUIntZS = sig
  val size_in_bits : int
end

module UIntZ (P : PreUIntZS) : sig
  include UIntS with type t = Z.t
  val size_in_bits : int
  val size_in_bytes : int
end

module type PreUIntZableS = sig
  include Unsigned.S
  include PreUIntZS
  val z_of: t -> Z.t
  val of_z: Z.t -> t
end

module UIntZable (P: PreUIntZableS) : sig
  include UIntS with type t = P.t
  val size_in_bits : int
  val size_in_bytes : int
end

(** convert a n-bit natural number to a big-endian string of bytes *)
val big_endian_bits_of_nat : int -> Z.t -> string

(** convert a big-endian string of bytes to a n-bit natural number *)
val nat_of_big_endian_bits : int -> string -> Z.t

(** convert a natural number to a big-endian string of hex characters *)
val hex_string_of_nat : Z.t -> string

(** convert a big-endian string of hex characters to a natural number *)
val nat_of_hex_string : string -> Z.t

(** convert a n-bit natural number to a big-endian string of hex characters *)
val hex_string_of_sized_nat : int -> Z.t -> string

(** convert a big-endian string of hex characters to a n-bit natural number *)
val sized_nat_of_hex_string : int -> string -> Z.t

(** check that a unary op's output is valid, assuming the inputs were already validated
    but otherwise arbitrary elements of the type *)
val unary_post_op_check : ('a -> 'b) -> ('b -> bool) ->
  string * string * ('a -> string) ->
  'a -> 'b

val binary_post_op_check : ('a -> 'b -> 'c) -> ('c -> bool) ->
  string * string * ('a -> string) * ('b -> string) ->
  'a -> 'b -> 'c

val unary_pre_op_check : ('a -> 'b) -> ('a -> bool) ->
  string * string * ('a -> string) ->
  'a -> 'b

val binary_pre_op_check : ('a -> 'b -> 'c) -> ('a -> 'b -> bool) ->
  string * string * ('a -> string) * ('b -> string) ->
  'a -> 'b -> 'c

(* integer.mli -- various integer modules with common interface *)
open Lib
open Marshaling

(** TODO: before we go to production, robustify the shit out of all
    the type conversions, arithmetic operations, etc.,
    least it be used as an attack surface.
*)

module type IntS = sig
  include Unsigned.S
  include PreMarshalableS with type t := t
  include ShowableS with type t := t
  include JsonableS with type t := t
  val z_of: t -> Z.t
  val of_z: Z.t -> t
  val equal : t -> t -> bool
  val sign : t -> int
  val extract : t -> int -> int -> t
  val numbits : t -> int
  val has_bit : t -> int -> bool
  val of_bits : string -> t
  val of_hex_string : string -> t
  val to_hex_string : t -> string
  val of_big_endian_bits : string -> t
  val to_big_endian_bits : t -> string
end

module Int : IntS with type t = Z.t

module Nat : IntS with type t = Z.t

module UInt16 : IntS (* with type t = Unsigned.UInt16.t *)

module UInt32 : IntS (* with type t = Unsigned.UInt32.t *)

module UInt64 : IntS (* with type t = Unsigned.UInt64.t *)

module UInt256_z : IntS with type t = Z.t

module UInt256 : IntS (* with type t = Z.t *)

module Data256 : IntS (* with type t = Z.t *)


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

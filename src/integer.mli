(* integer.mli -- various integer modules with common interface *)

module type IntS = sig
  include Unsigned.S
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

module Nat : IntS with type t = Z.t

module UInt64 : IntS with type t = Unsigned.UInt64.t

module UInt256 : IntS with type t = Z.t

(** convert a n-bit natural number to a big-endian string of bytes *)
val big_endian_bits_of_nat : int -> Nat.t -> string

(** convert a big-endian string of bytes to a n-bit natural number *)
val nat_of_big_endian_bits : int -> string -> Nat.t

(** convert a natural number to a big-endian string of hex characters *)
val hex_string_of_nat : Nat.t -> string

(** convert a big-endian string of hex characters to a natural number *)
val nat_of_hex_string : string -> Nat.t

(** convert a n-bit natural number to a big-endian string of hex characters *)
val hex_string_of_sized_nat : int -> Nat.t -> string

(** convert a big-endian string of hex characters to a n-bit natural number *)
val sized_nat_of_hex_string : int -> string -> Nat.t

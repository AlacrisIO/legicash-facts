module type UnsignedS = sig
  include Unsigned.S
  val z_of: t -> Z.t
  val of_z: Z.t -> t
  val equal : t -> t -> bool
  val sign : t -> int
  val extract : t -> int -> int -> t
  val numbits : t -> int
  val has_bit : t -> int -> bool
  val of_bits : string -> t
end

module Nat : sig
  include UnsignedS
end

module UInt64 : UnsignedS
open Lib

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

module Nat = struct
  include Z
  let z_of = identity
  let of_z = identity
  let has_bit key position = equal one (extract key position 1)
  let max_int = Z.of_int (-1)
  module Infix = struct
    let (+) = Z.add
    let (-) = Z.sub
    let ( * ) = Z.mul
    let (/) = Z.div
    let (mod) = Z.rem
    let (land) = Z.logand
    let (lor) = Z.logor
    let (lxor) = Z.logxor
    let (lsl) = Z.shift_left
    let (lsr) = Z.shift_right
  end
end

module type __UnsignedS_Zable = sig
  include Unsigned.S
  val z_of: t -> Z.t
  val of_z: Z.t -> t
end

module OurUInt (UInt: __UnsignedS_Zable) = struct
  include UInt
  let equal x y = compare x y = 0
  let sign x = compare x zero
  let extract key position length = of_z (Z.extract (z_of key) position length)
  let numbits key = Z.numbits (z_of key)
  let has_bit key position = equal one (extract key position 1)
  let of_bits bits = Z.of_bits bits |> of_z
end

module UInt64_Zable = struct
  include Unsigned.UInt64
  let z_2_64 = Z.shift_left Z.one 64
  let z_maxint = Z.sub z_2_64 Z.one
  let z_maxposint = Z.sub (Z.shift_left Z.one 63) Z.one
  let z_of u = let z = Z.of_int64 (Unsigned.UInt64.to_int64 u) in
    if Z.sign z >= 0 then z else Z.logand z_maxint z
  let of_z z = Unsigned.UInt64.of_int64
                 (if Z.compare z z_maxposint <= 0 then
                    Z.to_int64 z
                  else
                    Z.to_int64 (Z.sub z z_2_64))
end

module UInt64 = struct
  include OurUInt (UInt64_Zable)
end

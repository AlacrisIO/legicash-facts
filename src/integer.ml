open Lib

let hex_string_of_nat bits nat =
  if (Z.sign nat < 0) || (Z.numbits nat > bits) then
    raise (Internal_error (Printf.sprintf "not a %d bit natural number: %s" bits (Z.to_string nat)))
  else
    let num_digits = (bits + 3) / 4 in
    String.init num_digits (fun i -> hex_char_of_int (Z.to_int (Z.extract nat ((num_digits - i - 1)*4) 4)))

let nat_of_big_endian_bits bits string =
  let num_bytes = (bits + 7) / 8 in
  if String.length string > num_bytes then
    raise (Internal_error (Printf.sprintf "not a %d bit string: %S" bits string))
  else
    Z.of_bits (string_reverse string)

let big_endian_bits_of_nat bits nat =
  if (Z.sign nat < 0) || (Z.numbits nat > bits) then
    raise (Internal_error (Printf.sprintf "not a %d bit natural number: %s" bits (Z.to_string nat)))
  else
    let num_bytes = (bits + 7) / 8 in
    String.init num_bytes (fun i -> Char.chr (Z.to_int (Z.extract nat ((num_bytes - i - 1)*8) 8)))

let nat_of_hex_string bits string =
  let num_digits = (bits + 3) / 4 in
  let len = String.length string in
  let fail _ = raise (Internal_error (Printf.sprintf "not a %d bit hex string: %S" bits string)) in
  if len > num_digits then
    fail ()
  else
    let n = Z.of_bits (string_reverse (parse_hex_string string)) in
    if Z.numbits n > bits then
      fail ()
    else
      n

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

module Nat = struct
  let of_hex_string hs = nat_of_hex_string (4 * String.length hs) hs
  let to_hex_string nat = hex_string_of_nat (Z.numbits nat) nat
  let of_big_endian_bits bs = nat_of_big_endian_bits (8 * String.length bs) bs
  let to_big_endian_bits nat = big_endian_bits_of_nat (Z.numbits nat) nat
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

module type __IntS_Zable = sig
  include Unsigned.S
  val z_of: t -> Z.t
  val of_z: Z.t -> t
end

module OurUInt (UInt: __IntS_Zable) = struct
  include UInt
  let equal x y = compare x y = 0
  let sign x = compare x zero
  let extract key position length = of_z (Z.extract (z_of key) position length)
  let numbits key = Z.numbits (z_of key)
  let has_bit key position = equal one (extract key position 1)
  let of_bits bits = Z.of_bits bits |> of_z
  let of_hex_string hs = of_z (Nat.of_hex_string hs)
  let to_hex_string u = Nat.to_hex_string (z_of u)
  let of_big_endian_bits bs = of_z (Nat.of_big_endian_bits bs)
  let to_big_endian_bits u = Nat.to_big_endian_bits (z_of u)
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

module Test = struct
  let%test "hex_string_of_nat 18 2018" =
    hex_string_of_nat 18 (Nat.of_z (Z.of_int 2018)) = "007e2"

  let%test "hex_string_of_nat 9 2018" =
    throws (Internal_error "not a 9 bit natural number: 2018")
      (fun _ -> hex_string_of_nat 9 (Nat.of_z (Z.of_int 2018)))

  let%test "nat_of_hex_string 18 007e2" =
    nat_of_hex_string 18 "007e2" = Z.of_int 2018

  let%test "nat_of_hex_string 9 007e2" =
    throws (Internal_error "not a 9 bit hex string: \"007e2\"")
      (fun _ -> nat_of_hex_string 9 "007e2")
end

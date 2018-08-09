open Lib
open Marshaling

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


let validate_nat_non_negative nat =
  if Z.sign nat < 0 then
    raise (Internal_error (Printf.sprintf "Expected natural number is negative: %s" (Z.to_string nat)))
let validate_sized_nat bits nat =
  validate_nat_non_negative nat ;
  if (Z.numbits nat > bits) then
    raise (Internal_error (Printf.sprintf "Natural natural number %s won't fit in %d bits" (Z.to_string nat) bits))

let hex_string_of_sized_nat bits nat =
  validate_sized_nat bits nat ;
  let num_digits = (bits + 3) / 4 in
  String.init num_digits (fun i -> hex_char_of_int (Z.to_int (Z.extract nat ((num_digits - i - 1)*4) 4)))

let hex_string_of_nat nat =
  if Z.sign nat = 0 then
    "0"
  else
    hex_string_of_sized_nat (Z.numbits nat) nat

let validate_sized_string num_bytes string =
  if String.length string > num_bytes then
    raise (Internal_error (Printf.sprintf "String longer than the expected %d bytes: %S" num_bytes string))

let nat_of_big_endian_bits bits string =
  let num_bytes = (bits + 7) / 8 in
  validate_sized_string num_bytes string ;
  Z.of_bits (string_reverse string)

let big_endian_bits_of_nat bits nat =
  validate_sized_nat bits nat ;
  let num_bytes = (bits + 7) / 8 in
  String.init num_bytes (fun i -> Char.chr (Z.to_int (Z.extract nat ((num_bytes - i - 1)*8) 8)))

let sized_nat_of_hex_string bits string =
  let num_digits = (bits + 3) / 4 in
  validate_sized_string num_digits string ;
  let nat = Z.of_bits (string_reverse (parse_hex_string string)) in
  validate_sized_nat bits nat ;
  nat

let nat_of_hex_string string =
  sized_nat_of_hex_string (4 * String.length string) string

module Int = struct
  include Z
  let z_of = identity
  let of_z = identity
  let has_bit key position = equal one (extract key position 1)
  let max_int = Z.of_int (-1)
  let of_hex_string = nat_of_hex_string
  let to_hex_string = hex_string_of_nat
  let of_big_endian_bits bs = nat_of_big_endian_bits (Pervasives.( * ) 8 (String.length bs)) bs
  let to_big_endian_bits nat = big_endian_bits_of_nat (Z.numbits nat) nat
  let marshaling = marshaling_not_implemented
  let pp formatter x = Format.fprintf formatter "%s" (to_string x)
  let show x = Format.asprintf "%a" pp x
  let to_json x = `String (to_string x)
  let of_json = function
    | `String s -> (of_string s)
    | _ -> (Yojson.json_error "bad Integer")
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

(* TODO: check sign after every relevant operation, etc. *)
module Nat = struct
  include Int
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
  (*  let of_big_endian_bits bs = of_z (Nat.of_big_endian_bits bs)
      let to_big_endian_bits u = Nat.to_big_endian_bits (z_of u) *)
  let pp formatter x = Format.fprintf formatter "%s" (to_string x)
  let show x = Format.asprintf "%a" pp x
  let to_json x = Int.to_json (z_of x)
  let of_json j = of_z (Int.of_json j)
end

module UInt16 = struct
  module Zable = struct
    include Unsigned.UInt16
    let of_z z = Unsigned.UInt16.of_int (Z.to_int z)
    let z_of u = Z.of_int (Unsigned.UInt16.to_int u)
  end
  include OurUInt (Zable)
  let of_big_endian_bits b = of_z (nat_of_big_endian_bits 16 b)
  let to_big_endian_bits u = big_endian_bits_of_nat 16 (z_of u)
  let marshaling = marshaling_sized_string 2 to_big_endian_bits of_big_endian_bits
  let to_json x = `Int (to_int x)
  let of_json = function
    | `Int i -> of_int i
    | _ -> (Yojson.json_error "bad integer")
end

module UInt32 = struct
  module Zable = struct
    include Unsigned.UInt32
    let of_z z = Unsigned.UInt32.of_int64 (Z.to_int64 z)
    let z_of u = Z.of_int64 (Unsigned.UInt32.to_int64 u)
  end
  include OurUInt (Zable)
  let of_big_endian_bits bs = of_z (nat_of_big_endian_bits 32 bs)
  let to_big_endian_bits u = big_endian_bits_of_nat 32 (z_of u)
  let marshaling = marshaling_sized_string 4 to_big_endian_bits of_big_endian_bits
end

module UInt64 = struct
  module Zable = struct
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
  include OurUInt (Zable)
  let of_big_endian_bits bs = of_z (nat_of_big_endian_bits 64 bs)
  let to_big_endian_bits u = big_endian_bits_of_nat 64 (z_of u)
  let marshaling = marshaling_sized_string 8 to_big_endian_bits of_big_endian_bits
end

(* TODO: check bounds, after every operation, etc. *)
module UInt256_z = struct
  include Int
  let of_big_endian_bits = nat_of_big_endian_bits 256
  let to_big_endian_bits = big_endian_bits_of_nat 256
  let marshaling = marshaling_sized_string 32 to_big_endian_bits of_big_endian_bits
end

module UInt256 = UInt256_z

module Data256 = struct
  include UInt256_z
  let of_hex_string = sized_nat_of_hex_string 256
  let to_hex_string = hex_string_of_sized_nat 256
end

module Test = struct
  let%test "hex_string_of_nat 18 2018" =
    hex_string_of_nat (Nat.of_z (Z.of_int 2018)) = "7e2"

  let%test "hex_string_of_nat 18 2018" =
    hex_string_of_sized_nat 18 (Nat.of_z (Z.of_int 2018)) = "007e2"

  let%test "hex_string_of_nat 9 2018" =
    throws (Internal_error "Natural natural number 2018 won't fit in 9 bits")
      (fun _ -> hex_string_of_sized_nat 9 (Nat.of_z (Z.of_int 2018)))

  let%test "nat_of_hex_string 7e2" =
    nat_of_hex_string "7e2" = Z.of_int 2018

  let%test "nat_of_hex_string 007e2" =
    nat_of_hex_string "007e2" = Z.of_int 2018

  let%test "sized_nat_of_hex_string 18 007e2" =
    sized_nat_of_hex_string 18 "007e2" = Z.of_int 2018

  let%test "sized_nat_of_hex_string 9 007e2" =
    throws (Internal_error "String longer than the expected 3 bytes: \"007e2\"")
      (fun _ -> sized_nat_of_hex_string 9 "007e2")

  let%test "foo" =
    UInt256.to_string (UInt256.of_z (Z.of_int 42)) = "42"
    && UInt64.to_string (UInt64.of_z (UInt256.z_of (UInt256.of_int 42))) = "42"
end

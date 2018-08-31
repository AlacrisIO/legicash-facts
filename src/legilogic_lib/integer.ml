open Lib
open Hex
open Yojsoning
open Marshaling

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
  val size_in_bits : int
  val size_in_bytes : int
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
  val to_bits : t -> string
  val of_hex_string : string -> t
  val to_hex_string : t -> string
  val of_0x_string : string -> t
  val to_0x_string : t -> string
  val of_big_endian_bits : string -> t
  val to_big_endian_bits : t -> string
  val is_add_valid : t -> t -> bool
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

let is_nat z = Z.sign z >= 0
let is_sized_nat bits z = Z.sign z >= 0 && Z.numbits z <= bits

let validate_nat z =
  if not (is_nat z) then
    bork "Expected natural number is negative: %s" (Z.to_string z)
let validate_sized_nat bits z =
  validate_nat z ;
  if (Z.numbits z > bits) then
    bork "Natural natural number %s won't fit in %d bits" (Z.to_string z) bits

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
  if String.length string != num_bytes then
    bork "String length not the expected %d bytes: %S" num_bytes string

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

let unary_pre_op_check op check info x =
  if check x then op x else
    let (module_name, op_name, to_string) = info in
    bork "attempted %s.%s %s but the arguments are out of range"
      module_name op_name (to_string x)

let binary_pre_op_check op check info x y =
  if check x y then op x y else
    let (module_name, op_name, x_to_string, y_to_string) = info in
    bork "attempted %s.%s %s %s but the arguments are out of range"
      module_name op_name (x_to_string x) (y_to_string y)

let unary_post_op_check op check info x =
  let y = op x in
  if check y then y else
    let (type_name, op_name, to_string) = info in
    bork "attempted %s.%s %s but the operation goes out of range"
      type_name op_name (to_string x)

let binary_post_op_check = fun
  op check info x y ->
  let z = op x y in
  if check z then z else
    let (type_name, op_name, x_to_string, y_to_string) = info in
    bork "attempted %s.%s %s %s but the operation goes out of range"
      type_name op_name (x_to_string x) (y_to_string y)

module Int = struct
  include Z
  let z_of = identity
  let of_z = identity
  let module_name = "Int"
  let size_in_bits = -1
  let size_in_bytes = -1
  let check_invariant _  = true
  let has_bit key position = equal one (extract key position 1)
  let is_numbits n z = numbits z <= n
  let is_non_negative z = sign z >= 0
  let is_add_valid _ _ = true
  let is_mul_valid _ _ = true
  let is_sum x y z = equal x (add y z)
  let is_product x y z = equal x (mul y z)
  let max_int = Z.of_int (-1)
  let of_hex_string = nat_of_hex_string
  let to_hex_string = hex_string_of_nat
  let of_0x_string = parse_0x_quantity
  let to_0x_string = unparse_0x_quantity
  let of_big_endian_bits bs = nat_of_big_endian_bits (Pervasives.( * ) 8 (String.length bs)) bs
  let to_big_endian_bits nat = big_endian_bits_of_nat (Z.numbits nat) nat
  (* TODO: Use ethereum-style json everywhere *)
  let yojsoning = yojsoning_map to_string of_string string_yojsoning
  let marshaling = marshaling_not_implemented
  let pp formatter x = Format.fprintf formatter "%s" (to_string x)
  let show x = Format.asprintf "%a" pp x
  module Infix = struct
    type t = Z.t
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
  include (Int : UIntMoreS with type t = Z.t)
  let module_name = "Nat"
  let check_underflow = is_non_negative
  let check_invariant = check_underflow
  let sub =
    binary_post_op_check sub check_underflow (module_name, "sub", to_string, to_string)
  let pred =
    unary_post_op_check pred check_underflow (module_name, "pred", to_string)
  let of_z =
    unary_post_op_check of_z check_underflow (module_name, "of_z", Z.to_string)
  let of_string =
    unary_post_op_check of_string check_underflow (module_name, "of_string", identity)
  let of_int =
    unary_post_op_check of_int check_underflow (module_name, "of_int", string_of_int)
  let of_int64 =
    unary_post_op_check of_int64 check_underflow
      (module_name, "of_int64", Signed.Int64.to_string)
  let marshaling = marshaling_not_implemented
  module Infix = struct
    include Int.Infix
    let (-) = sub
  end
end

module type PreUIntZS = sig
  val size_in_bits : int
end

module UIntZ (P : PreUIntZS) = struct
  include (Int : UIntMoreS with type t = Z.t)
  include P
  let module_name = "UInt" ^ (string_of_int size_in_bits)
  let size_in_bytes = (size_in_bits + 7) / 8
  let check_overflow = is_numbits size_in_bits
  let check_underflow = is_non_negative
  let check_invariant z = check_underflow z && check_overflow z
  (* NB: included is_sum and is_product use pre-overflow-check add and mul -- good *)
  let add =
    binary_post_op_check add check_overflow
      (module_name, "add", to_string, to_string)
  let sub =
    binary_post_op_check sub check_underflow
      (module_name, "sub", to_string, to_string)
  let mul =
    binary_post_op_check mul check_overflow
      (module_name, "mul", to_string, to_string)
  let shift_left =
    binary_post_op_check shift_left check_overflow
      (module_name, "shift_left", to_string, string_of_int)
  let succ =
    unary_post_op_check succ check_overflow (module_name, "succ", to_string)
  let pred =
    unary_post_op_check pred check_underflow (module_name, "succ", to_string)
  let of_big_endian_bits b = of_z (nat_of_big_endian_bits size_in_bits b)
  let to_big_endian_bits u = big_endian_bits_of_nat size_in_bits (z_of u)
  let of_z =
    unary_post_op_check of_z check_invariant (module_name, "of_z", Z.to_string)
  let of_string =
    unary_post_op_check of_string check_invariant
      (module_name, "of_string", identity)
  let of_hex_string =
    unary_post_op_check of_hex_string check_overflow (module_name, "of_hex_string", identity)
  let of_0x_string =
    unary_post_op_check of_0x_string check_overflow (module_name, "of_hex_string", identity)
  let of_int =
    unary_post_op_check of_int check_invariant
      (module_name, "of_int", string_of_int)
  let of_int64 =
    unary_post_op_check of_int64 check_invariant
      (module_name, "of_int64", Signed.Int64.to_string)

  let yojsoning = yojsoning_map to_0x_string of_0x_string string_yojsoning

  (* Beware: unmarshaling here assumes that assumes size_in_bits is exactly size_in_bytes*8;
     if that's not the case, you must override this value. *)
  let marshaling = marshaling_sized_string size_in_bytes to_big_endian_bits of_big_endian_bits

  module Infix = struct
    include Int.Infix
    let (+) = add
    let (-) = sub
    let ( * ) = mul
    let (lsl) = shift_left
  end
end

(* TODO: check bounds, after every operation, etc. *)
module UInt256_z = struct
  include UIntZ (struct let size_in_bits = 256 end)
  let yojsoning = yojsoning_map to_0x_string of_0x_string string_yojsoning
end

module UInt256 = UInt256_z

module Data256 = struct
  include UInt256_z
  let of_hex_string = sized_nat_of_hex_string 256
  let to_hex_string = hex_string_of_sized_nat 256
  let of_0x_string = parse_0x_prefix of_hex_string
  let to_0x_string = unparse_0x_prefix to_hex_string
  let yojsoning = yojsoning_map to_0x_string of_0x_string string_yojsoning
end

module Data160 = struct
  include UIntZ(struct let size_in_bits = 160 end)
  let of_hex_string = sized_nat_of_hex_string 160
  let to_hex_string = hex_string_of_sized_nat 160
  let of_0x_string = parse_0x_prefix of_hex_string
  let to_0x_string = unparse_0x_prefix to_hex_string
  let yojsoning = yojsoning_map to_0x_string of_0x_string string_yojsoning
end

module UInt128 = struct
  include UIntZ (struct let size_in_bits = 128 end)
  let yojsoning = yojsoning_map to_0x_string of_0x_string string_yojsoning
end

module type PreUIntZableS = sig
  include Unsigned.S
  include PreUIntZS
  val z_of: t -> Z.t
  val of_z: Z.t -> t
end

module UIntZable (P: PreUIntZableS) = struct
  include (P : UIntBaseS with type t = P.t)
  let size_in_bits = P.size_in_bits
  let module_name = "UInt" ^ (string_of_int size_in_bits)
  let size_in_bytes = (size_in_bits + 7) / 8
  let of_z = unary_pre_op_check P.of_z (is_sized_nat size_in_bits) (module_name, "of_z", Z.to_string)
  let z_of = P.z_of
  let check_invariant _ = true
  let equal x y = compare x y = 0
  let sign x = compare x zero
  let is_non_negative _ = true
  let extract key position length = of_z (Z.extract (z_of key) position length)
  let numbits key = Z.numbits (z_of key)
  let is_numbits n z = numbits z < n
  let has_bit key position = equal one (extract key position 1)
  let of_bits bits = Z.of_bits bits |> of_z
  let to_bits n = n |> z_of |> Z.to_bits
  let of_hex_string = Nat.of_hex_string >> of_z
  let to_hex_string = z_of >> Nat.to_hex_string
  let of_big_endian_bits = nat_of_big_endian_bits size_in_bits >> of_z
  let to_big_endian_bits = z_of >> big_endian_bits_of_nat size_in_bits
  let of_0x_string = Nat.of_0x_string >> of_z
  let to_0x_string = z_of >> Nat.to_0x_string
  let pp formatter x = Format.fprintf formatter "%s" (to_string x)
  let show x = Format.asprintf "%a" pp x
  let yojsoning = yojsoning_map to_0x_string of_0x_string string_yojsoning
  let marshaling = marshaling_sized_string size_in_bytes to_big_endian_bits of_big_endian_bits
  let is_add_carry x y = compare x (lognot y) > 0
  let is_add_valid x y = not (is_add_carry x y)
  let is_mul_valid x y = is_sized_nat size_in_bits (Z.mul (z_of x) (z_of y))
  let is_sum x y z = is_add_valid y z && equal x (add y z) (* NB: pre-check add *)
  let is_product x y z = is_mul_valid y z && equal x (mul y z) (* NB: pre-check mul *)
  let add = binary_pre_op_check add is_add_valid (module_name, "add", to_string, to_string)
  let sub = binary_pre_op_check sub (fun x y -> compare x y >= 0)
              (module_name, "sub", to_string, to_string)
  let mul = binary_pre_op_check mul is_mul_valid (module_name, "sub", to_string, to_string)
  let shift_left =
    binary_pre_op_check shift_left
      (fun x s -> s = 0 || (s > 0 && s <= size_in_bits && shift_right x (size_in_bits - s) = zero))
      (module_name, "shift_left", to_string, string_of_int)
  let succ =
    unary_pre_op_check succ (fun x -> lognot x != zero) (module_name, "succ", to_string)
  let pred =
    unary_pre_op_check pred (fun x -> x != zero) (module_name, "succ", to_string)
  let of_big_endian_bits b = of_z (nat_of_big_endian_bits size_in_bits b)
  let to_big_endian_bits u = big_endian_bits_of_nat size_in_bits (z_of u)
  let of_string = Z.of_string >> of_z
  let of_int = Z.of_int >> of_z
  let of_int64 = Z.of_int64 >> of_z

  module Infix = struct
    type nonrec t = t
    include P.Infix
    let (+) = add
    let (-) = sub
    let ( * ) = mul
    let (lsl) = shift_left
  end
end

module UInt16 = struct
  module P = struct
    include Unsigned.UInt16
    let size_in_bits = 16
    let of_z z = Unsigned.UInt16.of_int (Z.to_int z)
    let z_of u = Z.of_int (Unsigned.UInt16.to_int u)
  end
  include UIntZable (P)
end

module UInt32 = struct
  module P = struct
    include Unsigned.UInt32
    let size_in_bits = 32
    let of_z z = Unsigned.UInt32.of_int64 (Z.to_int64 z)
    let z_of u = Z.of_int64 (Unsigned.UInt32.to_int64 u)
  end
  include UIntZable (P)
end

module UInt64 = struct
  module P = struct
    include Unsigned.UInt64
    let size_in_bits = 64
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
  include UIntZable (P)
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
    throws (Internal_error "String length not the expected 3 bytes: \"007e2\"")
      (fun _ -> sized_nat_of_hex_string 9 "007e2")

  let internal_error_thrown thunk = try ignore (thunk ()) ; None with Internal_error x -> Some x

  module SimpleUIntTests (U : UIntS) = struct
    let t name init func expected =
      let result = func init in
      if result = expected then true else
        bork "test %s, expected %s but got %s" name expected result
    let run () =
      t "of_int >> check_invariant" 42 (U.of_int >> U.check_invariant >> string_of_bool) "true"
      && t "of_int >> to_string " 42 (U.of_int >> U.to_string) "42"
      && t "of_string >> to_int " "42" (U.of_string >> U.to_int >> string_of_int) "42"
      && t "of_int >> to_yojson >> of_yojson >> to_int"
           42 (U.of_int >> U.yojsoning.to_yojson >> U.yojsoning.of_yojson
               >> ResultOrString.get >> U.to_string) "42"
  end
  module MoreUIntTests (U : UIntS) = struct
    include SimpleUIntTests (U)
    let run () =
      run ()
      && t "internal_error_thrown of_int -42"
           (fun () -> U.of_int ~-42) (internal_error_thrown >> Option.get)
           (Printf.sprintf "attempted %s.of_int -42 but the operation goes out of range" U.module_name)
  end
  module TNat = SimpleUIntTests (Nat)
  module TU16 = SimpleUIntTests (UInt16)
  module TU32 = SimpleUIntTests (UInt32)
  module TU64 = SimpleUIntTests (UInt64)
  module TU256 = MoreUIntTests (UInt256)
  module TD160 = MoreUIntTests (Data160)
  module TD256 = MoreUIntTests (Data256)
  let%test "simple Nat tests" = TNat.run ()
  let%test "simple UInt16 tests" = TU16.run ()
  let%test "simple UInt32 tests" = TU32.run ()
  let%test "simple UInt64 tests" = TU64.run ()
  let%test "simple UInt256 tests" = TU256.run ()
  let%test "simple Data160 tests" = TD160.run ()
  let%test "simple Data256 tests" = TD256.run ()

  let%test "0x_string <-> UInt256" =
    List.for_all
      (fun (n, hex) -> let num = UInt256.of_int n in
        UInt256.to_0x_string num = hex
        && UInt256.equal num (UInt256.of_0x_string hex))
      [(0,"0x0");(1,"0x1");(10,"0xa");(291,"0x123");(61453,"0xf00d");(0xabcde,"0xabcde")]

  let%test "UInt256 <> 0x_string errors" =
    List.for_all
      (fun (hex, err) -> try ignore (UInt256.of_0x_string hex) ; false with
           Internal_error x -> x = err)
      [("0x", "Hex quantity has no digits");
       ("0x0400","Hex quantity has leading zero");
       ("ff","Hex string does not begin with 0x")]
end

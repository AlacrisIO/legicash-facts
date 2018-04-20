type t

val zero : t

val one : t

val minus_one : t
(** wrapped around, so max_int !!! *)

val neg : t -> t
(** wraps around, so two's complement *)

val add : t -> t -> t
(** wraps around *)

val sub : t -> t -> t
(** wraps around *)

val mul : t -> t -> t
(** wraps around, so mod 2**64 *)

val div : t -> t -> t
(** Integer division. Raise Division_by_zero if the second argument is zero. *)

val rem : t -> t -> t
(** modulo *)

val succ : t -> t
(** wraps around *)

val pred : t -> t
(** wraps around *)

val abs : t -> t

val max_int : t

val min_int : t

val logand : t -> t -> t

val logor : t -> t -> t

val logxor : t -> t -> t

val lognot : t -> t

val shift_left : t -> int -> t
(** The result is unspecified if y < 0 or y >= 64. *)

val shift_right : t -> int -> t
(** This is a logical shift. The result is unspecified if y < 0 or y >= 64. *)

val shift_right_logical : t -> int -> t
(** This is a logical shift. The result is unspecified if y < 0 or y >= 64. *)

val shift_right_arithmetic : t -> int -> t
(** This is an "arithmetic" shift, i.e. as if signed Int64. The result is unspecified if y < 0 or y >= 64. *)

val of_int : int -> t
(** wrap-around signed-extended conversion *)

val to_int : t -> int
(** modulo 31 bits *)

val of_float : float -> t
(** Convert the given floating-point number to a 64-bit integer, discarding the fractional part (truncate towards 0). The result of the conversion is undefined if, after truncation, the number is outside the range [0, T.max_int]. *)

val to_float : t -> float

val of_int32 : int32 -> t
(** wrap-around signed-extended conversion *)

val to_int32 : t -> int32

val of_nativeint : nativeint -> t

val to_nativeint : t -> nativeint

val of_string : string -> t

val of_string_opt : string -> t option

val to_string : t -> string

val bits_of_float : float -> t

val float_of_bits : t -> float

val compare : t -> t -> int

val equal : t -> t -> bool

val of_int64 : int64 -> t

val to_int64 : t -> int64

val is_add_carry : t -> t -> bool

val is_odd : t -> bool

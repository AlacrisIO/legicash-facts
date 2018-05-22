(* ethereum_util.mli *)

val string_of_hex_string : string -> string
(** convert Ethereum format, 0x followed by hex-digit pairs, to string *)

val hex_string_of_string : string -> string
(** convert string to Ethereum format, 0x followed by hex-digit pairs *)

(* ethereum_util.mli *)

open Legibase

val string_of_hex_string : string -> string
(** convert Ethereum format, 0x followed by hex-digit pairs, to string *)

val hex_string_of_string : string -> string
(** convert string to Ethereum format, 0x followed by hex-digit pairs *)

val hex_string_of_bytes : Bytes.t -> string
(** convert bytes to Ethereum format hex string *)

val hex_string_of_address : Address.t -> string
(** convert address to hex string *)

val address_of_hex_string : string -> Address.t
(** convert hex string to address *)

val token_amount_of_hex_string : string -> Main_chain.TokenAmount.t
(** convert Ethereum format hex strings to TokenAmount *)

val hex_string_of_int64 : int64 -> string
(** convert number to Ethereum hex format *)

val hex_string_of_token_amount : Main_chain.TokenAmount.t -> string
(** convert token amount to Ethereum hex format *)

val hex_string_of_nonce : Main_chain.Nonce.t -> string
(** convert nonce to Ethereum hex format *)

val hash : string -> string

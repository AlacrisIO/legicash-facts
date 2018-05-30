(* ethereum_util.mli *)

open Legibase

val string_of_hex_string : string -> string
(** convert Ethereum format, 0x followed by hex-digit pairs, to string *)

val hex_string_of_string : ?left_pad:bool -> string -> string
(** convert string to Ethereum format, 0x followed by hex-digit pairs; optional argument indicates whether to allow leading 0 *)

val hex_string_of_bytes : Bytes.t -> string
(** convert bytes to string, where each character represents a hex digit *)

val bytes_of_hex_string : string -> Bytes.t
(** convert Ethereum hex string to bytes; useful for converting solc --bin output to bytes *)

val string_of_int64 : int64 -> string
(** convert int64 to string, where each character represents a hex digit *)

val address_of_hex_string : string -> Address.t
(** convert hex string to address *)

val hex_string_of_bytes : Bytes.t -> string
(** convert bytes to Ethereum format hex string *)

val hex_string_of_address : Address.t -> string
(** convert address to hex string *)

val address_of_hex_string : string -> Address.t
(** convert hex string to address *)

val token_amount_of_hex_string : string -> Main_chain.TokenAmount.t
(** convert Ethereum hex string to TokenAmount *)

val hex_string_of_address : Address.t -> string
(** convert address to Ethereum hex string *)

val hex_string_of_token_amount : Main_chain.TokenAmount.t -> string
(** convert token amount to Ethereum hex string *)

val string_of_token_amount : Main_chain.TokenAmount.t -> string
(** convert token amount to string, where each character represents a hex digit *)

val hex_string_of_nonce : Main_chain.Nonce.t -> string
(** convert nonce to Ethereum hex string *)

val string_of_nonce : Main_chain.Nonce.t -> string
(** convert nonce to string, where each character represents a hex digit *)

val hash : string -> string

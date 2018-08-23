(* ethereum_util.mli *)

open Crypto

(** Keccak256 hash *)
val hash : string -> string
val hash_bytes : bytes -> string

(* Hexadecimal support. See https://github.com/ethereum/wiki/wiki/JSON-RPC#hex-value-encoding *)

(** Convert string to Ethereum format hexadecimal, 0x followed by hex-digit pairs
    (or single 0 for the empty string) *)
val hex_string_of_string : string -> string

(** Validate and convert Ethereum format hexadecimal back into a number. *)
val string_of_hex_string : string -> string

(** Convert bytes to Ethereum format hexadecimal, 0x followed by hex-digit pairs
    (or single 0 for the empty sequence of bytes) *)
val hex_string_of_bytes : Bytes.t -> string

(** Validate and convert Ethereum format hexadecimal back into a sequence of bytes.
    Useful for converting solc --bin output to bytes *)
val bytes_of_hex_string : string -> Bytes.t

(** Convert address to bytes *)
val bytes_of_address : Address.t -> Bytes.t

(** Convert address to Ethereum format hexadecimal, with checksum per EIP-55
    https://github.com/ethereum/EIPs/blob/master/EIPS/eip-55.md *)
val hex_string_of_address_with_checksum : Address.t -> string

(** Convert hex string to address, validating the checksum per EIP-55 *)
val address_of_hex_string_with_checksum : string -> Address.t

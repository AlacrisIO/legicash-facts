(* ethereum_util.mli *)

open Legibase
open Crypto

(** Keccak256 hash *)
val hash : string -> string

(* Hexadecimal support. See https://github.com/ethereum/wiki/wiki/JSON-RPC#hex-value-encoding *)

(** Convert numeric quantity to Ethereum format hexadecimal: 0x followed by hex-digits,
    the first of which isn't 0 unless it's the only hex-digit. *)
val hex_string_of_number : Nat.t -> string

(** Validate and convert Ethereum format hexadecimal back into a number. *)
val number_of_hex_string : string -> Nat.t

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

(** Convert address to Ethereum format hexadecimal, not including checksum *)
val hex_string_of_address : Address.t -> string

(** Convert hex string to address, not validating the checksum *)
val address_of_hex_string : string -> Address.t

(** Convert address to Ethereum format hexadecimal, with checksum per EIP-55
    https://github.com/ethereum/EIPs/blob/master/EIPS/eip-55.md *)
val hex_string_of_address_with_checksum : Address.t -> string

(** Convert hex string to address, validating the checksum per EIP-55 *)
val address_of_hex_string_with_checksum : string -> Address.t

(** Convert Ethereum hex string to TokenAmount *)
val token_amount_of_hex_string : string -> Main_chain.TokenAmount.t

(** Convert token amount to Ethereum hex string *)
val hex_string_of_token_amount : Main_chain.TokenAmount.t -> string

(** Convert token amount to string, where each character represents a hex digit *)
val bits_of_token_amount : Main_chain.TokenAmount.t -> string

(** Convert nonce to Ethereum hex string *)
val hex_string_of_nonce : Main_chain.Nonce.t -> string

(** Convert nonce to string, where each character represents a hex digit *)
val bits_of_nonce : Main_chain.Nonce.t -> string

(* ethereum_util.mli *)
open Legilogic_lib
open Signing

(* Hexadecimal support. See https://github.com/ethereum/wiki/wiki/JSON-RPC#hex-value-encoding *)

(** Convert address to bytes *)
val bytes_of_address : Address.t -> Bytes.t

(** Convert address to Ethereum format hexadecimal, with checksum per EIP-55
    https://github.com/ethereum/EIPs/blob/master/EIPS/eip-55.md *)
val hex_string_of_address_with_checksum : Address.t -> string

(** Convert hex string to address, validating the checksum per EIP-55 *)
val address_of_hex_string_with_checksum : string -> Address.t

module Test : sig
  val expect_string : string -> string -> string -> unit
  val expect_0x_string : string -> string -> string -> unit
  val expect_0x_bytes : string -> string -> bytes -> unit
end

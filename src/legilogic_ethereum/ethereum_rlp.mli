(* ethereum_rlp.mli -- Ethereum's notion of RLP ("Recursive Length Prefix") encoding *)

(** reference:

    https://github.com/ethereum/wiki/wiki/[English]-RLP
*)

type rlp_item = RlpItem of string | RlpItems of rlp_item list

type t

val encode : rlp_item -> t
(** RLP-encode item(s)  *)

val encode_string : string -> t
(** produce RLP-encoding from string *)

val encode_bytes : Bytes.t -> t
(** produce RLP-encoding from bytes *)

val to_string : t -> string
(** for debugging, expose string in RLP-encoding *)

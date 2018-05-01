(* ethereum_rlp.mli -- Ethereum's notion of RLP ("Recursive Length Prefix") encoding *)

(** reference:

    https://github.com/ethereum/wiki/wiki/[English]-RLP
*)

type t

(** produce RLP-encoding from string *)
val encode_string : string -> t

(** produce RLP-encoding from bytes *)
val encode_bytes : Bytes.t -> t

(** for debugging, expose string in RLP-encoding *)
val to_string : t -> string

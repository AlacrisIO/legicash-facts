(* ethereum_rlp.mli -- Ethereum's notion of RLP ("Recursive Length Prefix") encoding *)

(** reference:

    https://github.com/ethereum/wiki/wiki/[English]-RLP
*)

type t

(** produce RLP-encoding from string *)
val rlp_encode_string : string -> t

(** produce RLP-encoding from bytes *)
val rlp_encode_bytes : Bytes.t -> t

(** for debugging, expose string in RLP-encoding *)
val string_of_encoding : t -> string

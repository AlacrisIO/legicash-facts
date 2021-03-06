(** Ethereum's notion of RLP ("Recursive Length Prefix") encoding

    Reference:

    https://github.com/ethereum/wiki/wiki/[English]-RLP
*)

type rlp_item = Ppx_deriving_rlp_runtime.Rlp.rlp_item = RlpItem of string | RlpItems of rlp_item list

type t

val encode : rlp_item -> t
(** RLP-encode item(s)  *)

val encoded_string : rlp_item -> string
(** RLP-encode items then extract string *)

val encode_string : string -> t
(** produce RLP-encoding from string *)

val encode_bytes : Bytes.t -> t
(** produce RLP-encoding from bytes *)

val to_string : t -> string
(** for debugging, expose string in RLP-encoding *)

(* base.mli -- base types for Legicash platform *)

exception Timeout of string
exception Double_spend of string

type 'a legi_result = ('a, exn) result

(** unique identifier for all parties, that is, customers and facilitators *)
type public_key
(* = Crypto_box.public_key *)

type int256 =
  {field1: Int64.t; field2: Int64.t; field3: Int64.t; field4: Int64.t}

type signature_t = int256

type digest_t = int256

(** represents account balance *)
type token_amount

type revision_t = Int64.t

(** Hash of the header for the height. *)
type duration_t = Int32.t

type 'a signed =
  | Signed of {payload: 'a; signer: public_key; signature: signature_t}

(** Number of consensus, if linear block-based. Timestamp if DAG-based? *)
type main_chain_height_t = Int32.t

type main_chain_state = digest_t

type side_chain_state = digest_t

(** endpoint + state of communication + possibility of reconnection *)
type conversation

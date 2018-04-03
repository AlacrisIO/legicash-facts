(* base.mli -- base types for Legicash platform *)

exception Timeout of string
exception Double_spend of string

type 'a legi_result

(** unique identifier for all parties, that is, customers and facilitators *)
type public_key

type int256

type signature_t

type digest_t

(** represents account balance *)
type token_amount

type revision_t

(** Hash of the header for the height. *)
type duration_t

type 'a signed

(** Number of consensus, if linear block-based. Timestamp if DAG-based? *)
type main_chain_height_t

type main_chain_state

type side_chain_state

(** endpoint + state of communication + possibility of reconnection *)
type conversation

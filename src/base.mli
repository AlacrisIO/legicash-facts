(* base.mli -- base types for Legicash platform *)

exception Timeout of string

exception Double_spend of string

(** 'a or exception *)
type 'a legi_result

(** unique identifier for all parties, that is, customers and facilitators *)
type public_key

(** morally a 256-bit integer *)
type int256

(** a signature for an object of type 'a *)
type 'a signature

val is_signature_valid: public_key -> 'a -> 'a signature -> bool

(** an object of type 'a with its signature by one party *)
type 'a signed

(** a cryptographic digest, "hash", for an object of type 'a *)
type 'a digest

(** represents account balance in a the particular currency (tez, eth, etc.) *)
type token_amount

(** count of changes in an object.
    A positive integer less than 2**63, incremented at every change to a notional object's state.
    In Quake, we used to call that the mod-count (modification count).
    Can serve as a local counter in a Lamport clock.
    Should a user record have two of them, one for the user one for the server?
    Not for now: the server can use the global clock for the server.
 *)

type revision

(** type of a timestamp *)
type timestamp

(** type for a duration in terms of consensus cycles, for use in timeouts *)
type duration

(** Number of consensus rounds if linear block-based. Timestamp if DAG-based? *)
type main_chain_height

(** State of a main chain block. In tezos, it's a Block_header.t *)
type main_chain_state

(** Transaction to be posted on the main chain *)
type main_chain_transaction

(** Confirmation of a transaction on the main chain *)
type main_chain_transaction_confirmation

(** A conversation between two parties
    The type embodies an endpoint + state of communication + possibility of reconnection
    Maybe make the state of communication static, with a session type?
 *)
type conversation

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

(** count of changes in an object.
    A positive integer less than 2**63, incremented at every change to a notional object's state.
    In Quake, we used to call that the mod-count (modification count).
    Can serve as a local counter in a Lamport clock.
    Should a user record have two of them, one for the user one for the server?
    Not for now: the server can use the global clock for the server.
 *)

(** sequence number for changes in a side-chain *)
type revision = Int64.t

(** type of a timestamp *)
type timestamp

(** type for a duration in terms of consensus cycles, for use in timeouts *)
type duration

(** A conversation between two parties
    The type embodies an endpoint + state of communication + possibility of reconnection
    Maybe make the state of communication static, with a session type?
 *)
type conversation

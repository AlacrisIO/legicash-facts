(* base.mli -- base types for Legicash platform
   This code is for basic infrastructure somewhat specific to Legicash
*)

exception Timeout of string

exception Double_spend of string


(** sequence number for changes in a side-chain *)
module Revision : Unsigned.S

(** type of a timestamp *)
module Timestamp : Unsigned.S

(** duration in terms of nanoseconds, for use in timeouts. TODO: should the unit be consensus cycles instead? *)
module Duration : Unsigned.S

(** A conversation between two parties
    The type embodies an endpoint + state of communication + possibility of reconnection
    Maybe make the state of communication static, with a session type?
*)
type conversation

module type MapS = sig
  include Map.S

  val lens : key -> ('a t, 'a) Lens.t

  val find_defaulting : (unit -> 'a) -> key -> 'a t -> 'a
end

val defaulting_lens : (unit -> 'b) -> ('a, 'b) Lens.t -> ('a, 'b) Lens.t
(** Assuming that the lens raises Not_found if the value is not found, and then using the provided default, modify the value found (or the default) and put it back in the object *)

module MapMake (Key : Map.OrderedType) : MapS with type key = Key.t


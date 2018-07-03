(* base.mli -- base types for Legicash platform
   This code is for basic infrastructure somewhat specific to Legicash
*)
open Integer

exception Timeout of string

exception Double_spend of string

(** A conversation between two parties
    The type embodies an endpoint + state of communication + possibility of reconnection
    Maybe make the state of communication static, with a session type?
*)
type conversation


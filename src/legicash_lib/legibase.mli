(** Base declarations specific to the Legicash platform *)

exception Timeout of string

exception Double_spend of string

(*(** A conversation between two parties
  The type embodies an endpoint + state of communication + possibility of reconnection
  Maybe make the state of communication static, with a session type?
 *)
  type conversation*)

(** Name of the database, relative to the directory from which everything is run. *)
val db_name : string

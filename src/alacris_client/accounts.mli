(* accounts.mli *)

open Legilogic_lib
open Signing
open Action

open Alacris_lib
open Side_chain_user

val address_to_user_state_tbl : (Address.t,UserState.t)  Hashtbl.t
(** table of current user states, maintained as transactions occur *)

val get_user_name : Address.t -> string
(** user's "friendly" name *)

val get_user_keys : int -> string * Keypair.t
(** get a demo user name and keys from table *)

val prepare_server : unit -> unit Lwt_exn.t
(** setup demo accounts on test net, load facilitator state *)

(* accounts.mli *)

open Legilogic_lib
open Signing

open Legicash_lib
open Side_chain
open Side_chain_user

val address_to_user_state_tbl : (Address.t,UserState.t)  Hashtbl.t
(** table of current user states, maintained as transactions occur *)

val trent_address : Address.t
(** our faclitator's address *)

val get_trent_state : unit -> FacilitatorState.t
(** faciliator state, maintained as transactions occur *)

val get_user_account : Address.t -> AccountState.t
(** user's account state, maintained as transactions occur *)

val get_user_name : Address.t -> string
(** user's "friendly" name *)

val get_user_keys : int -> string * Keypair.t
(** get a demo user name and keys from table *)

val prepare_server : unit -> unit Lwt.t
(** setup demo accounts on test net, load facilitator state *)

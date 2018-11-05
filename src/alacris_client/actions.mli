(* actions.mli *)

open Legilogic_lib
open Yojsoning
open Signing
open Types
open Action

open Alacris_lib
open Side_chain

val error_json : ('a, unit, string, yojson) format4 -> 'a
(** make JSON record with error *)

val get_all_balances_on_trent : unit -> yojson Lwt_exn.t
(** balances for all users *)

val get_transaction_rate_on_trent : unit -> yojson
(** timestamped transaction rate for last minute *)

val get_proof : Revision.t -> yojson Lwt_exn.t
(** Merkle proof for transaction with given tx_revision *)

val apply_main_chain_thread : int -> yojson
(** view deposit/withdrawal thread result *)

val deposit_to : facilitator:Address.t -> Address.t -> TokenAmount.t -> yojson
(** user, number of tokens *)

val withdrawal_from : facilitator:Address.t -> Address.t -> TokenAmount.t -> yojson
(** trent address, user address, number of tokens *)

val payment_on : facilitator:Address.t -> Address.t -> Address.t -> TokenAmount.t -> string -> yojson
(** payment between two users; first address is sender, second is recipient *)

val get_balance_on : facilitator:Address.t -> Address.t -> yojson Lwt_exn.t
(** balance for particular user *)

val get_recent_user_transactions_on_trent : Address.t -> Revision.t option -> yojson Lwt_exn.t
(** int is max number of transactions *)

val get_status_on_trent_and_main_chain : Address.t -> yojson Lwt_exn.t
(** user accounts information on side and main chains *)

val thread_pending_json : yojson
(** JSON returned when a thread is still working *)
(* actions.mli *)

open Legilogic_lib

open Legicash_lib
open Side_chain
open Signing

val error_json : ('a, unit, string, Yojsoning.yojson) format4 -> 'a
(** make JSON record with error *)

val get_all_balances_on_trent : unit -> Yojsoning.yojson Lwt.t
(** balances fora all users *)

val get_transaction_rate_on_trent : unit -> Yojsoning.yojson
(** timestamped transaction rate for last minute *)

val get_proof : int -> Yojsoning.yojson
(** Merkle proof for transaction with given tx_revision *)

val apply_main_chain_thread : int -> Yojsoning.yojson
(** view deposit/withdrawal thread result *)

val deposit_to_trent : Address.t -> TokenAmount.t -> Yojsoning.yojson
(** user, number of tokens *)

val withdrawal_from_trent : Address.t -> TokenAmount.t -> Yojsoning.yojson
(** user, number of tokens *)

val payment_on_trent : Address.t -> Address.t -> TokenAmount.t -> Yojsoning.yojson Lwt.t
(** payment between two users; first address is sender, second is recipient *)

val get_balance_on_trent : Address.t -> Yojsoning.yojson
(** balance for particular user *)

val get_recent_transactions_on_trent : Address.t -> int option -> Yojsoning.yojson
(** int is max number of transactions *)

val get_status_on_trent_and_main_chain : Address.t -> Yojsoning.yojson Lwt.t
(** user accounts information on side and main chains *)

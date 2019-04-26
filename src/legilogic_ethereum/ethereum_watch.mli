open Legilogic_lib
open Types
open Action
open Signing
open Ethereum_json_rpc
open Ethereum_abi

val main_chain_block_notification_stream :
  ?delay:float             (* Wait this long between polls of geth *)
  -> ?start_block:Revision.t (* Don't report until this block number has passed. *)
  -> ?get_block: (?timeout:float -> ?log:bool -> (unit, Revision.t) Lwt_exn.arr) (* Testing affordance. Don't use *)
  -> unit
  -> Revision.t AsyncStream.t Lwt.t (* Stream of block numbers *)
(** [main_chain_block_notification_stream () start delay] is an asynchronous
    stream of notifications that a new block has been observed, based on polling
    geth every [delay] seconds, and starting with block [start] *)

val sleep_delay_exn : float -> unit Lwt_exn.t
(** This function allows to wait for a delay (in seconds) before continuing the computation.
    It handles exceptions *)

val retrieve_relevant_list_logs_data : float -> Address.t -> Digest.t option -> Bytes.t option list -> abi_type list -> abi_value option list -> (LogObject.t * (abi_value list)) list Lwt_exn.t
(** The computation of the list of logs that match the address and topics. There should
    be only one matching entry.
    TODO: named argument for the float. Better description of what the arguments are. *)

val retrieve_relevant_single_logs_data : float -> Address.t -> Digest.t option -> Bytes.t option list -> abi_type list -> abi_value option list -> (LogObject.t * (abi_value list)) Lwt_exn.t
(** TODO: named argument for the float. Better description of what the arguments are. *)

val wait_for_contract_event : Address.t -> Digest.t option -> Bytes.t option list -> abi_type list -> abi_value option list -> (LogObject.t * (abi_value list)) Lwt_exn.t

val retrieve_last_entries_group : Revision.t -> Address.t -> Bytes.t option list list -> (Revision.t * (LogObject.t list list)) Lwt_exn.t
(** TODO: named argument for the float. Better description of what the arguments are. *)

val retrieve_relevant_list_logs_group : float -> Address.t -> Bytes.t option list list -> EthListLogObjects.t list Lwt_exn.t
(** TODO: named argument for the float. Better description of what the arguments are. *)

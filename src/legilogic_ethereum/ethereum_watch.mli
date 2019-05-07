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

val retrieve_relevant_list_logs_data : delay:float -> start_revision:Revision.t -> contract_address:Address.t -> transaction_hash:Digest.t option -> topics:Bytes.t option list -> abi_type list -> abi_value option list -> (Revision.t * (LogObject.t * (abi_value list)) list) Lwt_exn.t
(** The computation of the list of logs that match the address, topics and the value put *)

val retrieve_relevant_single_logs_data : delay:float -> contract_address:Address.t -> transaction_hash:Digest.t option -> topics:Bytes.t option list -> abi_type list -> abi_value option list -> (LogObject.t * (abi_value list)) Lwt_exn.t
(** Retrieve one single entry from the ethereum log. If there is more than one entry
    then a bork is emited. *)

val wait_for_contract_event : contract_address:Address.t -> transaction_hash:Digest.t option -> topics:Bytes.t option list -> abi_type list -> abi_value option list -> (LogObject.t * (abi_value list)) Lwt_exn.t
(** Waiting for one single event from a specific contract address.
   * We may filter (or not) according to the transaction hash
   * We need to provide the type of all values in the output.
   * For each entry in the event log a search value may be precised *)


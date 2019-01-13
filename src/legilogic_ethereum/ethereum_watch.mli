open Legilogic_lib
open Types
open Action
open Signing
open Ethereum_json_rpc
   
val main_chain_block_notification_stream :
  ?delay:float             (* Wait this long between polls of geth *)
  -> ?start_block:Revision.t (* Don't report until this block number has passed. *)
  -> ?get_block: (?timeout:float -> ?log:bool -> (unit, Revision.t) Lwt_exn.arr) (* Testing affordance. Don't use *)
  -> unit
  -> Revision.t AsyncStream.t Lwt.t (* Stream of block numbers *)
(** [main_chain_block_notification_stream () start delay] is an asynchronous
    stream of notifications that a new block has been observed, based on polling
    geth every [delay] seconds, and starting with block [start] *)

val retrieve_relevant_list_logs : float -> Address.t -> Bytes.t option list -> LogObject.t list Lwt_exn.t
(** The computation of the list of logs that match the address and topics. There should
    be only one matching entry *)

val retrieve_relevant_single_logs : float -> Address.t -> Bytes.t option list -> LogObject.t Lwt_exn.t
(** The computation of the list of logs that match the address and topics. There should
    be only one matching entry *)

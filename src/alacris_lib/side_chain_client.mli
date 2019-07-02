(* side_chain_client.mli -- call into side_chain via TCP/IP *)

open Side_chain
open Operator_contract

open Legilogic_lib
open Action
open Signing
open Yojsoning

val post_user_query_request : (UserQueryRequest.t, yojson) Lwt_exn.arr
(** send user query to side chain over TCP/IP *)

(* val post_admin_query_request : (AdminQueryRequest.t, yojson) Lwt_exn.arr *)
(** send admin query to side chain over TCP/IP *)

val post_user_transaction_request : (UserTransactionRequest.t signed, TransactionCommitment.t) Lwt_exn.arr
(** send transaction request to side chain over TCP/IP *)

val get_operator_address_client : unit -> Address.t Lwt.t
(** Address of the configured operator for the current client. *)
(*  TODO: actually support more than one operator in the client. *)

val quadruple_contract_info_for_client : unit -> quadruple_contract Lwt.t
(** Address of the contract address for the current client. *)

module Test : sig
  val post_query_hook : (Query.t, yojson) Lwt_exn.arr ref
  val post_user_transaction_request_hook : (UserTransactionRequest.t signed, TransactionCommitment.t) Lwt_exn.arr ref
end

(* side_chain_client.mli -- call into side_chain via TCP/IP *)

open Side_chain

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

val operator_address : Address.t Lazy.t
(** Address of the configured operator for the current client. *)

module Test : sig
  val post_query_hook : (Query.t, yojson) Lwt_exn.arr ref
  val post_user_transaction_request_hook : (UserTransactionRequest.t signed, TransactionCommitment.t) Lwt_exn.arr ref
end

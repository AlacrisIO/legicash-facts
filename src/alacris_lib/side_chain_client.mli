(* side_chain_client.mli -- call into side_chain via TCP/IP *)

open Side_chain

open Legilogic_lib
open Action
open Signing
open Yojsoning

val post_user_query_request_to_side_chain : UserQueryRequest.t -> yojson Lwt_exn.t
(** send user query to side chain over TCP/IP *)

val post_admin_query_request_to_side_chain : AdminQueryRequest.t -> yojson Lwt_exn.t
(** send admin query to side chain over TCP/IP *)

val post_user_transaction_request_to_side_chain : UserTransactionRequest.t signed -> TransactionCommitment.t Lwt_exn.t
(** send transaction request to side chain over TCP/IP *)

val facilitator_address : Address.t Lazy.t
(* Address of the configured facilitator for the current client.
   TODO: actually support more than one facilitator in the client. *)

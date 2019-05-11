open Legilogic_lib
open Yojsoning
open Persisting
open Action
open Signing

open Side_chain

exception Operator_not_found of string
exception Malformed_request  of string

(** Private state of a operator (as opposed to what's public in the side-chain)
    TODO: lawsuits? index expedited vs non-expedited transactions? multiple pending confirmations?
    Remember operations pending operations with the main chain?
    Include a Ethereum_chain.user_state? State for dealing with the court registry? *)
module OperatorState : sig
  type t = { keypair: Keypair.t
           ; committed: State.t signed
           ; current: State.t
           ; fee_schedule: OperatorFeeSchedule.t }
  [@@deriving lens { prefix=true }]
  include PersistableS with type t := t
  val load : Address.t -> t
end

val initial_operator_state : Address.t -> OperatorState.t

module OperatorAction : ActionS with type state = OperatorState.t
module OperatorAsyncAction : AsyncActionS with type state = OperatorState.t

(** Type of a lens to access some individual account inside some operator's state *)
type account_lens = (OperatorState.t, AccountState.t) Lens.t

(* TODO: rename to make_account_lens or just account_lens ? *)
val operator_account_lens : Address.t -> account_lens

(** start the background operator processes for given operator address *)
val start_operator : (Address.t, unit) Lwt_exn.arr

(** [oper_post_user_transaction_request request] asynchronously processes [request] (not forced)
    returning a [Transaction.t] on success.
*)
val oper_post_user_transaction_request : (UserTransactionRequest.t signed, TransactionCommitment.t) Lwt_exn.arr

(** [oper_post_user_query_request request] asynchronously processes [request]
    returning a [Transaction] on success. *)
val oper_post_user_query_request : (UserQueryRequest.t, yojson) Lwt_exn.arr

(** [oper_post_admin_query_request request] asynchronously processes [request]
    returning a [Transaction] on success. *)
val oper_post_admin_query_request : (AdminQueryRequest.t, yojson) Lwt_exn.arr


val start_state_update_periodic_operator : unit -> unit Lwt_exn.t
(** start of the operator that do state_update at frequent date
    (for example every 25s). *)

(*
   (** For a operator, commit the state of the side-chain to the main-chain *)
   val commit_operator_state : (unit, unit) OperatorAsyncAction.arr

   (* val embed_request: (user_request, Ethereum_chain.transaction) user_action *)

   (** Flow 3 Step 2: Trent, who follows the main chain, checks for such exit requests.
   When one is found, Trent is on notice to post an update of his side-chain within
   an allowed deadline, that features a confirmation for these requests.
   Alternatively, Trent fails, and bankruptcy proceedings start — see Flow 6, 7 and 8.
 *)
   val check_main_chain_for_exits : (unit, UserRequest.t list) OperatorAsyncAction.arr
*)

module Test : sig
  (** Backdoor to get a reference to the current state of the operator.
      NB 1: You're not allowed to write it, only the operator code is (it's a pure value, anyway).
      NB 2: Thou shalt only use it but by permission of the owner.
      It is NOT OK to probe into other people's internals except for e.g. testing and debugging.
  *)
  val get_operator_state : unit -> OperatorState.t
end

open Legilogic_lib
open Yojsoning
open Action
open Signing

open Side_chain

module FacilitatorAction : ActionS with type state = FacilitatorState.t
module FacilitatorAsyncAction : AsyncActionS with type state = FacilitatorState.t

type account_lens = (FacilitatorState.t, AccountState.t) Lens.t

val facilitator_account_lens : Address.t -> account_lens

(** start the background facilitator processes for given address *)
val start_facilitator : (Address.t, unit) Lwt_exn.arr

(** [post_user_transaction_request request] asynchronously processes [request] (not forced)
    returning a [Transaction.t] on success.
*)
val post_user_transaction_request : (UserTransactionRequest.t signed, TransactionCommitment.t) Lwt_exn.arr

(** [post_user_query_request request] asynchronously processes [request]
    returning a [Transaction] on success. *)
val post_user_query_request : (UserQueryRequest.t, yojson) Lwt_exn.arr

(** [post_admin_query_request request] asynchronously processes [request]
    returning a [Transaction] on success. *)
val post_admin_query_request : (AdminQueryRequest.t, yojson) Lwt_exn.arr


(*
   (** For a facilitator, commit the state of the side-chain to the main-chain *)
   val commit_facilitator_state : (unit, unit) FacilitatorAsyncAction.arr

   (* val embed_request: (user_request, Main_chain.transaction) user_action *)

   (** Flow 3 Step 2: Trent, who follows the main chain, checks for such exit requests.
   When one is found, Trent is on notice to post an update of his side-chain within
   an allowed deadline, that features a confirmation for these requests.
   Alternatively, Trent fails, and bankruptcy proceedings start — see Flow 6, 7 and 8.
 *)
   val check_main_chain_for_exits : (unit, UserRequest.t list) FacilitatorAsyncAction.arr
*)

module Test : sig
  (** Backdoor to get a reference to the current state of the facilitator.
      NB 1: You're not allowed to write it, only the facilitator code is (it's a pure value, anyway).
      NB 2: Thou shalt only use it but by permission of the owner.
      It is NOT OK to probe into other people's internals except for e.g. testing and debugging.
  *)
  val get_facilitator_state : unit -> FacilitatorState.t
end

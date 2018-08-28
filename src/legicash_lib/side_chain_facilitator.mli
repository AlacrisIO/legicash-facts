open Legilogic_lib
open Action
open Signing

open Side_chain

module FacilitatorAction : ActionS with type state = FacilitatorState.t
module FacilitatorAsyncAction : AsyncActionS with type state = FacilitatorState.t

type account_lens = (FacilitatorState.t, AccountState.t) Lens.t

val facilitator_account_lens : Address.t -> account_lens

(** start the background facilitator processes for given address *)
val start_facilitator : Address.t -> unit Lwt.t

(** Backdoor to get a reference to the current state of the facilitator.
    NB 1: You're not allowed to write it, only the facilitator code is (it's a pure value, anyway).
    NB 2: Thou shalt only use it but by permission of the owner.
    It is NOT OK to probe into other people's internals except for e.g. testing and debugging.
*)
val get_facilitator_state : unit -> FacilitatorState.t

(** [process_request (request, is_forced)] asynchronously processes [request]
   according to whether it [is_forced], returning a [Transaction] on success. *)
val process_request : (Request.t signed * bool, Transaction.t) Lwt_exn.arr

val commit_facilitator_state : (unit, unit) FacilitatorAsyncAction.arr
(** For a facilitator, commit the state of the side-chain to the main-chain *)

(* val embed_request: (user_request, Main_chain.transaction) user_action *)

val check_main_chain_for_exits : (unit, Request.t list) FacilitatorAsyncAction.arr
(** Flow 3 Step 2: Trent, who follows the main chain, checks for such exit requests.
    When one is found, Trent is on notice to post an update of his side-chain within
    an allowed deadline, that features a confirmation for these requests.
    Alternatively, Trent fails, and bankruptcy proceedings start — see Flow 6, 7 and 8.
*)

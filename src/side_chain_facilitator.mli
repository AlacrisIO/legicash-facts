open Crypto
open Side_chain

val process_request : (Request.t signed, Confirmation.t signed) facilitator_async_action
(** Flow 1 Step 5: facilitator acknowledges deposit, stores it and sends it to user *)

val commit_facilitator_state : (unit, unit) facilitator_action
(** For a facilitator, commit the state of the side-chain to the main-chain *)

(* val embed_request: (user_request, Main_chain.transaction) user_action *)

val check_main_chain_for_exits : (unit, Request.t list) facilitator_action
(** Flow 3 Step 2: Trent, who follows the main chain, checks for such exit requests.
    When one is found, Trent is on notice to post an update of his side-chain within
    an allowed deadline, that features a confirmation for these requests.
    Alternatively, Trent fails, and bankruptcy proceedings start — see Flow 6, 7 and 8.
*)

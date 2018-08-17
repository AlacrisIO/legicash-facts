open Lib

(** 'output or exception *)
type 'output or_exn = ('output, exn) result

(** function from 'input to 'output that acts on a 'state and may return an exception *)
type ('input, 'output, 'state) action = 'input -> 'state -> 'output or_exn * 'state

(** asynchronous function from 'input to 'output that acts on a 'state and may return an exception *)
type ('input, 'output, 'state) async_action = 'input -> 'state -> ('output or_exn * 'state) Lwt.t

exception Assertion_failed of string

module type StatefulErrableActionS = sig
  type state
  include MonadS

  (** computations that need to read the state can use with_state *)
  val with_state : ('a * state, 'b) arr -> ('a, 'b) arr

  (** computations that need to write the state can use return_state *)
  val return_state : state -> ('a, 'a) arr

  (** change the state, otherwise the identity arrow *)
  val map_state : (state -> state) -> ('a, 'a) arr

  (** computations that fail *)
  val fail : exn -> _ t

  (** a pure computation can read the state but have no other side-effect *)
  type ('i, 'o) pure = 'i -> state -> 'o
  val of_pure : ('i, 'o) pure -> ('i, 'o) arr

  (** given a pure computation returning a bool, make an action that asserts the bool is true,
      and if not fails with an Assertion_failed with a string computed by calling the thunk where,
      which might compose a message based on Pervasives.__LOC__ or __POS__. *)
  val assert_: (unit -> string) -> ('i, bool) pure -> ('i, 'i) arr

  (** run a computation on a global state ref. *)
  val run : state ref -> 'a -> ('a, 'b) arr -> 'b
end

module type ActionS = sig
  type state
  include StatefulErrableActionS
    with type state := state
     and type 'a t = state -> 'a or_exn * state
end

module Action (State : TypeS) : ActionS with type state = State.t

module type AsyncActionS = sig
  type state
  include StatefulErrableActionS
    with type state := state
     and type 'a t = state -> ('a or_exn * state) Lwt.t

  (** run a computation on a global state ref in an existing Lwt context *)
  val run_lwt : state ref -> 'a -> ('a, 'b) arr -> 'b Lwt.t
  val of_action : ('a -> state -> 'b or_exn * state) -> ('a, 'b) arr
end
module AsyncAction (State : TypeS) : AsyncActionS with type state = State.t

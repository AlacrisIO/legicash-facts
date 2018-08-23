open Lib

(** 'output or exception *)
type 'output or_exn = ('output, exn) result

(** function from 'input to 'output that acts on a 'state and may return an exception *)
type ('input, 'output, 'state) action = 'input -> 'state -> 'output or_exn * 'state

(** asynchronous function from 'input to 'output that acts on a 'state and may return an exception *)
type ('input, 'output, 'state) async_action = 'input -> 'state -> ('output or_exn * 'state) Lwt.t

exception Assertion_failed of string

module type StatefulErrableActionS = sig
  include StateMonadS
  include ErrorMonadS
    with type error = exn
     and type 'a t := 'a t
     and type ('i, 'o) arr := ('i, 'o) arr

  (** given a readonly computation returning a bool, make an action that asserts the bool is true,
      and if not fails with an Assertion_failed with a string computed by calling the thunk where,
      which might compose a message based on Pervasives.__LOC__ or __POS__. *)
  val assert_: (unit -> string) -> ('i, bool) readonly -> ('i, 'i) arr

  (** run a computation on a global state ref. *)
  val run : state ref -> ('a, 'b) arr -> 'a -> 'b
end

module type ActionS = sig
  type state
  include StatefulErrableActionS
    with type state := state
     and type 'a t = state -> 'a or_exn * state
     and type ('i, 'o) readonly = 'i -> state -> 'o
  val to_async : ('a, 'b) arr -> 'a -> state -> ('b or_exn * state) Lwt.t
end

module Action (State : TypeS) : ActionS
  with type state = State.t
   and type ('i, 'o) readonly = 'i -> State.t -> 'o

module type AsyncActionS = sig
  type state
  include StatefulErrableActionS
    with type state := state
     and type error = exn
     and type 'a t = state -> ('a or_exn * state) Lwt.t
     and type ('i, 'o) readonly = 'i -> state -> 'o

  (** run a computation on a global state ref in an existing Lwt context *)
  val run_lwt_exn : state ref -> ('a, 'b) arr -> 'a -> 'b or_exn Lwt.t
  val run_lwt : state ref -> ('a, 'b) arr -> 'a -> 'b Lwt.t

  val of_action : ('a -> state -> 'b or_exn * state) -> ('a, 'b) arr
end
module AsyncAction (State : TypeS) : AsyncActionS
  with type state = State.t
   and type 'a t = State.t -> ('a or_exn * State.t) Lwt.t
   and type ('i, 'o) readonly = 'i -> State.t -> 'o

module Lwt_monad : MonadS with type 'a t = 'a Lwt.t

module Lwt_exn : sig
  include ErrorMonadS with type 'a t = 'a or_exn Lwt.t
  val run_lwt : ('a, 'b) arr -> 'a -> 'b Lwt.t
  val run : ('a, 'b) arr -> 'a -> 'b
end

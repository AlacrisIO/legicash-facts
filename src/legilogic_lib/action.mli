open Lib

(** 'output or exception *)
type +'output or_exn = ('output, exn) result

(** function from 'input to 'output that acts on a 'state and may return an exception *)
type (-'input, +'output, 'state) action = 'input -> 'state -> 'output or_exn * 'state

(** asynchronous function from 'input to 'output that acts on a 'state and may return an exception *)
type (-'input, +'output, 'state) async_action = 'input -> 'state -> ('output or_exn * 'state) Lwt.t

exception Assertion_failed of string

module type FunctorS = sig
  type _ t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val (<$>) : ('a -> 'b) -> 'a t -> 'b t
end
module type ApplicativeS = sig
  include FunctorS
  val pure : 'a -> 'a t (* return of the monad *)
  val ap : ('a -> 'b) t -> ('a t -> 'b t)
end
module type ArrowS = sig
  type ('i, 'o) arr
  val returnA : ('a, 'a) arr
  val arr : ('i -> 'o) -> ('i, 'o) arr
  val (>>>) : ('a, 'b) arr -> ('b, 'c) arr -> ('a, 'c) arr
  val const : 'a -> (_, 'a) arr
  val forever : ('a, 'a) arr -> ('a, _) arr
end

module type MonadBaseS = sig
  type _ t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end
module type MonadS = sig
  include MonadBaseS
  include ApplicativeS with type 'a t := 'a t
  include ArrowS with type ('i, 'o) arr = 'i -> 'o t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
end
module Monad (M : MonadBaseS) : MonadS with type 'a t = 'a M.t

module type ErrorMonadS = sig
  type error
  include MonadS

  (** computations that fail *)
  val fail : error -> _ t

  (** Reifying errors *)
  val trying : ('i,'o) arr -> ('i, ('o, error) result) arr

  (** Handling errors *)
  val handling : (error,'a) arr -> (('a, error) result, 'a) arr
end

module ErrorMonad (Error: TypeS) : ErrorMonadS
  with type error = Error.t
   and type 'a t = ('a, Error.t) result
   and type ('i, 'o) arr = 'i -> ('o, Error.t) result

module type ExnMonadS = sig
  include ErrorMonadS with type error = exn

  (** Internal error *)
  val bork : ('a, unit, string, 'b t) format4 -> 'a

  (** Running an OCaml function that may raise exceptions, and catch them as the error of the monad *)
  val catching : ('i -> 'o) -> ('i, 'o) arr
end

module ExnMonad : ExnMonadS
  with type 'a t = 'a or_exn
   and type ('i, 'o) arr = 'i -> 'o or_exn

module type ReaderMonadBaseS = sig
  type state
  include MonadBaseS
  val state : state t
end
module type ReaderMonadS = sig
  include ReaderMonadBaseS
  include MonadS with type 'a t := 'a t
  val get_state : _ -> state t
  val pair_with_state : 'a -> ('a * state) t
end
module ReaderMonadMethods (B: ReaderMonadBaseS) : sig
  type state = B.state
  val state : state B.t
  val get_state : _ -> state B.t
  val pair_with_state : 'a -> ('a * state) B.t
end
module ReaderMonad (State: TypeS) : ReaderMonadS
  with type state = State.t
   and type 'a t = State.t -> 'a

module type StateMonadBaseS = sig
  include ReaderMonadBaseS
  val put_state : state -> unit t
end
module type StateMonadS = sig
  include StateMonadBaseS
  include ReaderMonadS with type state := state and type 'a t := 'a t

  (** change the state, otherwise the identity arrow *)
  val map_state : (state -> state) -> ('a, 'a) arr

  (** a pure computation can read the state but have no other side-effect *)
  type ('i, 'o) readonly
  val of_readonly : ('i, 'o) readonly -> ('i, 'o) arr
end
module StateMonadMethods (B: StateMonadBaseS) : sig
  include module type of ReaderMonadMethods(B)
  val map_state : (B.state -> B.state) -> 'a -> 'a B.t
end
module StateMonad (State: TypeS) : StateMonadS
  with type state := State.t
   and type 'a t = State.t -> ('a * State.t)
   and type ('i, 'o) readonly = 'i -> State.t -> 'o

module Lwt_monad : MonadS with type 'a t = 'a Lwt.t

module Lwt_exn : sig
  include ExnMonadS
    with type 'a t = 'a or_exn Lwt.t
     and type ('i, 'o) arr = ('i, 'o or_exn) Lwt_monad.arr
  val run_lwt : ('a, 'b) arr -> 'a -> 'b Lwt.t
  val run : ('a, 'b) arr -> 'a -> 'b
  val of_lwt : ('a -> 'b Lwt.t) -> ('a, 'b) arr
  val list_iter_s : ('a -> unit t) -> 'a list -> unit t
  val list_iter_p : ('a -> unit t) -> 'a list -> unit t
  val printf : ('a, unit, string, unit t) format4 -> 'a
  val eprintf : ('a, unit, string, unit t) format4 -> 'a
end

module type StatefulErrableActionS = sig
  include StateMonadS
  include ExnMonadS
    with type 'a t := 'a t
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
  val of_lwt_exn : ('a, 'b) Lwt_exn.arr -> ('a, 'b) arr
  val of_lwt : ('a, 'b) Lwt_monad.arr -> ('a, 'b) arr
  (*  val catching_lwt : ('a, 'b) Lwt_monad.arr -> ('a, 'b) arr
      val catching_lwt_exn : ('a, 'b) Lwt_exn.arr -> ('a, 'b) arr *)
end
module AsyncAction (State : TypeS) : AsyncActionS
  with type state = State.t
   and type 'a t = State.t -> ('a or_exn * State.t) Lwt.t
   and type ('i, 'o) readonly = 'i -> State.t -> 'o

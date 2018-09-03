open Lib

(** 'output or exception *)
type +'output or_exn = ('output, exn) result

(** function from 'input to 'output that acts on a 'state and may return an exception *)
type (-'input, +'output, 'state) action = 'input -> 'state -> 'output or_exn * 'state

(** asynchronous function from 'input to 'output that acts on a 'state and may return an exception *)
type (-'input, +'output, 'state) async_action = 'input -> 'state -> ('output or_exn * 'state) Lwt.t

exception Assertion_failed of string

(** In the context of this application, there is no need to understand this
    abstraction in its full generality. It is only used in [Monad], and the
    docstrings for its methods only apply to that context. *)
module type FunctorS = sig
  type _ t

  (** "Rips off" the monadic wrapper [t] from an [x: 'a t], and returns [f x] *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** Shorthand for [map]. *)
  val (<$>) : ('a -> 'b) -> 'a t -> 'b t
end

(** In the context of this application, there is no need to understand this
    abstraction, as its functionality is currently unused. XXX: Delete? *)
module type ApplicativeS = sig
  include FunctorS
  val pure : 'a -> 'a t (* Akin to [Monad.return] *)
  val ap : ('a -> 'b) t -> ('a t -> 'b t)
end

(** Signature for representing processes which take ['i]s and outputs ['o]s
    See https://wiki.haskell.org/Arrow_tutorial and
    http://hackage.haskell.org/package/base-4.11.1.0/docs/Control-Arrow.html
    for more information.

    In the context of this application, it is only necessary to understand
    Kleisli arrows, as this abstraction is only used in [Monad]s. *)
module type ArrowS = sig

  (** The input and output types *)
  type ('i, 'o) arr

  (** "Identity" arrow. Akin to [Monad.return] *)
  val returnA : ('a, 'a) arr

  (** Lift a function to an arrow which represents execution of that function *)
  val arr : ('i -> 'o) -> ('i, 'o) arr

  (** Composition of arrows: Take output from first process, and feed it into
      the second. *)
  val (>>>) : ('a, 'b) arr -> ('b, 'c) arr -> ('a, 'c) arr

  (** Process which does nothing but return a particular ['a] *)
  val const : 'a -> (_, 'a) arr

  (** Process which repeatedly feeds its own output back into itself. *)
  val forever : ('a, 'a) arr -> ('a, _) arr
end

(** Signatures of the fundamental monadic operations *)
module type MonadBaseS = sig
  type _ t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(** Signature for a simpler/more powerful representation of composable
    computational effects than [ArrowS], applicable when process inputs can be
    represented as parameters of a function (the function passed to [bind])

    For more information, see
    - https://wiki.haskell.org/Monad
    - https://stackoverflow.com/a/25213104/1941213
    - http://www.cse.chalmers.se/~rjmh/afp-arrows.pdf *)
module type MonadS = sig
  include MonadBaseS
  include ApplicativeS with type 'a t := 'a t
  include ArrowS with type ('i, 'o) arr = 'i -> 'o t (* Kleisli arrows *)

  (** Synonym for [bind]. Composes an ['a t] monad with an ['a -> 'bt] monadic
      function *)
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  (** Lift a function on base types to a function on the monadic wrappers *)
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
end
(** See docstring for MonadS *)
module Monad (M : MonadBaseS) : MonadS with type 'a t = 'a M.t

(** Signature for monadic representation of computational processes with
    reportable failure states, and short-circuit logic on failure. *)
module type ErrorMonadS = sig
  type error
  include MonadS

  (** Returns monad in error state *)
  val fail : error -> _ t

  (** Lift an arrow to one which can output an error.
      XXX: Don't understand how this works. *)
  val trying : ('i,'o) arr -> ('i, ('o, error) result) arr

  (** Handling errors. XXX: Don't understand this... *)
  val handling : (error,'a) arr -> (('a, error) result, 'a) arr
end

(** See docstring for ErrorMonadS. Note that here the base type for the monad is
    [('a, Error.t) result]. *)
module ErrorMonad (Error: TypeS) : ErrorMonadS
  with type error = Error.t
   and type 'a t = ('a, Error.t) result
   and type ('i, 'o) arr = 'i -> ('o, Error.t) result

(** Signature for an [ErrorMonadS] which catches exceptions and returns them as
    monadic errors, i.e., as values of type [error], which can be recognized as
    such by subsequent [ErrorMonadS]. *)
module type ExnMonadS = sig
  include ErrorMonadS with type error = exn

  (** Internal error *)
  val bork : ('a, unit, string, 'b t) format4 -> 'a

  (** Run an OCaml function that may raise exceptions, which are caught and
      presented as monadic errors *)
  val catching : ('i -> 'o) -> ('i, 'o) arr
end

(** See docstring for ExnMonadS.  *)
module ExnMonad : ExnMonadS
  with type 'a t = 'a or_exn
   and type ('i, 'o) arr = 'i -> 'o or_exn

(** Signature for expression of composable computational effects which have a
    tracked state threaded through them (the [state] variable inherited from
    [ReaderMonadS], but note that no other [ReaderMonadS] functionality is used
    here.) See https://wiki.haskell.org/State_Monad for more details.

    The inputs/outputs to the computations, and their corresponding states, are
    represented internally as pairs (value, state). We use that representation
    here to describe method effects. *)
module type StateMonadS = sig
  type state
  include MonadBaseS
  include MonadS with type 'a t := 'a t
  val state : state t

  (** [map_state f x s] represents a monadic transition from (x, s) to (x, f s).
      I.e., only the state is changed; the input is output unchanged. *)
  val map_state : (state -> state) -> 'a -> 'a t

  type ('i, 'o) readonly

  (** [of_readonly r x s] represents monadic transition from (x, s) to (r x s,s)
      I.e., only the value is changed; the state remains unchanged. *)
  val of_readonly : ('i, 'o) readonly -> ('i, 'o) arr
end

(** See docstring for [StateMonadS] *)
module StateMonad (State: TypeS) : StateMonadS
  with type state := State.t
   and type 'a t = State.t -> ('a * State.t)
   and type ('i, 'o) readonly = 'i -> State.t -> 'o

(** Translation of Lwt monadic notation to ours, for consistency. *)
module Lwt_monad : MonadS with type 'a t = 'a Lwt.t

(** Lwt computations, with exceptions caught and presented as monadic errors. *)
module Lwt_exn : sig
  include ExnMonadS
    with type 'a t = 'a or_exn Lwt.t
     and type ('i, 'o) arr = ('i, 'o or_exn) Lwt_monad.arr

  (** [run_lwt mf x] represents applying Lwt process [mf] to x, and throwing an
      exception on failure which will be caught by the [ExnMonadS] machinery. *)
  val run_lwt : ('a, 'b) arr -> 'a -> 'b Lwt.t

  (** Run a [run_lwt] thread as the main process via [Lwt_main.run] *)
  val run : ('a, 'b) arr -> 'a -> 'b

  (** [of_lwt a x] is the result of applying Lwt process [a] to [x] *)
  val of_lwt : ('a -> 'b Lwt.t) -> ('a, 'b) arr

  (** Kick off an [of_lwt] process for each element of the list, in series *)
  val list_iter_s : ('a -> unit t) -> 'a list -> unit t

  (** Kick off an [of_lwt] process for each element of the list, in parallel *)
  val list_iter_p : ('a -> unit t) -> 'a list -> unit t

  (** Print to stdout via [Lwt_io] *)
  val printf : ('a, unit, string, unit t) format4 -> 'a

  (** Print to stderr via [Lwt_io] *)
  val eprintf : ('a, unit, string, unit t) format4 -> 'a
end

module type StatefulErrableActionS = sig
  include StateMonadS
  include ExnMonadS
    with type 'a t := 'a t
     and type ('i, 'o) arr := ('i, 'o) arr

  (** [assert_ where test value state] is an action which runs a readonly
     computation on the [state], namely [test value state] which returns a
     [bool], and returns [value] if that computation is [true] but otherwise
     raises [Assertion_failed] with a string computed by calling the thunk
     [where]. [where] might compose a message based on Pervasives.__LOC__ or
     __POS__. *)
  val assert_: (unit -> string) -> ('i, bool) readonly -> ('i, 'i) arr

  (** run a computation on a global state ref, and either return the value or
     raise an exception (which will get caught by the [ExnMonad] machinery.) *)
  val run : state ref -> ('a, 'b) arr -> 'a -> 'b
end

(** Synchronous action on a state monad with monadic error handling. *)
module type ActionS = sig
  type state
  include StatefulErrableActionS
    with type state := state
     and type 'a t = state -> 'a or_exn * state
     and type ('i, 'o) readonly = 'i -> state -> 'o
  (** [to_async a x s] converts [a x s] to an [Lwt.return] value so it can be
      included in Lwt process chains. *)
  val to_async : ('a, 'b) arr -> 'a -> state -> ('b or_exn * state) Lwt.t
end

(** See docstring for [ActionS] *)
module Action (State : TypeS) : ActionS
  with type state = State.t
   and type ('i, 'o) readonly = 'i -> State.t -> 'o

(** Asynchronous action on a state monad with monadic error handling. *)
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

  (** Synchronous action result, wrapped in Lwt monadic machinery *)
  val of_action : ('a -> state -> 'b or_exn * state) -> ('a, 'b) arr
  (** [Lwt_exn] monad to [Lwt] monad *)
  val of_lwt_exn : ('a, 'b) Lwt_exn.arr -> ('a, 'b) arr
  (** [Lwt_monad] monad to [Lwt] monad *)
  val of_lwt : ('a, 'b) Lwt_monad.arr -> ('a, 'b) arr
  (*  val catching_lwt : ('a, 'b) Lwt_monad.arr -> ('a, 'b) arr
      val catching_lwt_exn : ('a, 'b) Lwt_exn.arr -> ('a, 'b) arr *)
end

(** See docstring for [AsyncActionS] *)
module AsyncAction (State : TypeS) : AsyncActionS
  with type state = State.t
   and type 'a t = State.t -> ('a or_exn * State.t) Lwt.t
   and type ('i, 'o) readonly = 'i -> State.t -> 'o

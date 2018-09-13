open Lib

(** 'output or exception *)
type +'output or_exn = ('output, exn) result

(** function from 'input to 'output that acts on a 'state *)
type (-'input, +'output, 'state) action = 'input -> 'state -> 'output * 'state

(** asynchronous function from 'input to 'output that acts on a 'state *)
type (-'input, +'output, 'state) async_action = 'input -> 'state -> ('output * 'state) Lwt.t

(** function from 'input to 'output that acts on a 'state and may return an exception *)
type (-'input, +'output, 'state) exn_action = ('input, 'output or_exn, 'state) action

(** asynchronous function from 'input to 'output that acts on a 'state and may return an exception *)
type (-'input, +'output, 'state) async_exn_action = ('input, 'output or_exn, 'state) async_action



exception Assertion_failed of string

(** This is the standard notion of Functor as in Haskell, Scala, and other OCaml libraries,
    and distantly related to the notion of "functor" in the OCaml module system.
    Actually, from a Categorical perspective, it's a endofunctor in the category Type of types,
    i.e. a functor from Type to Type itself.
    TODO: use an actual Monad library, possibly the one from Jane Street.
    It applies to List, Option, Monad, ErrorMonad, etc.
*)
module type FunctorS = sig
  type _ t

  (** Mapping a function.
      Interpretation 1: [t] being an endofunctor from the category Type of types to Type itself,
      it is not only possible to send each ['a] pointwise to ['a t] via [t],
      but also to send each morphism [f : 'a -> 'b] into a morphism [map f : 'a t -> 'b t].
      Interpretation 2: applying [map f] applies the pure function [f] "under" the functorial
      wrapper [t] to each value in ['a] notionally wrapped in a ['a t].
  *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** Haskell-style infix shorthand for [map]. *)
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
  include ArrowS with type ('i, 'o) arr = 'i -> 'o t (* Kleisli arrows, a.k.a. monadic function *)

  (** Synonym for [bind]. Composes an ['a t] monad with an ['a -> 'b t] arrow *)
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  (** Lift a function on base types to a function on the monadic wrappers.
      NB: It's just [flip]ping the arguments of [map]. *)
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

  (** Reifying the ambient result of a computation into an explicit result as a pure (Ok) value.
      It is the monadic equivalent of the "try" part of OCaml's "try/with" construct.
      Use "handling" below for the "with" part, or build your own more powerful constructs instead,
      for instance a test primitive that only succeeds if the computation failed suitably.
  *)
  val trying : ('i,'o) arr -> ('i, ('o, error) result) arr

  (** Handling errors from a computation the result of which was previously reified with [trying]
      It is the monadic equivalent of the "with" part of OCaml's "try/with" construct *)
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

(** Composing the Lwt and Exn monads:
    representing success or failure with an or_exn while using the Lwt monad for threading. *)
module Lwt_exn : sig
  include ExnMonadS
    with type 'a t = 'a or_exn Lwt.t
     and type ('i, 'o) arr = ('i, 'o or_exn) Lwt_monad.arr

  (** [run_lwt a x] runs an Lwt_exn arrow [a] on an initial value [x] in an existing Lwt context,
      expressing failure with [or_exn] in a monadic way inside the evaluation of [mf], and
      raising an exception at the very end if the evaluation failed. *)
  val run_lwt : ('a, 'b) arr -> 'a -> 'b Lwt.t

  (** Run a [run_lwt a x] arrow [a] on an initial value [x] in a new Lwt context,
      using [Lwt_main.run] and raising an exception at the very end if the evaluation failed. *)
  val run : ('a, 'b) arr -> 'a -> 'b

  (** [of_exn a] given a ExnMonad arrow [a] that has no asynchronous effects, returns an arrow *)
  val of_exn : ('a, 'b) ExnMonad.arr -> ('a, 'b) arr

  (** [of_lwt a] given a Lwt arrow [a] that doesn't fail returns a Lwt_exn arrow that
      returns its successful result. *)
  val of_lwt : ('a -> 'b Lwt.t) -> ('a, 'b) arr

  (** Kick off a thread with unit result for each element of the list, in series *)
  val list_iter_s : ('a -> unit t) -> 'a list -> unit t

  (** Kick off a thread with unit result for each element of the list, in parallel *)
  val list_iter_p : ('a -> unit t) -> 'a list -> unit t

  (** Print to stdout via [Lwt_io], then flush *)
  val printf : ('a, unit, string, unit t) format4 -> 'a

  (** Print to stderr via [Lwt_io], then flush *)
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

  (** run a Lwt_exn computation on a global state ref; at the end, update the state ref
   *then* either return the value (if successful) or raise an exception (if one was caught
      via the [ExnMonad] machinery.) *)
  val run : state ref -> ('a, 'b) arr -> 'a -> 'b
end

(** Synchronous action on a state monad with monadic error handling. *)
module type ActionS = sig
  type state
  include StatefulErrableActionS
    with type state := state
     and type 'a t = state -> 'a or_exn * state
     and type ('i, 'o) readonly = 'i -> state -> 'o

  (** [to_async] converts an Action into a corresponding AsyncAction that uses Lwt for threading. *)
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

  (** From an [Action] to an [AsyncAction] *)
  val of_action : ('a -> state -> 'b or_exn * state) -> ('a, 'b) arr
  (** From a [Lwt_exn] arrow to an AsyncAction arrow *)
  val of_lwt_exn : ('a, 'b) Lwt_exn.arr -> ('a, 'b) arr
  (** From a [Lwt] arrow to an AsyncAction arrow *)
  val of_lwt : ('a, 'b) Lwt_monad.arr -> ('a, 'b) arr
  (*  val catching_lwt : ('a, 'b) Lwt_monad.arr -> ('a, 'b) arr
      val catching_lwt_exn : ('a, 'b) Lwt_exn.arr -> ('a, 'b) arr *)
end

(** See docstring for [AsyncActionS] *)
module AsyncAction (State : TypeS) : AsyncActionS
  with type state = State.t
   and type 'a t = State.t -> ('a or_exn * State.t) Lwt.t
   and type ('i, 'o) readonly = 'i -> State.t -> 'o

(** Given a mailbox and a way to make messages from a pair of input and output-co-promise, return
    an Lwt Kleisli arrow that goes from input to output *)
val simple_client : 'msg Lwt_mvar.t -> ('i * 'o Lwt.u -> 'msg) -> ('i, 'o) Lwt_monad.arr

(** Given a mailbox for messages being a pair of input and output-co-promise,
    and given an AsyncAction arrow for some type of state, and an initial state of that type,
    return a background thread that sequentially processes those messages. *)
val simple_server : ('i * 'o Lwt.u) Lwt_mvar.t -> ('i, 'o, 'state) async_action -> 'state -> _ Lwt.t

(** Given an AsyncAction arrow for some type of state, return a client Lwt arrow that can
    call the asynchronous action in a sequentialized way, and an action that given an initial state
    for the server will create a background server thread to actually process the client requests. *)
val simple_client_make_server : ('i, 'o, 'state) async_action ->
  ('i, 'o) Lwt_monad.arr * ('state -> _ Lwt.t)

(** Given a mailbox for messages being a pair of input and output-co-promise,
    and given an Lwt arrow return a background thread that sequentially processes those messages. *)
val stateless_server : ('i * 'o Lwt.u) Lwt_mvar.t -> ('i, 'o) Lwt_monad.arr -> _ Lwt.t

(** Given an AsyncAction arrow and an initial state,
    return a client Lwt arrow that provides sequentialized access to the action arrow *)
val sequentialize : ('i, 'o, 'state) async_action -> 'state -> ('i, 'o) Lwt_monad.arr

(** Given an AsyncAction arrow and an initial state,
    return a client Lwt arrow that provides sequentialized access to the action arrow *)
val stateless_sequentialize : ('i, 'o) Lwt_monad.arr -> ('i, 'o) Lwt_monad.arr

(*
   (** Given a mailbox for messages being a pair of input and output-co-promise,
   and given an Lwt arrow return a background thread that processes those messages in parallel.
   In a given process, you might as well do without a mailbox, but if you have to have a mailbox anyway...
 *)

   val stateless_parallel_server : ('i * 'o Lwt.u) Lwt_mvar.t -> ('i, 'o) Lwt_monad.arr -> _ Lwt.t

   (** Given a Lwt arrow, return a client Lwt arrow that does the same thing,
   but going through the bottleneck of a mailbox.
   In a given process, you might as well do without the entire mailbox thing! *)
   val stateless_parallelize : ('i, 'o) Lwt_monad.arr -> ('i, 'o) Lwt_monad.arr

*)

(* reading, writing strings from Lwt_io channels *)

val read_string_from_lwt_io_channel : Lwt_io.input_channel -> string Lwt_exn.t
(** read a string from an Lwt_io.input_channel, which must have been written by
    write_string_lwt_io_channel
*)

val write_string_to_lwt_io_channel : Lwt_io.output_channel -> string -> unit Lwt_exn.t
(** write a string to an Lwt_io.output_channel, which can then be read with
    read_string_lwt_io_channel
*)

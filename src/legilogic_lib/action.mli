open Lib

(* TODO: a variant Action2, ActionX or Interaction,
   where all types have an extra parameter: ('a, 'x) t, ('i, 'o, 'x) arr
   TODO: either become compatible with
   https://github.com/rgrinberg/ocaml-mtl/blob/master/lib/mtl.mli
*)

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
      but also to send each morphism [f : 'i -> 'o] into a morphism [map f : 'i t -> 'o t].
      Interpretation 2: applying [map f] applies the pure function [f] "under" the functorial
      wrapper [t] to each value in ['a] notionally wrapped in a ['a t].
  *)
  val map : ('i -> 'o) -> 'i t -> 'o t

  (** Haskell-style infix shorthand for [map]. *)
  val (<$>) : ('i -> 'o) -> 'i t -> 'o t
end

(** In the context of this application, there is no need to understand this
    abstraction, as its functionality is currently unused. XXX: Delete? *)
module type ApplicativeS = sig
  include FunctorS
  val pure : 'a -> 'a t (* Akin to [Monad.return] *)
  val ap : ('i -> 'o) t -> ('i t -> 'o t)
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
  val bind : 'i t -> ('i -> 'o t) -> 'o t
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

  (** the join operator:
      join x = x >>= identity
      x >>= f = join (map f x)
  *)
  val join : 'a t t -> 'a t

  (** Synonym for [bind]. Composes an ['i t] monad with an ['i -> 'o t] arrow *)
  val (>>=) : 'i t -> ('i -> 'o t) -> 'o t

  (** Lift a function on base types to a function on the monadic wrappers.
      NB: It's just [flip]ping the arguments of [map]. *)
  val (>>|) : 'i t -> ('i -> 'o) -> 'o t
end
(** See docstring for MonadS *)
module Monad (M : MonadBaseS) : MonadS with type 'a t = 'a M.t

module Identity : MonadS with type 'a t = 'a

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

  val (>>=|) : 'a t -> (unit -> 'a t) -> 'a t
  (** Try the second alternative if the first one fails *)
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
module type OrExnS = sig
  include ErrorMonadS with type error = exn

  val bork : ('a, unit, string, 'b t) format4 -> 'a
  (** Internal error *)

  val catching : ('i, 'o) arr -> ('i, 'o) arr
  (** Run an arrow that may raise exceptions, catch any exception and reify them monadically *)

  val catching_arr : ('i -> 'o) -> ('i, 'o) arr
  (** Run an OCaml function that may raise exceptions, which are caught and
      presented as monadic errors. Same as arr >> catching *)
end

(** See docstring for OrExnS.  *)
module OrExn : sig
  include OrExnS
    with type 'a t = 'a or_exn
     and type ('i, 'o) arr = 'i -> 'o or_exn
  val get : 'a t -> 'a (* should it instead be called [run]? *)
  val of_option : ('i -> 'o option) -> ('i, 'o) arr
  val of_or_string : ('i -> ('o, string) result) -> ('i, 'o) arr
end

module OrString : sig
  include ErrorMonadS
    with type 'a t = ('a, string) result
     and type ('i, 'o) arr = 'i -> ('o, string) result
     and type error = string
  val get : 'a t -> 'a

  val bork : ('a, unit, string, 'b t) format4 -> 'a
  (** Internal error *)

  val catching : ('i, 'o) arr -> ('i, 'o) arr
  (** Run an arrow that may raise exceptions, catch any exception and reify them monadically *)

  val catching_arr : ('i -> 'o) -> ('i, 'o) arr
  (** Run an OCaml function that may raise exceptions, which are caught and
      presented as monadic errors. Same as arr >> catching *)

  val of_or_exn : ('i, 'o) OrExn.arr -> ('i, 'o) arr
end

(** Signature for expression of composable computational effects which have a
    tracked state threaded through them (the [state] variable inherited from
    [ReaderMonadS], but note that no other [ReaderMonadS] functionality is used
    here.) See https://wiki.haskell.org/State_Monad for more details.

    The inputs/outputs to the computations, and their corresponding states, are
    represented internally as pairs (value, state). We use that representation
    here to describe method effects.

    TODO: add variants of the State monad and its derivatives that are parametric in the state type.
*)
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

(** Translation of Lwt monadic notation to ours, for consistency.
    The name "Lwter" is a a bad pun on "Lwt" and "Later"; it sounds like a kluge,
    which is exactly what it is:
    This is not an alternative to Lwt, just a thin layer above,
    slightly "better" suited for our purposes,
    yet slightly incompatible (see `Lwt.join` vs `Lwter.join`).
    It is not meant as an alternative to Lwt, or a rewrite, or anything:
    it is "an extension of a subset of Lwt", with which it shares a common core,
    but developed differently.
*)
module Lwter : sig
  include MonadS
    with type 'a t = 'a Lwt.t
     and type ('i, 'o) arr = 'i -> 'o Lwt.t
  val const_unit : (_, unit) arr
end

module type LwtExnS = sig
  include OrExnS

  val of_exn : ('i, 'o) OrExn.arr -> ('i, 'o) arr
  (** [of_exn a] given a OrExn arrow [a] that has no asynchronous effects, returns an arrow *)

  val of_lwt : ('i, 'o) Lwter.arr -> ('i, 'o) arr
  (** [of_lwt a] given a Lwt arrow [a] that doesn't fail returns a Lwt_exn arrow that
      returns its successful result. *)

  val catching_lwt : ('i, 'o) Lwter.arr -> ('i, 'o) arr
  (** Catch exceptions in an Lwt arrow. Same as of_lwt >> catching *)

  val retry : retry_window:float -> max_window:float -> max_retries:int option
    -> ('i, 'o) arr -> ('i, 'o) arr
    (** Retry the action up to max_tries time (or indefinitely, if None),
        with exponential backoff up to a maximum, then constant backoff.
        The initial window is retry_window, in seconds, doubled each time until it reaches max_window *)
end

(** Composing the Lwt and Exn monads:
    representing success or failure with an or_exn while using the Lwt monad for threading. *)
module Lwt_exn : sig
  include LwtExnS
    with type 'a t = 'a or_exn Lwt.t
     and type ('i, 'o) arr = ('i, 'o or_exn) Lwter.arr

  val run_lwt : ('i, 'o) arr -> 'i -> 'o Lwt.t
  (** [run_lwt a x] runs an Lwt_exn arrow [a] on an initial value [x] in an existing Lwt context,
      expressing failure with [or_exn] in a monadic way inside the evaluation of [mf], and
      raising an exception at the very end if the evaluation failed. *)

  val run : ('i, 'o) arr -> 'i -> 'o
  (** Run a [run_lwt a x] arrow [a] on an initial value [x] in a new Lwt context,
      using [Lwt_main.run] and raising an exception at the very end if the evaluation failed. *)

  val list_iter_s : ('a -> unit t) -> 'a list -> unit t
  (** Kick off a thread with unit result for each element of the list, in series *)

  val list_iter_p : ('a -> unit t) -> 'a list -> unit t
  (** Kick off a thread with unit result for each element of the list, in parallel *)

  val printf : ('a, unit, string, unit t) format4 -> 'a
  (** Print to stdout via [Lwt_io], then flush *)

  val eprintf : ('a, unit, string, unit t) format4 -> 'a
  (** Print to stderr via [Lwt_io], then flush *)
end

module type StatefulErrableActionS = sig
  include StateMonadS
  include OrExnS
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
      via the [OrExn] machinery.) *)
  val run : state ref -> ('i, 'o) arr -> 'i -> 'o
end

(** Synchronous action on a state monad with monadic error handling. *)
module type ActionS = sig
  type state
  include StatefulErrableActionS
    with type state := state
     and type 'a t = state -> 'a or_exn * state
     and type ('i, 'o) readonly = 'i -> state -> 'o

  (** [to_async] converts an Action into a corresponding AsyncAction that uses Lwt for threading. *)
  val to_async : ('i, 'o) arr -> 'i -> state -> ('o or_exn * state) Lwt.t
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
  include LwtExnS
    with type 'a t := 'a t
     and type ('i, 'o) arr := ('i, 'o) arr
     and type error := error

  (** run a computation on a global state ref in an existing Lwt context *)
  val run_lwt_exn : state ref -> ('i, 'o) arr -> 'i -> 'o or_exn Lwt.t
  val run_lwt : state ref -> ('i, 'o) arr -> 'i -> 'o Lwt.t

  (** From an [Action] to an [AsyncAction] *)
  val of_action : ('i -> state -> 'o or_exn * state) -> ('i, 'o) arr
  (** From a [Lwt_exn] arrow to an AsyncAction arrow *)
  val of_lwt_exn : ('i, 'o) Lwt_exn.arr -> ('i, 'o) arr
  (** From a [Lwt] arrow to an AsyncAction arrow *)
  val of_lwt : ('i, 'o) Lwter.arr -> ('i, 'o) arr

  (* val catching_lwt_exn : ('i, 'o) Lwt_exn.arr -> ('i, 'o) arr *)
end

(** See docstring for [AsyncActionS] *)
module AsyncAction (State : TypeS) : AsyncActionS
  with type state = State.t
   and type 'a t = State.t -> ('a or_exn * State.t) Lwt.t
   and type ('i, 'o) readonly = 'i -> State.t -> 'o

(** Given a mailbox and a way to make messages from a pair of input and output-co-promise, return
    an Lwt Kleisli arrow that goes from input to output *)
val simple_client : 'msg Lwt_mvar.t -> ('i * 'o Lwt.u -> 'msg) -> ('i, 'o) Lwter.arr

(** Given a mailbox for messages being a pair of input and output-co-promise,
    and given an AsyncAction arrow for some type of state, and an initial state of that type,
    return a background thread that sequentially processes those messages. *)
val simple_server : ('i * 'o Lwt.u) Lwt_mvar.t -> ('i, 'o, 'state) async_action -> 'state -> _ Lwt.t

(** Given an AsyncAction arrow for some type of state, return a client Lwt arrow that can
    call the asynchronous action in a sequentialized way, and an action that given an initial state
    for the server will create a background server thread to actually process the client requests. *)
val simple_client_make_server : ('i, 'o, 'state) async_action ->
  ('i, 'o) Lwter.arr * ('state -> _ Lwt.t)

(** Given a mailbox for messages being a pair of input and output-co-promise,
    and given an Lwt arrow return a background thread that sequentially processes those messages. *)
val stateless_server : ('i * 'o Lwt.u) Lwt_mvar.t -> ('i, 'o) Lwter.arr -> _ Lwt.t

(** Given an AsyncAction arrow and an initial state,
    return a client Lwt arrow that provides sequentialized access to the action arrow *)
val sequentialize : ('i, 'o, 'state) async_action -> 'state -> ('i, 'o) Lwter.arr

(** Given an AsyncAction arrow and an initial state,
    return a client Lwt arrow that provides sequentialized access to the action arrow *)
val stateless_sequentialize : ('i, 'o) Lwter.arr -> ('i, 'o) Lwter.arr

(*
   (** Given a mailbox for messages being a pair of input and output-co-promise,
   and given an Lwt arrow return a background thread that processes those messages in parallel.
   In a given process, you might as well do without a mailbox, but if you have to have a mailbox anyway...
 *)

   val stateless_parallel_server : ('i * 'o Lwt.u) Lwt_mvar.t -> ('i, 'o) Lwter.arr -> _ Lwt.t

   (** Given a Lwt arrow, return a client Lwt arrow that does the same thing,
   but going through the bottleneck of a mailbox.
   In a given process, you might as well do without the entire mailbox thing! *)
   val stateless_parallelize : ('i, 'o) Lwter.arr -> ('i, 'o) Lwter.arr

*)

(* reading, writing strings from Lwt_io channels *)

val read_string_from_lwt_io_channel : ?count:int -> Lwt_io.input_channel -> string Lwt_exn.t
(** read a string from an Lwt_io.input_channel, in chunks of size count;
    the string must have been written by write_string_lwt_io_channel
*)

val write_string_to_lwt_io_channel : Lwt_io.output_channel -> string -> unit Lwt_exn.t
(** write a string to an Lwt_io.output_channel, then flush the channel; the string
    can then be read with read_string_lwt_io_channel
*)

module AsyncStream : sig
  (* NB: the tail is *eagerly* scheduled to run by the time we get to the stream.
     Maybe we should change the type to unit -> 'a t to allow for lazy streaming? *)
  type 'a stream = | Nil | Cons of { hd: 'a; tl: 'a t }
  and 'a t = 'a stream Lwt.t
  (** [split stream n] returns a list of the first [n] values of the stream, and
      a new stream with the rest. *)
  val split : int -> 'a t -> ('a list * 'a t) Lwt.t
  val nil : unit -> 'a t
  val cons : 'a -> 'a t -> 'a t
end
(** Promise of asynchronous stream of events. If an EventStream.t is
    [Lwt.bind]-bound to a function

    [f { current_event; subsequent_event_stream} = ...]

    then [current_event] is the first event in the stream, and
    [subsequent_event_stream] is a promise for the next event in the stream. *)

val with_connection : Unix.sockaddr -> (Lwt_io.input_channel * Lwt_io.output_channel, 'a) Lwt_exn.arr -> 'a Lwt_exn.t
(** open a connection and run the function in it, closing input and output channels at the end.
    Return an Error Unix.Unix_error if the socket failed to be opened. *)

(** Simple actor
    You [make] an actor by providing, beside an initial state of type ['state] (at the end),
    a [save] action that will persist the state as part of some suitable transaction,
    either synchronously

    Then, you can call the [modify] function to modify the actor
    by passing a [('state, 'state) Lwter.arr] arrow.
    Or you can call [action] and pass it an [('i, 'o, 'state) async_action] arrow
    as well as an ['i] input, and it will return an ['o] output after side-effecting
    but before saving the state.

    You can also [peek] at the last known saved state of the actor, or run a [peek_action]
    that operates on the last known saved state and discards any modifications to it
    (usually you would want to only run read-only actions; TODO: implement async_reader).
    Because the actions are asynchronous, it is possible (though unlikely) that
    the peeking will see a slightly earlier or later version of the state than is current;
    therefore, you should only make decisions based on constant or monotonic aspects of the state,
    or on accumulated statistics that do not have to be overly precise;
    if you want synchronous access to the state, use a regular action.

    See [Persisting.SynchronousPersistentAction] and [Persisting.AsynchronousPersistentAction]
    for more specific uses of this pattern.

    If you use [SimpleActor] directly instead of through these persistent elaborations,
    note that the actor won't call the [save] function on the initial state.
    If you need the initial state you use to create the actor to persist because it's not
    an empty default state given the context, you need to do that by yourself before you create
    the actor, or at least before it's first used.

    TODO: define and use a Reader, Lwt, Exn monad instead of a State, Lwt, Exn monad for the peek_action.
    TODO: learn monad transformers and the proper order of stacking of those monad transformers.
    TODO: use an existing OCaml monad/monad transformer library.
*)
module type SimpleActorS = sig
  type 'state t
  val modify : 'state t -> ('state, 'state) Lwter.arr -> unit Lwt.t
  val action : 'state t -> ('i, 'o, 'state) async_action -> ('i, 'o) Lwter.arr
  val peek : 'state t -> 'state
  val peek_action : 'state t -> ('i, 'o, 'state) async_action -> ('i, 'o) Lwter.arr
end

module SimpleActor : sig
  include SimpleActorS
  val make : ?save:('state, unit) Lwter.arr -> 'state -> 'state t
end

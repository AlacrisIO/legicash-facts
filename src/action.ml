open Lib

type 'output or_exn = ('output, exn) result

type ('input, 'output, 'state) action = 'input -> 'state -> 'output or_exn * 'state

type ('input, 'output, 'state) async_action = 'input -> 'state -> ('output or_exn * 'state) Lwt.t

exception Assertion_failed of string

module type StatefulErrableActionS = sig
  type state
  include MonadS
  val with_state : ('a * state, 'b) arr -> ('a, 'b) arr
  val return_state : state -> ('a, 'a) arr
  val map_state : (state -> state) -> ('a, 'a) arr
  val fail : exn -> _ t
  type ('i, 'o) pure = 'i -> state -> 'o
  val of_pure : ('i, 'o) pure -> ('i, 'o) arr
  val assert_: (unit -> string) -> ('i, bool) pure -> ('i, 'i) arr
  val run : state ref -> 'a -> ('a, 'b) arr -> 'b
end

module type ActionS = sig
  type state
  include StatefulErrableActionS
    with type state := state
     and type 'a t = state -> 'a or_exn * state
end

module Action (State : TypeS) : ActionS with type state = State.t = struct
  type state = State.t
  include Monad(struct
      type 'a t = state -> 'a or_exn * state
      let return x s = Ok x, s
      let bind m f s = (m s) |> function
      | Ok a, s' -> f a s'
      | Error e, s' -> Error e, s'
    end)
  let with_state f x s = f (x, s) s
  let return_state s x _s' = Ok x, s
  let map_state f x s = Ok x, f s
  let fail failure s = Error failure, s
  type ('i, 'o) pure = 'i -> state -> 'o
  let of_pure p x s = return (p x s) s
  let assert_ where test value state =
    (if test value state then Ok value else Error (Assertion_failed (where ()))), state
  let run r x mf = match (mf x !r) with (roe, s) -> r := s ; ResultOrExn.get roe
end

module type AsyncActionS = sig
  type state
  include StatefulErrableActionS
    with type state := state
     and type 'a t = state -> ('a or_exn * state) Lwt.t
  val run_lwt : state ref -> 'a -> ('a, 'b) arr -> 'b Lwt.t
  val of_action : ('a -> state -> 'b or_exn * state) -> ('a, 'b) arr
end
module AsyncAction (State : TypeS) : AsyncActionS with type state = State.t = struct
  type state = State.t
  include Monad(struct
      type 'a t = state -> ('a or_exn * state) Lwt.t
      let return x s = Lwt.return (Ok x, s)
      let bind m f s = Lwt.bind (m s)
                         (function
                           | Ok a, s' -> f a s'
                           | Error e, s' -> Lwt.return (Error e, s'))
    end)
  module Action = Action(State)
  let with_state f x s = f (x, s) s
  let return_state s x _s' = (Ok x, s) |> Lwt.return
  let map_state f x s = (Ok x, f s) |> Lwt.return
  let fail failure s = Action.fail failure s |> Lwt.return
  let assert_ where pure_action value state =
    Action.assert_ where pure_action value state |> Lwt.return
  let run_lwt r x mf =
    Lwt.bind (mf x !r) (fun (roe, s) -> r := s ; ResultOrExn.get roe |> Lwt.return)
  let run r x mf = (Lwt_main.run (run_lwt r x mf))
  type ('i, 'o) pure = 'i -> state -> 'o
  let of_pure p x s = return (p x s) s
  let of_action a x s = a x s |> Lwt.return
end

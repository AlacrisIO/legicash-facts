open Lib

type +'output or_exn = ('output, exn) result

type (-'input, +'output, 'state) action = 'input -> 'state -> 'output or_exn * 'state

type (-'input, +'output, 'state) async_action = 'input -> 'state -> ('output or_exn * 'state) Lwt.t

exception Assertion_failed of string

module type StatefulErrableActionS = sig
  include StateMonadS
  include ErrorMonadS
    with type error = exn
     and type 'a t := 'a t
     and type ('i, 'o) arr := ('i, 'o) arr
  val assert_: (unit -> string) -> ('i, bool) readonly -> ('i, 'i) arr
  val run : state ref -> ('a, 'b) arr -> 'a -> 'b
end

module type ActionS = sig
  type state
  include StatefulErrableActionS
    with type state := state
     and type error = exn
     and type 'a t = state -> 'a or_exn * state
     and type ('i, 'o) readonly = 'i -> state -> 'o
  val to_async : ('i, 'o) arr -> 'i -> state -> ('o or_exn * state) Lwt.t
end

module Action (State : TypeS) = struct
  type state = State.t
  type error = exn
  include Monad(struct
      type +'a t = state -> 'a or_exn * state
      let return x s = Ok x, s
      let bind m f s = (m s) |> function
      | Ok a, s' -> f a s'
      | Error e, s' -> Error e, s'
    end)
  let pair_with_state x s = (Ok (x, s), s)
  let state s = Ok s, s
  let get_state _ = state
  let put_state s _ = Ok (), s
  let map_state f x s = Ok x, f s
  let fail failure s = Error failure, s
  type ('i, 'o) readonly = 'i -> state -> 'o
  let of_readonly p x s = return (p x s) s
  let assert_ where test value state =
    (if test value state then Ok value else Error (Assertion_failed (where ()))), state
  let run r mf x = match (mf x !r) with (roe, s) -> r := s ; ResultOrExn.get roe
  let to_async a x s = a x s |> Lwt.return
end

module type AsyncActionS = sig
  type state
  include StatefulErrableActionS
    with type state := state
     and type error = exn
     and type 'a t = state -> ('a or_exn * state) Lwt.t
     and type ('i, 'o) readonly = 'i -> state -> 'o
  val run_lwt_exn : state ref -> ('a, 'b) arr -> 'a -> 'b or_exn Lwt.t
  val run_lwt : state ref -> ('a, 'b) arr -> 'a -> 'b Lwt.t
  val of_action : ('a -> state -> 'b or_exn * state) -> ('a, 'b) arr
end
module AsyncAction (State : TypeS) = struct
  type state = State.t
  type error = exn
  include Monad(struct
      type +'a t = state -> ('a or_exn * state) Lwt.t
      let return x s = Lwt.return (Ok x, s)
      let bind m f s = Lwt.bind (m s)
                         (function
                           | Ok a, s' -> f a s'
                           | Error e, s' -> Lwt.return (Error e, s'))
    end)
  module Action = Action(State)
  let pair_with_state x s = (Ok (x, s), s) |> Lwt.return
  let state s = (Ok s, s) |> Lwt.return
  let get_state _ = state
  let put_state s _ = (Ok (), s) |> Lwt.return
  let map_state f x s = (Ok x, f s) |> Lwt.return
  let fail failure s = Action.fail failure s |> Lwt.return
  let assert_ where pure_action value state =
    Action.assert_ where pure_action value state |> Lwt.return
  let run_lwt_exn r mf x = (* TODO: switch run_lwt and run_lwt_exn ? *)
    Lwt.bind (mf x !r) (fun (roe, s) -> r := s ; Lwt.return roe)
  let run_lwt r mf x =
    Lwt.bind (mf x !r) (fun (roe, s) -> r := s ; ResultOrExn.get roe |> Lwt.return)
  let run r mf x = run_lwt r mf x |> Lwt_main.run
  type ('i, 'o) readonly = 'i -> state -> 'o
  let of_readonly p x s = return (p x s) s
  let of_action a x s = a x s |> Lwt.return
end

module Lwt_monad = struct
  include Monad(struct
      type +'a t = 'a Lwt.t
      let return = Lwt.return
      let bind = Lwt.bind
    end)
  let map = Lwt.map
end

module Lwt_exn = struct
  type error = exn
  let fail e = Lwt.return (Error e)
  include Monad(struct
      type +'a t = 'a or_exn Lwt.t
      let return x = Lwt.return (Ok x)
      let bind m a = Lwt.bind m (function Ok x -> a x | Error e -> fail e)
    end)
  let run_lwt mf x =
    Lwt.bind (mf x) (ResultOrExn.get >> Lwt.return)
  let run mf x = run_lwt mf x |> Lwt_main.run
end

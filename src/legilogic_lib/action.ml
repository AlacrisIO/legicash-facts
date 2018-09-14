open Lib

type +'output or_exn = ('output, exn) result

type (-'input, +'output, 'state) action = 'input -> 'state -> 'output * 'state

type (-'input, +'output, 'state) async_action = 'input -> 'state -> ('output * 'state) Lwt.t

type (-'input, +'output, 'state) exn_action = ('input, 'output or_exn, 'state) action

type (-'input, +'output, 'state) async_exn_action = ('input, 'output or_exn, 'state) async_action

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
  val (>>=) : 'a t -> ('a, 'b) arr -> 'b t
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
end
module Monad (M : MonadBaseS) = struct
  include M
  let (>>=) = bind
  let map f m = bind m (f >> return)
  let (<$>) = map
  let (>>|) m f = map f m
  let pure = return
  type ('i, 'o) arr = 'i -> 'o t
  let returnA = return
  let arr f x = x |> f |> return
  let (>>>) f g x = x |> f >>= g
  let const x = fun _ -> return x
  let ap mf m = mf >>= fun f -> m >>= arr f
  let rec forever arr acc = (arr acc) >>= (forever arr)
end
module type ErrorMonadS = sig
  type error
  include MonadS
  val fail : error -> _ t
  val trying : ('i,'o) arr -> ('i, ('o, error) result) arr
  val handling : (error,'a) arr -> (('a, error) result, 'a) arr
end

module ErrorMonad (Error: TypeS) = struct
  type error = Error.t
  module B = struct
    type +'a t = ('a, error) result
    let return x = Ok x
    let bind m fm = match m with
      | Ok x -> fm x
      | Error e -> Error e
  end
  include Monad(B)
  let fail e = Error e
  (* Note that [trying] returns either an [Ok (Ok _)], or [Ok (Error _)]. It's
     expected that this is passed to [handling] via a [bind], which unwraps the
     outer [Ok]. *)
  let trying a i = return (a i)
  let handling a = function
    | Ok x -> return x
    | Error e -> a e
end

module type ExnMonadS = sig
  include ErrorMonadS with type error = exn
  val bork : ('a, unit, string, 'b t) format4 -> 'a
  val catching : ('i -> 'o) -> ('i, 'o) arr
end

module ExnMonad = struct
  include ErrorMonad(struct type t = exn end)
  let bork fmt = Printf.ksprintf (fun x -> fail (Internal_error x)) fmt
  let catching a i = try Ok (a i) with e -> Error e
end

module type StateMonadS = sig
  type state
  include MonadBaseS
  include MonadS with type 'a t := 'a t
  val state : state t
  val map_state : (state -> state) -> ('a, 'a) arr
  type ('i, 'o) readonly
  val of_readonly : ('i, 'o) readonly -> ('i, 'o) arr
end
module StateMonad (State: TypeS) = struct
  module Base = struct
    type state = State.t
    type +'a t = state -> ('a * state)
    let return x s = (x, s)
    let bind m fm s = m s |> uncurry fm
    let state s = (s, s)
    type ('i, 'o) readonly = 'i -> state -> 'o
    let of_readonly r x s = (r x s, s)
  end
  include Base
  include (Monad(Base) : MonadS with type 'a t := 'a t)
  let map_state f x s = (x, f s)
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
  let bork fmt = Printf.ksprintf (fun x -> fail (Internal_error x)) fmt
  let catching f x = try f x |> return with e -> fail e
  let trying a x = Lwt.bind (a x) return
  let handling a = function
    | Ok x -> return x
    | Error e -> a e
  let of_exn a x = a x |> Lwt.return
  let of_lwt a x = Lwt.bind (a x) return
  let list_iter_s f = of_lwt (Lwt_list.iter_s (run_lwt f))
  let list_iter_p f = of_lwt (Lwt_list.iter_p (run_lwt f))
  let printf fmt =
    let (>>=) = Lwt.bind in
    Printf.ksprintf (fun x -> Lwt_io.(printf "%s" x >>= fun () -> flush stdout >>= return)) fmt
  let eprintf fmt =
    let (>>=) = Lwt.bind in
    Printf.ksprintf (fun x -> Lwt_io.(eprintf "%s" x >>= fun () -> flush stderr >>= return)) fmt
end

module type StatefulErrableActionS = sig
  include StateMonadS
  include ExnMonadS
    with type 'a t := 'a t
     and type ('i, 'o) arr := ('i, 'o) arr
  val assert_: (unit -> string) -> ('i, bool) readonly -> ('i, 'i) arr
  val run : state ref -> ('a, 'b) arr -> 'a -> 'b
end

module type ActionS = sig
  type state
  include StatefulErrableActionS
    with type state := state
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
  let state s = Ok s, s
  let map_state f x s = Ok x, f s
  let fail failure s = Error failure, s
  type ('i, 'o) readonly = 'i -> state -> 'o
  let of_readonly p x s = return (p x s) s
  let assert_ where test value state =
    (if test value state then Ok value else Error (Assertion_failed (where ()))), state
  let run r mf x = match (mf x !r) with (roe, s) -> r := s ; ResultOrExn.get roe
  let to_async a x s = a x s |> Lwt.return
  let bork fmt = Printf.ksprintf (fun x -> fail (Internal_error x)) fmt
  let catching f x s = try return (f x) s with e -> fail e s
  let trying a x s = a x s |> fun (r, s) -> Ok r, s
  let handling a = function
    | Ok x -> return x
    | Error e -> a e
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
  val of_lwt_exn : ('a, 'b) Lwt_exn.arr -> ('a, 'b) arr
  val of_lwt : ('a, 'b) Lwt_monad.arr -> ('a, 'b) arr
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
  let state s = (Ok s, s) |> Lwt.return
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
  let of_lwt_exn a x s = Lwt.bind (a x) (fun r -> Lwt.return (r, s))
  let of_lwt a x s = Lwt.bind (a x) (fun r -> return r s)
  let bork fmt = Printf.ksprintf (fun x -> fail (Internal_error x)) fmt
  let catching f x s = Lwt.return ((try Ok (f x) with e -> Error e), s)
  let trying a x s = Lwt.bind (a x s) (fun (r, s) -> Lwt.return (Ok r, s))
  let handling a = function
    | Ok x -> return x
    | Error e -> a e
end

(* Simple client *)
let simple_client mailbox make_message =
  fun request ->
    let open Lwt in
    let (promise, resolver) = task () in
    make_message (request, resolver)
    |> Lwt_mvar.put mailbox
    >>= fun () -> promise

let simple_server mailbox processor =
  let open Lwt_monad in
  forever
    (fun state ->
       Lwt_mvar.take mailbox
       >>= fun (input, continuation) ->
       processor input state
       >>= fun (output, new_state) ->
       Lwt.wakeup_later continuation output;
       Lwt.return new_state)

let simple_client_make_server processor =
  let mailbox = Lwt_mvar.create_empty () in
  (simple_client mailbox identity,
   simple_server mailbox processor)

let sequentialize processor state =
  let (client, make_server) = simple_client_make_server processor in
  Lwt.async (fun () -> make_server state);
  client

let stateless_server mailbox processor =
  let open Lwt_monad in
  () |>
  forever
    (fun () ->
       Lwt_mvar.take mailbox
       >>= fun (input, continuation) ->
       processor input
       >>= fun (output) ->
       Lwt.wakeup_later continuation output;
       Lwt.return_unit)

let stateless_sequentialize processor =
  let mailbox = Lwt_mvar.create_empty () in
  Lwt.async (fun () -> stateless_server mailbox processor);
  simple_client mailbox identity

(*
   let stateless_parallel_server mailbox processor =
   let open Lwt_monad in
   () |>
   forever
   (fun () ->
   Lwt_mvar.take mailbox
   >>= fun (input, continuation) ->
   Lwt.async (fun () ->
   processor input
   >>= fun (output) ->
   Lwt.wakeup_later continuation output;
   Lwt.return_unit);
   Lwt.return_unit)

   let stateless_parallelize processor =
   let mailbox = Lwt_mvar.create_empty () in
   Lwt.async (fun () -> stateless_parallel_server mailbox processor);
   simple_client mailbox identity
*)

(* reading, writing strings from Lwt_io channels *)

let read_string_from_lwt_io_channel ?(count=64) in_channel =
  let open Lwt_exn in
  let open Lwt_io in
  of_lwt read_int16 in_channel
  >>= fun len ->
  let rec loop sofar accum =
    if sofar >= len then
      String.concat "" (List.rev accum) |> return
    else
      of_lwt (read ~count) in_channel
      >>= fun s -> loop (sofar + String.length s) (s::accum)
  in
  loop 0 []

let write_string_to_lwt_io_channel out_channel s =
  let open Lwt_exn in
  let open Lwt_io in
  let len = String.length s in
  of_lwt (write_int16 out_channel) len
  >>= fun () -> Lwt_stream.of_string s |> of_lwt (write_chars out_channel)
  >>= fun () -> of_lwt flush out_channel (* flushing is critical *)

module Test = struct
  module Error_string_monad = ErrorMonad(struct
      type t = string
      type error = string
    end)
  let%test "Lwt_exn error handling works as expected" =
    Error_string_monad.(
      match trying (fun _ -> Error "error") 0 >>= handling (fun x -> fail x) with
      | Error _ -> true
      | Ok _ -> false)
end

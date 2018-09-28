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
  val join : 'a t t -> 'a t
  val (>>=) : 'a t -> ('a, 'b) arr -> 'b t
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
end
module Monad (M : MonadBaseS) = struct
  include M
  let join x = bind x identity
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

module Identity = struct
  module I = struct
    type 'a t = 'a
    let return = identity
    let bind = (|>)
  end
  include Monad(I)
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

module type OrExnS = sig
  include ErrorMonadS with type error = exn
  val bork : ('a, unit, string, 'b t) format4 -> 'a
  val catching : ('i, 'o) arr -> ('i, 'o) arr
  val catching_arr : ('i -> 'o) -> ('i, 'o) arr
end

module OrExn = struct
  include ErrorMonad(struct type t = exn end)
  let bork fmt = Printf.ksprintf (fun x -> fail (Internal_error x)) fmt
  let catching a i = try a i with e -> Error e
  let catching_arr a i = try Ok (a i) with e -> Error e
  let get = function
    | Ok x -> x
    | Error e -> raise e
  let of_option f x =
    match (f x) with
    | Some r -> Ok r
    | None -> Error Not_found
  let of_or_string f x =
    match f x with
    | Ok x -> Ok x
    | Error e -> Error (Internal_error e)
end

module OrString = struct
  include ErrorMonad(struct type t = string end)
  let bork fmt = Printf.ksprintf fail fmt
  let catching a i = try a i with e -> Error (Printexc.to_string e)
  let catching_arr a = catching (arr a)
  let get = function
    | Ok x -> x
    | Error e -> raise (Internal_error e)
  let of_or_exn f x =
    match f x with
    | Ok x -> Ok x
    | Error e -> Error (Printexc.to_string e)
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

module Lwter = struct
  include (Monad(struct
             type +'a t = 'a Lwt.t
             let return = Lwt.return
             let bind = Lwt.bind
           end))
  let const_unit _ = Lwt.return_unit
end

module type LwtExnS = sig
  include OrExnS
  val of_exn : ('a, 'b) OrExn.arr -> ('a, 'b) arr
  val of_lwt : ('a -> 'b Lwt.t) -> ('a, 'b) arr
  val catching_lwt : ('i, 'o) Lwter.arr -> ('i, 'o) arr
  val retry : retry_window:float -> max_window:float -> max_retries:int option
    -> ('i, 'o) arr -> ('i, 'o) arr
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
    Lwt.bind (mf x) (OrExn.get >> Lwt.return)
  let run mf x = run_lwt mf x |> Lwt_main.run
  let bork fmt = Printf.ksprintf (fun x -> fail (Internal_error x)) fmt
  let catching f x = try f x with e -> fail e
  let catching_arr f = arr f |> catching
  let catching_lwt f x = try Lwt.bind (f x) return with e -> fail e
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
  let rec retry ~retry_window ~max_window ~max_retries action input =
    let open Lwt in
    action input
    >>= function
    | Ok result -> Lwt.return (Ok result)
    | Error e ->
      if (match max_retries with None -> true | Some n -> n > 1) then
        let retry_window = min retry_window max_window in
        Lwt_unix.sleep (Random.float retry_window)
        >>= fun () ->
        retry ~retry_window:(retry_window *. 2.0) ~max_window
          ~max_retries:(Option.map ((+) 1) max_retries)
          action input
      else
        Lwt.return (Error e)
end

module type StatefulErrableActionS = sig
  include StateMonadS
  include OrExnS
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
  let run r mf x = match (mf x !r) with (roe, s) -> r := s ; OrExn.get roe
  let to_async a x s = a x s |> Lwt.return
  let bork fmt = Printf.ksprintf (fun x -> fail (Internal_error x)) fmt
  let catching f x s = try f x s with e -> fail e s
  let catching_arr f = arr f |> catching
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
  include LwtExnS
    with type 'a t := 'a t
     and type ('i, 'o) arr := ('i, 'o) arr
     and type error := error
  val run_lwt_exn : state ref -> ('i, 'o) arr -> 'i -> 'o or_exn Lwt.t
  val run_lwt : state ref -> ('i, 'o) arr -> 'i -> 'o Lwt.t
  val of_action : ('i -> state -> 'o or_exn * state) -> ('i, 'o) arr
  val of_lwt_exn : ('i, 'o) Lwt_exn.arr -> ('i, 'o) arr
  val of_lwt : ('i, 'o) Lwter.arr -> ('i, 'o) arr
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
    Lwt.bind (mf x !r) (fun (roe, s) -> r := s ; OrExn.get roe |> Lwt.return)
  let run r mf x = run_lwt r mf x |> Lwt_main.run
  type ('i, 'o) readonly = 'i -> state -> 'o
  let of_readonly p x s = return (p x s) s
  let of_action a x s = a x s |> Lwt.return
  let of_lwt_exn a x s = Lwt.bind (a x) (fun r -> Lwt.return (r, s))
  let of_lwt a x s = Lwt.bind (a x) (fun r -> return r s)
  let of_exn a x s = a x |> function Ok r -> return r s | Error e -> fail e s
  let bork fmt = Printf.ksprintf (fun x -> fail (Internal_error x)) fmt
  let catching f x s = try f x s with e -> fail e s
  let catching_arr f = arr f |> catching
  let catching_lwt f = of_lwt f |> catching
  let trying a x s = Lwt.bind (a x s) (fun (r, s) -> Lwt.return (Ok r, s))
  let handling a = function
    | Ok x -> return x
    | Error e -> a e
  let rec retry ~retry_window ~max_window ~max_retries action input s =
    let open Lwt in
    action input s
    >>= function
    | (Ok result, new_state) -> return (Ok result, new_state)
    | (Error e, new_state) ->
      if (match max_retries with None -> true | Some n -> n > 1) then
        let retry_window = min retry_window max_window in
        Lwt_unix.sleep (Random.float retry_window)
        >>= fun () ->
        retry ~retry_window:(retry_window *. 2.0) ~max_window
          ~max_retries:(Option.map ((+) 1) max_retries)
          action input new_state
      else
        Lwt.return (Error e, new_state)
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
  let open Lwter in
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
  let open Lwter in
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
   let open Lwt in
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
  catching_lwt read_int16 in_channel
  >>= fun len ->
  let rec loop sofar accum =
    if sofar >= len then
      String.concat "" (List.rev accum) |> return
    else
      catching_lwt (read ~count) in_channel
      >>= fun s -> loop (sofar + String.length s) (s::accum)
  in
  loop 0 []

let write_string_to_lwt_io_channel out_channel s =
  let open Lwt_exn in
  let open Lwt_io in
  let len = String.length s in (* TODO: handle the case of length overflow *)
  catching_lwt (write_int16 out_channel) len
  >>= fun () -> Lwt_stream.of_string s |> catching_lwt (write_chars out_channel)
  >>= fun () -> catching_lwt flush out_channel (* flushing is critical *)

(* TODO: some kind of try ... finally to always close the channels *)
let with_connection sockaddr f =
  let open Lwt_exn in
  catching_lwt Lwt_io.open_connection sockaddr
  >>= fun (in_channel, out_channel) ->
  let open Lwt in
  f (in_channel, out_channel)
  >>= fun r ->
  (try Lwt_io.close in_channel with _ -> return_unit) (* TODO: Log something on error? *)
  >>= fun () ->
  (try Lwt_io.close out_channel with _ -> return_unit)
  >>= fun () ->
  Lwt.return r

module AsyncStream = struct
  type 'a t = 'a stream Lwt.t
  and 'a stream = | Nil | Cons of { hd: 'a; tl: 'a t }
  let split (n : int) (s : 'a t) : ('a list * 'a t) Lwt.t =
    let rec f acc s = function
      | 0 -> Lwt.return (List.rev acc, s)
      | n when n > 0 ->
        Lwt.(s >>= function
        | Nil -> bork "Took too many entries from this stream!"
        | Cons { hd; tl } -> f (hd :: acc) tl (pred n))
      | n -> bork "Negative value to [iter]: %i" n in
    f [] s n
  (* TODO: [Monad(something)]?? What should [bind] do? Should it be [map]? *)
  let nil () = Lwt.return Nil
  let cons hd tl = Lwt.return @@ Cons {hd ; tl}
end

module type SimpleActorS = sig
  type 'state t
  val modify : 'state t -> ('state, 'state) Lwter.arr -> unit Lwt.t
  val action : 'state t -> ('i, 'o, 'state) async_action -> ('i, 'o) Lwter.arr
  val peek : 'state t -> 'state
  val peek_action : 'state t -> ('i, 'o, 'state) async_action -> ('i, 'o) Lwter.arr
end

module SimpleActor = struct
  open Lwter
  type 'state t =
    { state_ref : 'state ref
    ; save : ('state, unit) Lwter.arr
    ; mailbox : ('state, 'state) Lwter.arr Lwt_mvar.t }
  let make ?(save=const_unit) initial_state =
    let state_ref = ref initial_state in
    let mailbox = Lwt_mvar.create_empty () in
    Lwt.async (fun () ->
      forever
        (fun state ->
           Lwt_mvar.take mailbox
           >>= (|>) state)
        initial_state);
    { state_ref ; save ; mailbox }
  let peek actor = !(actor.state_ref)
  let poke actor ?respond ~get_new_state transform =
    Lwt_mvar.put actor.mailbox
      (transform
       >>> (fun output ->
         let new_state = get_new_state output in
         (match respond with
          | None -> ()
          | Some f -> Lwt.async (fun () -> f output));
         actor.save new_state
         >>= fun () ->
         actor.state_ref := new_state;
         return new_state))
  let modify actor f =
    poke actor ~get_new_state:identity f
  let action actor f i =
    let (promise, copromise) = Lwt.task () in
    poke actor
      ~respond:(fun (o, _) -> Lwt.wakeup_later copromise o; Lwt.return_unit)
      ~get_new_state:snd
      (f i)
    >>= fun () -> promise
  let peek_action actor f i =
    let state = peek actor in
    f i state >>= (fst >> return)
end

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

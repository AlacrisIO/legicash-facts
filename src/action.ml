type 'output legi_result = ('output, exn) result

type ('input, 'output, 'state) action = 'state * 'input -> 'state * 'output legi_result

type ('input, 'output, 'state) async_action = 'state * 'input -> ('state * 'output legi_result) Lwt.t

(** run the action, with side-effects and all *)
let effect_action action state_ref x =
  match action (!state_ref, x) with
  | s, Ok y ->
    state_ref := s ;
    y
  | s, Error e ->
    state_ref := s ;
    raise e

let no_action (s, a) = (s, Ok a)

let fail_action failure (s, _) = (s, Error failure)

(** compose two actions *)
let compose_actions c_of_b b_of_a (s, a) =
  match b_of_a (s, a) with
  | t, Error e -> (t, Error e)
  | t, Ok b -> c_of_b (t, b)

(** compose two async actions *)
let compose_async_actions c_of_b b_of_a (s, a) =
  let open Lwt in
  b_of_a (s, a)
  >>= fun result ->
  match result with
    t, Error e -> return (t, Error e)
  | t, Ok b -> c_of_b (t, b)

(** compose a list of actions *)
let compose_action_list action_list (s, a) =
  let rec loop action_list (s, a) =
    match action_list with
    | [] -> (s, Ok a)
    | action :: more_actions ->
      match action (s, a) with t, Ok b -> loop more_actions (t, b) | (_, Error _) as x -> x
  in
  loop action_list (s, a)

let do_action (state, value) action = action (state, value)

let ( ^|> ) = do_action

(* Same as |> !!!!*)

let action_seq action1 action2 = compose_actions action2 action1

let ( ^>> ) = action_seq

let async_action_seq action1 action2 = compose_async_actions action2 action1

let ( ^>>+ ) = async_action_seq

exception Assertion_failed of string

let action_assert where pure_action (state, value) =
  if pure_action (state, value) then (state, Ok value) else (state, Error (Assertion_failed where))

type ('input, 'output, 'state) pure_action = 'state * 'input -> 'output

type ('input, 'output, 'state) pure_async_action = 'state * 'input -> 'output Lwt.t

let action_of_pure_action f (state, value) = (state, Ok (f (state, value)))

let async_action_of_pure_action f (state, value) = Lwt.return (state, Ok (f (state, value)))

(** compose two pure actions *)
let compose_pure_actions c_of_b b_of_a (s, a) = c_of_b (s, b_of_a (s, a))

let pure_action_seq b_of_a c_of_b (s, a) = compose_pure_actions c_of_b b_of_a (s, a)

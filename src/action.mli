(** 'output or exception *)
type 'output legi_result = ('output, exn) result

(** function from 'input to 'output that acts on a 'state and may return an exception *)
type ('input, 'output, 'state) action = 'state * 'input -> 'state * 'output legi_result

val effect_action : ('input, 'output, 'state) action -> 'state ref -> 'input -> 'output
(** run the action, with side-effects and all *)

val no_action : ('passthrough, 'passthrough, 'state) action

val fail_action : exn -> ('input, 'bottom, 'state) action

val do_action : 'state * 'input -> ('input, 'output, 'state) action -> 'state * 'output legi_result
(** apply an action, left to right *)

val ( ^|> ) : 'state * 'input -> ('input, 'output, 'state) action -> 'state * 'output legi_result

val compose_actions :
  ('intermediate, 'output, 'state) action
  -> ('input, 'intermediate, 'state) action
  -> ('input, 'output, 'state) action
(** compose two actions *)

val action_seq :
  ('input, 'intermediate, 'state) action
  -> ('intermediate, 'output, 'state) action
  -> ('input, 'output, 'state) action
(** compose two actions, left to right *)

val ( ^>> ) :
  ('input, 'intermediate, 'state) action
  -> ('intermediate, 'output, 'state) action
  -> ('input, 'output, 'state) action

val compose_action_list : ('a, 'a, 'state) action list -> ('a, 'a, 'state) action
(** compose a list of actions (NB: monomorphic in type being passed around *)

(** a pure action can read the global state, but not modify it, and not fail *)
type ('input, 'output, 'state) pure_action = 'state * 'input -> 'output

val action_of_pure_action :
  ('input, 'output, 'state) pure_action -> ('input, 'output, 'state) action

val compose_pure_actions :
  ('intermediate, 'output, 'state) pure_action
  -> ('input, 'intermediate, 'state) pure_action
  -> ('input, 'output, 'state) pure_action

val pure_action_seq :
  ('input, 'intermediate, 'state) pure_action
  -> ('intermediate, 'output, 'state) pure_action
  -> ('input, 'output, 'state) pure_action

exception Assertion_failed of string

val action_assert : string -> ('a, bool, 'state) pure_action -> ('a, 'a, 'state) action
(** given a pure_action returning a bool, make an action that asserts the bool is true
    the string could be the file location as given by Pervasives.__LOC__, or other
    string identifying the point of failure, which is provided to Assertion_failed *)

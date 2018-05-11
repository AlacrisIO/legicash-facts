open Legibase

(** type for a description of a data schema *)
type schema

(** type for bytes to be processed according to some schema *)
type data

(** type for any digest among many: sha256, keccak256, etc. *)
type any_digest

(** type for some search criterion *)
type search_criterion

type operation =
  | Publish of schema*data (* Publish some data *)
  | Get_signature of schema*any_digest (* Sign that you have seen this data *)
  | Get_data of schema*any_digest (* Get data from a given digest *)
  | Subscribe of schema*search_criterion (* Subscribe to data that satisfies some criterion *)

type registry_state

type ('input, 'output) registry_action = ('input, 'output, registry_state) action


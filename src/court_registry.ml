open Legibase

type schema

type data

type any_digest

type search_criterion

type operation =
  | Publish of schema*data
  | Get_signature of schema*any_digest
  | Get_data of schema*any_digest
  | Subscribe of schema*search_criterion

type registry_state

type ('input, 'output) registry_action = ('input, 'output, registry_state) action


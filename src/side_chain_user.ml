open Lib
open Action
open Yojsoning
open Marshaling
open Crypto
open Db
open Db_types
open Merkle_trie
open Side_chain

module KnowledgeStage = struct
  type t = Unknown | Pending | Confirmed | Rejected
  let to_char = function Unknown -> 'U' | Pending -> 'P' | Confirmed -> 'C' | Rejected -> 'R'
  let of_char = function | 'U' -> Unknown | 'P' -> Pending | 'C' -> Confirmed | 'R' -> Rejected
                         | _ -> raise (Internal_error "Invalid KnowledgeStage character")
  module PrePersistable = struct
    type nonrec t = t
    let marshaling = marshaling_map to_char of_char char_marshaling
    let make_persistent = already_persistent
    let walk_dependencies = no_dependencies
    let yojsoning = yojsoning_map to_char of_char char_yojsoning
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
end

module Episteme = struct
  [@warning "-39"]
  type t =
    { request: Request.t signed
    ; confirmation_option: Confirmation.t signed option
    ; main_chain_confirmation_option: Main_chain.Confirmation.t option }
  [@@deriving lens, yojson]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling3
        (fun { request; confirmation_option; main_chain_confirmation_option } ->
           request, confirmation_option, main_chain_confirmation_option)
        (fun request confirmation_option main_chain_confirmation_option ->
           { request; confirmation_option; main_chain_confirmation_option })
        (marshaling_signed Request.marshaling)
        (option_marshaling (marshaling_signed Confirmation.marshaling))
        (option_marshaling Main_chain.Confirmation.marshaling)
    let walk_dependencies = no_dependencies
    let make_persistent = normal_persistent
    let yojsoning = {to_yojson;of_yojson}
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
end

module UserAccountStatePerFacilitator = struct
  [@warning "-39"]
  type t =
    { facilitator_validity: KnowledgeStage.t
    ; confirmed_state: AccountState.t
    ; pending_operations: Episteme.t list }
  [@@deriving lens { prefix=true }, yojson ]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling3
        (fun { facilitator_validity
             ; confirmed_state
             ; pending_operations } ->
          facilitator_validity, confirmed_state, pending_operations)
        (fun facilitator_validity confirmed_state pending_operations ->
           { facilitator_validity
           ; confirmed_state
           ; pending_operations })
        KnowledgeStage.marshaling AccountState.marshaling
        (list_marshaling Episteme.marshaling)
    let walk_dependencies = no_dependencies
    let make_persistent = normal_persistent
    let yojsoning = {to_yojson;of_yojson}
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
  let empty =
    { facilitator_validity= Confirmed
    ; confirmed_state=AccountState.empty
    ; pending_operations= [] }
end

module UserAccountStateMap = MerkleTrie (Address) (UserAccountStatePerFacilitator)

type user_state =
  { main_chain_user_state: Main_chain.user_state
  ; facilitators: UserAccountStateMap.t }
[@@deriving lens]

type ('input, 'action) user_action = ('input, 'action, user_state) action

type ('input, 'action) user_async_action = ('input, 'action, user_state) async_action

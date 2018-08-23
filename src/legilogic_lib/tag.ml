(** Tags used for Persistence *)
open Marshaling
open Persisting

let base_trie = 0x80
let empty = 0x80
let leaf = 0x81
let branch = 0x82
let skip = 0x83

let none = 0x84
let some = 0x85

let side_chain_state = 0xC0
let side_chain_invoice = 0xC1
let base_side_chain_operation = 0xC2
let side_chain_deposit = 0xC2
let side_chain_payment = 0xC3
let side_chain_withdrawal = 0xC4
let side_chain_rx_header = 0xC8
let side_chain_request = 0xC9
let side_chain_user_account_state_per_facilitator = 0xCA
let side_chain_facilitator_state = 0xD0
let side_chain_tx_header = 0xD1
let side_chain_confirmation = 0xD2
let side_chain_account_state = 0xD3
let side_chain_facilitator_fee_schedule = 0xD4

(** Strict upper limit on all tags defined so far *)
let tag_limit = 0xFF

let bad_tag_error start bytes =
  raise (Unmarshaling_error ("bad tag", start, bytes))

module SignaturePrefix = struct
  include Address
  let state_update = Address.of_hex_string "7E91CA540000000057A7E009DA7E000000000001"
end

module Tag = struct
  include UInt16int
  let marshaling =
    { marshal = marshal
    ; unmarshal = fun ?(start=0) bytes ->
        unmarshal_map
          (fun u -> if u < tag_limit then u else
              raise (Unmarshaling_error
                       (Printf.sprintf "bad tag %d" u, start, bytes)))
          unmarshal ~start bytes }
  let make_persistent = already_persistent
  let walk_dependencies = no_dependencies
end
include Persistable (Tag)

(** Tags used for Persistence *)
open Marshaling
open Db

let uint8 = 0x20
let uint16 = 0x21
let uint32 = 0x22
let uint64 = 0x23
let uint128 = 0x24
let uint256 = 0x25
let uint512 = 0x26

let address = 0x2a

let base_trie = 0x80
let empty = 0x80
let leaf = 0x81
let branch = 0x82
let skip = 0x83

let some = 0x84
let none = 0x85

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
  let state_update = Address.of_hex_string "7E91CA540000000057A7E009DA7E00000000001"
end

module Tag = struct
  type t = int
  let marshaling =
    { marshal = marshal_map UInt16.of_int UInt16.marshal
    ; unmarshal =
        fun ?(start=0) bytes ->
          let (u, p) = unmarshal_map UInt16.to_int UInt16.unmarshal ~start bytes in
          if u < tag_limit then
            u, p
          else
            bad_tag_error start bytes }

  let make_persistent = already_persistent
  let walk_dependencies = no_dependencies
end
include Persistable (Tag)

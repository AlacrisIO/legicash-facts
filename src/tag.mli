(** Global tags used for Persistence
    NB: these are fine for hashing,
    but when signing, we want to prepend a long address of some kind to the entire message.
*)
open Db

include PersistableS with type t = int

val uint8 : t
val uint16 : t
val uint32 : t
val uint64 : t
val uint128 : t
val uint256 : t
val uint512 : t

val address : t

val base_trie : t
val empty : t
val leaf : t
val branch : t
val skip : t

val none : t
val some : t

val side_chain_state : t
val side_chain_invoice : t
val base_side_chain_operation : t
val side_chain_deposit : t
val side_chain_payment : t
val side_chain_withdrawal : t
val side_chain_rx_header : t
val side_chain_request : t
val side_chain_user_account_state_per_facilitator : t
val side_chain_facilitator_state : t
val side_chain_tx_header : t
val side_chain_confirmation : t
val side_chain_account_state : t
val side_chain_facilitator_fee_schedule : t

(**
   When signing a message, we want to prepend to the message a long unique tag
   that makes it extremely hard for an attacker to pun messages
   between two different protocols using the same keys (e.g. for two different side-chains).
*)
module SignaturePrefix : sig
  include PersistableS
  val state_update : t
end

val bad_tag_error : int -> Bytes.t -> 'a

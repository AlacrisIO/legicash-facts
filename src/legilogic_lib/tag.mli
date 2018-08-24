(** Global tags used for Persistence NB: these are fine for hashing, but when
    signing, we want to prepend a long address of some kind to the entire
    message, to prevent replay attacks. *)
open Marshaling

include MarshalableS with type t = int

val base_trie : t
val empty : t
val leaf : t
val branch : t
val skip : t

val none : t
val some : t

val keypair : t

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

val bad_tag_error : int -> Bytes.t -> 'a

module UInt16int : YojsonMarshalableS with type t = int

val marshal_tagged : t -> 'a marshaler -> 'a marshaler
val unmarshal_tagged : t -> 'a unmarshaler -> 'a unmarshaler
val marshaling_tagged : t -> 'a marshaling -> 'a marshaling

val marshal_2cases : ('a -> bool) -> t -> t -> 'a marshaler -> 'a marshaler -> 'a marshaler
val unmarshal_2cases : t -> t -> 'a unmarshaler -> 'a unmarshaler -> 'a unmarshaler
val marshaling_2cases : ('a -> bool) -> t -> t -> 'a marshaling -> 'a marshaling -> 'a marshaling

val marshal_cases : ('a -> t) -> t -> 'a marshaling array -> 'a marshaler
val unmarshal_cases : t -> 'a marshaling array -> 'a unmarshaler
val marshaling_cases : ('a -> t) -> t -> 'a marshaling array -> 'a marshaling
val new_marshaling_cases : int -> 'a marshaling array
val init_marshaling_cases : t -> 'a marshaling array -> (t * 'a marshaling) list -> unit

val option_marshaling : 'a marshaling -> 'a option marshaling

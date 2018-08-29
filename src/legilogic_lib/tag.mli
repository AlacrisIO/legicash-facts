(** Global tags used for Persistence NB: these are fine for hashing, but when
    signing, we want to prepend a long address of some kind to the entire
    message, to prevent replay attacks. *)
open Marshaling

include MarshalableS with type t = int

(* Magic numbers used for tags *)
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

(* End of magic numbers used for tags *)

val bad_tag_error : int -> Bytes.t -> 'a

module UInt16int : YojsonMarshalableS with type t = int

val marshal_tagged : t -> 'a marshaler -> 'a marshaler
val unmarshal_tagged : t -> 'a unmarshaler -> 'a unmarshaler
(** [marshaling_tagged tag m] provides an ['a marshaling] which prefixes
   marshaled ['a]s with [tag]. *)
val marshaling_tagged : t -> 'a marshaling -> 'a marshaling

(** [marshal_2cases f tag1 tag2 m1 m2 buffer x] marshals [x] to [buffer] using
   [(tag1, m1)] if [f x] is true, or [(tag2, m2)] otherwise*)
val marshal_2cases : ('a -> bool) -> t -> t -> 'a marshaler -> 'a marshaler -> 'a marshaler
(** See [marshal_2cases] *)
val unmarshal_2cases : t -> t -> 'a unmarshaler -> 'a unmarshaler -> 'a unmarshaler
(** See [marshal_2cases] *)
val marshaling_2cases : ('a -> bool) -> t -> t -> 'a marshaling -> 'a marshaling -> 'a marshaling

val marshal_cases : ('a -> t) -> t -> 'a marshaling array -> 'a marshaler
val unmarshal_cases : t -> 'a marshaling array -> 'a unmarshaler
(** [marshaling_cases tag_of base_tag cases] marshaller for multiple [cases],
   based on clasifier [tag_of], which returns an offset into [cases], plus
   [base_tag]. *)
val marshaling_cases : ('a -> t) -> t -> 'a marshaling array -> 'a marshaling
(** Create an array of [marshaling_not_implemented] marshalers, as a base for
   [cases] in [init_marshaling_cases]. *)
val new_marshaling_cases : int -> 'a marshaling array
(** [init_marshaling_cases base_tag cases l] bashes the
   cases in [l] into [cases], to provide a [marshaling_cases] with the given
   [base_tag]. *)
val init_marshaling_cases : t -> 'a marshaling array -> (t * 'a marshaling) list -> unit

(** Marshals an ['a option] using [none] tag or [some] tag followed by
   binary representation of object. *)
val option_marshaling : 'a marshaling -> 'a option marshaling

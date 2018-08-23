open Marshaling

val list_marshaling : 'a marshaling -> 'a list marshaling
val option_marshaling : 'a marshaling -> 'a option marshaling

val marshal_tagged : Tag.t -> 'a marshaler -> 'a marshaler
val unmarshal_tagged : Tag.t -> 'a unmarshaler -> 'a unmarshaler
val marshaling_tagged : Tag.t -> 'a marshaling -> 'a marshaling

val marshal_2cases : ('a -> bool) -> Tag.t -> Tag.t -> 'a marshaler -> 'a marshaler -> 'a marshaler
val unmarshal_2cases : Tag.t -> Tag.t -> 'a unmarshaler -> 'a unmarshaler -> 'a unmarshaler
val marshaling_2cases : ('a -> bool) -> Tag.t -> Tag.t -> 'a marshaling -> 'a marshaling -> 'a marshaling

val marshal_cases : ('a -> Tag.t) -> Tag.t -> 'a marshaling array -> 'a marshaler
val unmarshal_cases : Tag.t -> 'a marshaling array -> 'a unmarshaler
val marshaling_cases : ('a -> Tag.t) -> Tag.t -> 'a marshaling array -> 'a marshaling
val new_marshaling_cases : int -> 'a marshaling array
val init_marshaling_cases : Tag.t -> 'a marshaling array -> (Tag.t * 'a marshaling) list -> unit


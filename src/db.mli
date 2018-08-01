open Lib
open Marshaling
open Crypto

type db
type transaction

val the_db : unit -> db
val the_db_transaction : unit -> transaction
val post_db_transaction : unit -> unit Event.channel
val has_db_key : string -> bool
val get_db : string -> string option
val put_db : string -> string -> unit
val remove_db : string -> unit
val db_value_of_digest : (string -> 'a) -> Digest.t -> 'a
val db_digest_of_value : ('a -> string) -> 'a -> Digest.t

type 'a dv

module type DigestValueBaseS = sig
  include RefS
  type digest
  val of_digest : digest -> t
  val digest : t -> digest
end

module type DigestValueS = sig
  module Value : MarshalableS
  include DigestValueBaseS
    with type t = Value.t dv
     and type value = Value.t
     and type digest = Digest.t
end

module DigestValue (Value : DigestibleS) :
  (DigestValueS
   with module Value = Value
    and type t = Value.t dv
    and type value = Value.t
    and type digest = Digest.t)

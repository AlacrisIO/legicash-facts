open Yojsoning
open Marshaling
open Integer

(** Digest (160-bit, using keccak256) *)
type digest
[@@deriving rlp]

(** given a string, return its keccak256 digest as a big-endian string *)
val keccak256_string : string -> string

(** given a string, return its keccak256 digest as an object *)
val digest_of_string : string -> digest

(** given a marshaler and an object, return its digest *)
val digest_of_marshal : 'a marshaler -> 'a -> digest

(** given a marshal_bytes method and an object, return its digest *)
val digest_of_marshal_bytes : ('a -> Bytes.t) -> 'a -> digest

(** a digest object full of zeroes, as a placeholder *)
val null_digest : digest

module type DigestibleS = sig
  include MarshalableS
  val digest : t -> digest (* Use Keccac256 as per Ethereum *)
end

module Digestible (M : MarshalableS) : DigestibleS with type t = M.t

module DigestibleOfPreMarshalable (P : PreMarshalableS) : DigestibleS with type t = P.t

module Digest : UIntS with type t = digest

(** Database access

    Note that we assume that there are global objects used in a single-threaded way.
    Otherwise, we would want thread-local objects, dynamically bound objects, or lexical objects,
    instead of global objects, to communicate with the database, and/or we would want locks around access.
*)
open Lib
open Marshaling
open Crypto

type db = LevelDB.db

type transaction = LevelDB.writebatch

let db_name = "legicash_state"

let the_global ?(ref=ref None) make_foo =
  fun () ->
    if !ref == None then
      ref := Some (make_foo ());
    option_get !ref

let the_db : unit -> db =
  the_global (fun () -> LevelDB.open_db db_name)

let the_db_batching_channel : unit -> (transaction * unit Event.channel) Event.channel =
  the_global Event.new_channel

let run_db_batching_thread () =
  let db = the_db () in
  let channel = the_db_batching_channel () in
  while true do
    let (transaction, continuation) = Event.sync (Event.receive channel) in
    LevelDB.Batch.write db ~sync:true transaction ;
    ignore (Event.send continuation ()) ;
  done

let the_db_batching_thread =
  the_global (Thread.create run_db_batching_thread)

let the_db_transaction_ref : (transaction option ref) =
  ref None

let the_db_transaction =
  the_global ~ref:the_db_transaction_ref LevelDB.Batch.make

let post_db_transaction () =
  let channel = the_db_batching_channel () in
  let _ = the_db_batching_thread () in
  let transaction = the_db_transaction () in
  let continuation = Event.new_channel () in
  the_db_transaction_ref := None ;
  ignore (Event.send channel (transaction, continuation)) ;
  continuation

let has_db_key key =
  LevelDB.mem (the_db ()) key

let get_db key =
  LevelDB.get (the_db ()) key

let put_db key data =
  let transaction = the_db_transaction () in
  LevelDB.Batch.put transaction key data

let remove_db key =
  let transaction = the_db_transaction () in
  LevelDB.Batch.delete transaction key

let content_addressed_storage_prefix = "K256"

let content_addressed_storage_key digest =
  content_addressed_storage_prefix ^ Digest.to_big_endian_bits digest

let db_value_of_digest unmarshal_string digest =
  digest |> content_addressed_storage_key |> get_db |> option_get |> unmarshal_string

let db_digest_of_value marshal_string value =
  let data = marshal_string value in
  let digest = digest_of_string data in
  let key = content_addressed_storage_key digest in
  (if not (has_db_key key) then put_db key data) ;
  digest

(** TODO: mechanism to forget old values? Or is GC enough? *)
type 'a dv = {digest: Digest.t Lazy.t; value: 'a Lazy.t}

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

module DigestValue (Value : DigestibleS) = struct
  module Value : DigestibleS with type t = Value.t = Value
  type value = Value.t
  type digest = Digest.t
  type t = value dv
  let get t = Lazy.force t.value
  let digest t = Lazy.force t.digest
  let make value =
    { digest=lazy (db_digest_of_value Value.marshal_string value)
    ; value=lazy value }
  let of_digest digest =
    { digest=lazy digest
    ; value=lazy (db_value_of_digest Value.unmarshal_string digest)}
end

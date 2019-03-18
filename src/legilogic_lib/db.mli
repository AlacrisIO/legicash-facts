(* Managing the database connection *)
open Action

(** TODO: virtualize the notion of database interface so we can have nested commits? *)

val open_connection : string -> unit Lwt.t
(** Open a database. The db_name is the name of the database, which is a path
    absolute or relative to getcwd().

    See [start_server] implementation for details on the background thread which
    actually interacts with the db. *)

val run : db_name:string -> (unit -> 'a Lwt.t) -> 'a
(** Run some code in a database.
    TODO: close the connection.
    Beware: this runs the code in an Lwt_main.run, and shouldn't
    be called if some other part of the code calls Lwt_main.run.
*)

val check_connection : unit -> unit
(** Raise an exception unless there is a DB connection open *)


(* User access primitives *)
type ('key, 'value) kv = {key: 'key; value: 'value}
[@@deriving rlp]

val has_key : string -> bool
(** Does the database have an entry with this key? *)

val get : string -> string option
(** Given a key, return an option for the entry associated to the key in the database *)

val put : string -> string -> unit Lwt.t
(** Given a key and a value, add an entry mapping that key to that value in the database
    as part of the current transaction. *)

val put_many : (string, string) kv list -> unit Lwt.t
(** Given a list of {key;value} pairs, atomically add an entry mapping each key
    to the corresponding value in the database as part of the same, current transaction. *)

val remove : string -> unit Lwt.t
(** Given a key and a value, remove any entry mapping that key to a value in the database
    as part of the current transaction. *)

val commit : unit -> unit Lwt.t
(** Commit the current transaction and wait for all data to be flushed to disk before returning.
    Moreover, if the process crashes or is otherwise terminated, then is restarted,
    then the same list of actions must be registered at process initialization
    based on the state that was persisted.
    The actions must be idempotent, in case the process crashes after effecting the action,
    but before persisting the fact that they have been effected.
    For asynchronous actions, it is cheaper to register a copromise to notify
    with the async_commit or commit function,
    but you can also spawn it as part of a larger synchronous commit hook. *)

val async_commit : unit Lwt.u -> unit Lwt.t
(** Commit the current transaction and resolve the promise given as parameter
    when it's fully flushed to disk.

    See commit for a note on post-commit actions in general. *)

val commit_hook : string -> (unit -> unit Lwt.t) -> unit Lwt.t
(** Register a post-commit finalizer action to be run synchronously
    after this batch commits and before the next batch commits.
    The finalizer action is associated to a db key identifier,
    and that action should always be the same or otherwise vary monotonically
    for a given key within a given batch (e.g. a growing list of messages to send),
    so that multiple calls be meaningful; alternatively, the system must ensure
    that a key only gets registered once per batch.

    See commit for a note on post-commit actions in general. *)

type transaction
[@@deriving rlp]

val open_transaction : unit -> transaction Lwt.t
(** Start a transaction. *)

val commit_transaction : transaction -> unit Lwt.t
(** Start a transaction. *)

val with_transaction : ('i, 'o) Lwter.arr -> ('i, 'o) Lwter.arr

val committing : ('a, 'a) Lwter.arr
(** Commit the transaction, pass data through *)

val committing_async : ('a, 'a) Lwter.arr
(** Schedule a transaction to be committed in parallel, pass data through *)

val get_batch_id : unit -> int Lwt.t
(** Get the id of the current batch, so you can test whether it has changed the proper amount *)

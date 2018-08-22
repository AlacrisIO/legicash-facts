(* Managing the database connection *)

(** Open a database.
   The db_name is the name of the database,
   which is a path absolute or relative to getcwd().
 *)
val open_connection : db_name:string -> unit Lwt.t

(** Run some code in a database.
   TODO: close the connection.
   Beware: this runs the code in an Lwt_main.run, and shouldn't
   be called if some other part of the code calls Lwt_main.run.
 *)
val run : db_name:string -> (unit -> 'a Lwt.t) -> 'a

(** Raise an exception unless there is a DB connection open *)
val check_connection : unit -> unit



(* User access primitives *)

(** Does the database have an entry with this key? *)
val has_key : string -> bool

(** Given a key, return an option for the entry associated to the key in the database *)
val get : string -> string option

(** Given a key and a value, add an entry mapping that key to that value in the database
    as part of the current transaction. *)
val put : string -> string -> unit Lwt.t

(** Given a key and a value, remove any entry mapping that key to a value in the database
    as part of the current transaction. *)
val remove : string -> unit Lwt.t

(** Commit the current transaction and wait for all data to be flushed to disk before returning. *)
val commit : unit -> unit Lwt.t

(** Commit the current transaction and resolve the promise given as parameter
    when it's fully flushed to disk. *)
val async_commit : unit Lwt.u -> unit Lwt.t

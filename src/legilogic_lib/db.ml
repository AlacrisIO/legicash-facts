(** Database access

    This file defines sequential access to a simple key-value store.
    Multiple Lwt threads can read the database with has_key and get,
    contribute to the current transaction with put and remove,
    and decide when they want to wait for their values to be safely persisted
    using commit and async_commit.

    WARNING 1: Global data structures

    We use global objects, that we must be used in a single-threaded way
    for a single database access at a time.
    Otherwise, we would want thread-local objects, dynamically bound objects, or lexical objects,
    instead of global objects, to communicate with the database, and/or we would want locks around access.

    WARNING 2: Not-so-pure functional interface

    Our interface assumes that database reads of content-addressed data is "not a side-effect"
    and can be written in direct style, whereas database writes are side-effects that require monadic
    style, except that flagging an object as persisted for walk optimization can be done directly.
    A conceptual mess? Yes it is! All because OCaml's type system can't deal with this kind of abstraction.
*)
open Lib
open Marshaling
open Action
open Lwter

type db = LevelDB.db
type batch = LevelDB.writebatch

type transaction = int

(* "transactions" will block the batch from being sent to disk for committing
   until all currently open transactions are closed, even though the batch may otherwise be ready to merge.
   On the other hand, while a batch is ready but waiting for transactions to be closed,
   attempts to open new transaction will themselves be blocked. *)

(* type snapshot = LevelDB.snapshot *)

type ('key, 'value) kv = {key: 'key; value: 'value}
let kv_to_yojson key_to_yojson value_to_yojson {key; value} =
  `List [key_to_yojson key; value_to_yojson value]

(** Interaction with mutating state in LevelDB *)
type request =
  (** Store [value] at [key] in the database *)
  | Put of (string, string) kv
  (** Store many key value pairs in the database *)
  | PutMany of (string, string) kv list (* Do we still need PutMany for performance? Semantically it's not needed since we have transactions *)
  (** Remove [key] from LevelDB *)
  | Remove of string
  (** Register a promise to be fulfilled after the commit.
      The system must otherwise ensure that the action that follows this promise
      will be restarted by a new instance of this program in case the process crashes after this commit,
      or is otherwise some client's responsibility to restart if the program acts as a server.
  *)
  | Commit of unit Lwt.u
  (** Register post-commit finalizer actions to be run after this batch commits
      and before the next batch commits, with a db key identifier.
      This would typically spawn an async or some such.
      The action should always be the same for a given key,
      or otherwise change monotonically within a batch,
      so that multiple calls be meaningful, and/or the system must ensure
      a key only gets activated once per batch.
      Presumably to be called within a transaction.
  *)
  | Commit_hook of string * (unit -> unit Lwt.t)
  (** Internal message indicating that LevelDB is available for interaction,
      the previous transaction of given number having been committed,
      and the following list of synchronous hooks having been registered at previous batch. *)
  | Ready of int * (string * (unit -> unit Lwt.t)) list
  (** Open Transaction *)
  | Open_transaction of transaction Lwt.u
  (** Close a Transaction *)
  | Close_transaction of transaction
  (** Get the batch id: not just for testing,
      but also, within a transaction, to get the id to prepare a hook,
      e.g. to send newly committed but previously unsent messages. *)
  | Get_batch_id of int Lwt.u

(* For debugging purposes only *)
let [@warning "-32"] yojson_of_request = function
  | Put {key; value} -> `List [`String "Put"
                              ; Data.to_yojson key
                              ; Data.to_yojson value]
  | PutMany list -> `List [`String "PutMany";
                           `List (List.map (kv_to_yojson Data.to_yojson Data.to_yojson) list)]
  | Remove key -> `List [`String "Remove"; Data.to_yojson key]
  | Commit _ -> `List [`String "Commit"]
  | Commit_hook _ -> `List [`String "Commit_hook"]
  | Ready (n, _) -> `List [`String "Ready"; `Int n]
  | Open_transaction _ -> `List [`String "Open_transaction"]
  | Close_transaction _ -> `List [`String "Close_transaction"]
  | Get_batch_id _ -> `List [`String "Test_get_batch_id"]

(* One might be tempted to use Lwt.task instead of an option ref to define
   the db_name, db. Unhappily, at least as far as the db goes,
   we can't rely on it, because we use the db read-only through implicit
   side-effects to preserve a pure-functional interface to our merkle trees
   (one man's explicit side-effects are another man's implicit infrastructure).

   On the other hand, we do make the mailbox its own global binding,
   initialized as it is defined as the regular binding it is, rather than
   put it in the connection object (though we did it before and may
   again in the future).
*)
type connection =
  { db_name : string (* actually a path, absolute or relative to getcwd, to the data directory *)
  ; db : db }

(* The mailbox to communicate with the db daemon.
   WARNING: GLOBAL STATE, so there's only one database daemon running. *)
let db_mailbox : request Lwt_mvar.t = Lwt_mvar.create_empty ()

(* The mailbox to communicate with the db daemon.

   Any Lwt thread which [Lwt_mvar.put]s to here is blocked until the message is
   read. *)
let the_connection_ref : (connection option ref) = ref None

let check_connection () =
  assert (!the_connection_ref != None)

(** Starts a loop checking [db_mailbox] for [request]s.

    A [Commit] kicks off a system thread which commits [request]s received since
    initialization or the last commit. Internal [ready] state tracks whether a
    [Commit] is currently in progress. If so, it's false. *)
let start_server ~db_name ~db () =
  let open Lwt in
  Logging.log "Opening LevelDB connection to db %s" db_name;
  let rec outer_loop batch_id previous () =
    let notify_list = ref [] in
    let hooks = Hashtbl.create 16 in
    let transaction_counter = ref 0 in
    let open_transactions : (transaction, unit) Hashtbl.t = Hashtbl.create 64 in
    let open_transaction u =
      let transaction = !transaction_counter in
      transaction_counter := transaction + 1;
      Hashtbl.replace open_transactions transaction ();
      Lwt.wakeup_later u transaction in
    let blocked_transactions : transaction Lwt.u list ref = ref [] in
    let batch = LevelDB.Batch.make () in
    let (wait_on_batch_commit, notify_batch_commit) = Lwt.task () in
    Lwt.async (fun () ->
      previous
      >>= fun (notify_list, hooks) ->
      List.iter (flip Lwt.wakeup_later ()) notify_list;
      Lwt_mvar.put db_mailbox (Ready (batch_id, hooks)));
    let rec inner_loop ~ready ~triggered ~held =
      if triggered && ready && not held then
        begin
          (*Logging.log "COMMIT BATCH %d!" batch_id;*)
          (* Fork a system thread to handle the commit;
             when it's done, wakeup the wait_on_batch_commit promise *)
          Lwt.async (fun () ->
            Lwt_preemptive.detach
              (fun () -> LevelDB.Batch.write db ~sync:true batch) ()
            >>= fun () ->
            (* Logging.log "BATCH %d COMMITTED!" batch_id;*)
            Lwt.wakeup_later notify_batch_commit (!notify_list, bindings_of_hashtbl hooks);
            return_unit);
          transaction_counter := 0;
          List.iter open_transaction !blocked_transactions;
          blocked_transactions := [];
          outer_loop (batch_id + 1) wait_on_batch_commit ()
        end
      else
        Lwt_mvar.take db_mailbox
        >>= function
        | Put {key;value} ->
          (*Logging.log "PUT key: %s value: %s" (Hex.unparse_0x_data key) (Hex.unparse_0x_data value);*)
          LevelDB.Batch.put batch key value;
          inner_loop ~ready ~triggered ~held
        | PutMany list ->
          List.iter (fun {key; value} -> LevelDB.Batch.put batch key value) list;
          inner_loop ~ready ~triggered ~held
        | Remove key ->
          (*Logging.log "REMOVE key: %s" (Hex.unparse_0x_data key);*)
          LevelDB.Batch.delete batch key;
          inner_loop ~ready ~triggered ~held
        | Commit notify ->
          (*Logging.log "COMMIT in batch %d" batch_id;*)
          notify_list := notify::!notify_list;
          inner_loop ~ready ~triggered:true ~held
        | Commit_hook (key, hook) ->
          Hashtbl.replace hooks key hook;
          inner_loop ~ready ~triggered ~held
        | Ready (n, hooks) ->
          assert (n = batch_id);
          Lwt_list.iter_s (fun (_key, hook) -> hook ()) hooks
          >>= fun () ->
          inner_loop ~ready:true ~triggered ~held
        | Open_transaction u ->
          (* TODO: assert that the transaction_counter never wraps around?
             Or check and block further transactions? *)
          if held then
            blocked_transactions := u :: !blocked_transactions
          else
            open_transaction u;
          inner_loop ~ready ~triggered ~held:true
        | Close_transaction transaction ->
          Hashtbl.remove open_transactions transaction;
          inner_loop ~ready ~triggered ~held:(Hashtbl.length open_transactions > 0)
        | Get_batch_id continuation ->
          Lwt.wakeup_later continuation batch_id;
          inner_loop ~ready ~triggered ~held in
    inner_loop ~ready:false ~triggered:false ~held:(Hashtbl.length open_transactions > 0) in
  outer_loop 0 (Lwt.return ([],[])) ()

let open_connection db_name =
  match !the_connection_ref with
  | Some x ->
    if x.db_name = db_name then
      (Logging.log "Process already has a LevelDB connection to db %s, won't start another one" db_name;
       Lwt.return_unit)
    else
      bork "Cannot start a LevelDB connection to db %s because there's already one to %s"
        db_name x.db_name
  | None ->
    let db = LevelDB.open_db db_name in
    the_connection_ref := Some { db_name ; db };
    Lwt.async (start_server ~db_name ~db);
    Lwt.return_unit

let the_connection =
  the_global the_connection_ref
    (fun () -> bork "no db connection for pid %d" (Unix.getpid ()))

let the_db () = (the_connection ()).db

let request r =
  Lwt_mvar.put db_mailbox r

let run ~db_name thunk =
  Lwt_main.run (open_connection db_name >>= thunk)

let async_commit continuation =
  Commit continuation |> request

let commit () =
  let (promise, resolver) = Lwt.task () in
  async_commit resolver >>= fun () -> promise

let commit_hook key action =
  Commit_hook (key, action) |> request

let has_key key =
  LevelDB.mem (the_db ()) key

let get key =
  LevelDB.get (the_db ()) key
(* Uncomment this line to spy on db read accesses:
   |> function None -> Printf.printf "key %s not found\n%!" (Hex.unparse_hex_string key) ; None | Some data -> Printf.printf "key %s value %s\n%!" (Hex.unparse_hex_string key) (Hex.unparse_hex_string data); Some data *)

let put key value =
  Put {key; value} |> request

let put_many list =
  PutMany list |> request

let remove key =
  Remove key |> request

let open_transaction () =
  let (promise, resolver) = Lwt.task () in
  Open_transaction resolver |> request
  >>= fun () -> promise

let commit_transaction tx =
  Close_transaction tx |> request
  >>= commit

let with_transaction thunk =
  open_transaction ()
  >>= fun tx ->
  thunk ()
  >>= fun result ->
  commit_transaction tx
  >>= const result

let get_batch_id () =
  let (t,u) = Lwt.task () in
  request (Get_batch_id u)
  >>= fun () -> t

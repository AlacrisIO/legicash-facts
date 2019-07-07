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

let db_log = false

type db = LevelDB.db
type batch = LevelDB.writebatch

type transaction = int
[@@deriving rlp]

(* Our "transactions" will block the batch from being sent to disk for committing
   until all currently open transactions are closed, even though the batch may otherwise be ready to merge.
   On the other hand, while a batch is ready but waiting for transactions to be closed,
   attempts to open new transaction will themselves be blocked.
   In other words, we have only roll-forward, not roll-back of transactions, and so
   transactions better be short, or there may be deadlock and/or memory overflow.

   NB: We do not at this time implement *nested* transactions... you will deadlock if you try,
   and thus you must develop a strict discipline of transaction ownership to avoid the issue,
   clearly identifying what are the "top-level" functions that contain transactions,
   and sub-functions that don't --- yet without typing support at this time.
   To properly implement nested transactions, we'd need to carry around a transaction context
   in some dynamic binding. Since OCaml and Lwt don't implement dynamic bindings, we'd have to
   implement our own solution based on some reader monad (transformation layer) to all our
   monads to provide dynamic binding.
   We could also have some trivial session types to force top-level vs non-top-level functions.
*)

(* type snapshot = LevelDB.snapshot *)

type ('key, 'value) kv = {key: 'key; value: 'value}
[@@deriving rlp]

let kv_to_yojson key_to_yojson value_to_yojson {key; value} =
  `List [key_to_yojson key; value_to_yojson value]

(** Interaction with mutating state in LevelDB *)
type request =
  (** Store [value] at [key] in the database *)
  | Put of (string, string) kv
  (** Store many key value pairs in the database *)
  (* Do we still need PutMany for performance? Semantically it's not needed since we have transactions *)
  | PutMany of (string, string) kv list
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
  | Put {key; value}    -> `List [`String "Put"
                                 ; Data.to_yojson key
                                 ; Data.to_yojson value ]
  | PutMany list        -> `List [`String "PutMany"
                                 ;`List (List.map (kv_to_yojson Data.to_yojson Data.to_yojson) list) ]
  | Remove key          -> `List [`String "Remove"; Data.to_yojson key]
  | Commit _            -> `List [`String "Commit"]
  | Commit_hook _       -> `List [`String "Commit_hook"]
  | Ready (n, _)        -> `List [`String "Ready"
                                 ;`Int n ]
  | Open_transaction _  -> `List [`String "Open_transaction"]
  | Close_transaction _ -> `List [`String "Close_transaction"]
  | Get_batch_id _      -> `List [`String "Test_get_batch_id"]

type connection =
  (* `db_name` is a path, absolute or relative to getcwd, to the data directory *)
  { db_name: string
  ; db:      db
  }

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
(* TODO break `start_server` into bite-sized chunks for readability's sake *)
let start_server ~db_name ~db () =
  let open Lwt in
  if db_log then
    Logging.log "Opening LevelDB connection to db %s" db_name;
  let transaction_counter = ref 0 in

  let rec outer_loop batch_id previous () =
    let notify_list                                       = ref []
    and hooks                                             = Hashtbl.create 16
    and open_transactions : (transaction, unit) Hashtbl.t = Hashtbl.create 64
    and blocked_transactions : transaction Lwt.u list ref = ref []
    and batch                                             = LevelDB.Batch.make ()
    and (wait_on_batch_commit, notify_batch_commit)       = Lwt.task ()

    in let open_transaction u =
      let transaction         = !transaction_counter
      in transaction_counter := transaction + 1;
      Hashtbl.replace open_transactions transaction ();
      Lwt.wakeup_later u transaction

    in Lwt.async (fun () ->
      previous >>= fun (notify_list, hooks) ->
        List.iter (flip Lwt.wakeup_later ()) notify_list;
        Lwt_mvar.put db_mailbox (Ready (batch_id, hooks)));

    let rec inner_loop ~ready ~triggered ~held =
      if triggered && ready && not held then
        begin
          (* Fork a system thread to handle the commit;
             when it's done, wakeup the wait_on_batch_commit promise *)
          Lwt.async (fun () -> Lwt_preemptive.detach
            (fun () -> LevelDB.Batch.write db ~sync:true batch) ()
              >>= fun () -> Lwt.wakeup_later notify_batch_commit
                                             (!notify_list, bindings_of_hashtbl hooks);
                            return_unit);

          List.iter open_transaction !blocked_transactions;
          blocked_transactions := [];
          outer_loop (batch_id + 1) wait_on_batch_commit ()
        end
      else
        Lwt_mvar.take db_mailbox >>= function
        | Put {key;value} ->
          if db_log then
            Logging.log "key=%s value=%s" key value;
          (*            Logging.log "key=(omit) value=(omit)";*)
          LevelDB.Batch.put batch key value;
          inner_loop ~ready ~triggered ~held

        | PutMany list ->
          List.iter (fun {key; value} -> LevelDB.Batch.put batch key value) list;
          inner_loop ~ready ~triggered ~held

        | Remove key ->
          LevelDB.Batch.delete batch key;
          inner_loop ~ready ~triggered ~held

        | Commit notify ->
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
             Or check and block further transactions when it does, before resetting the counter? *)
          (* TODO: commenting out the ready && triggered helps detect / enact deadlocks when running
             tests, by having only one active transaction at a time; but then the hold can and
             should be released as soon as "the" transaction is complete, unless we're already both
             ready && triggered for the next batch commit. Have an option for that? *)
          if ready && triggered && held then
            ((*Logging.log "HOLDING A TRANSACTION";*)
              blocked_transactions := u :: !blocked_transactions)
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
  if db_log then
    Logging.log "DB: Beginning of open_connection db_name=%s" db_name;
  match !the_connection_ref with
  | Some x ->
     if db_log then
       Logging.log "DB: open_connection, Some x case";
     if x.db_name = db_name then
      (if db_log then
         Logging.log "Process already has a LevelDB connection to db %s, won't start another one" db_name;
       Lwt.return_unit)
    else
      bork "Cannot start a LevelDB connection to db %s because there's already one to %s"
        db_name x.db_name

  | None ->
     if db_log then
       Logging.log "DB: open_connection, None case";
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

let get : string -> string option =
  fun key ->
  if db_log then
    Logging.log "Db.get access for key=%s" key;
  (*    Logging.log "Db.get access for key=(omit)";*)
  let value = LevelDB.get (the_db ()) key in
  match value with
  | None -> (if db_log then
               Logging.log "None case";
             value)
  | Some x -> (if db_log then
                 Logging.log "Some x =%s" x;
               value)
(* Uncomment the following to spy on db read accesses:
    |> function
      | None ->
        Printf.printf "key %s not found\n%!" (Hex.unparse_hex_string key);
        None

      | Some data ->
        Printf.printf "key %s value %s\n%!"
          (Hex.unparse_hex_string key)
          (Hex.unparse_hex_string data);
        Some data
*)

let put key value =
  if db_log then
    Logging.log "Db.put key=%s value=%s" key value;
  (*    Logging.log "Db.put key=(omit) value=(omit)";*)
  Put {key; value} |> request

let put_many list =
  PutMany list |> request

let remove key =
  Remove key |> request

let open_transaction () =
  let ((promise, resolver) : (transaction Lwt.t * transaction Lwt.u)) = Lwt.task () in
  Open_transaction resolver |> request
  >>= fun () -> promise

let close_transaction tx =
  Close_transaction tx |> request

let commit_transaction tx =
  close_transaction tx
  >>= commit

let with_transaction f i =
  open_transaction ()
    >>= fun tx -> f i
    >>= fun o  -> close_transaction tx
    >>= fun _  -> return o

let committing       = fun s -> commit () >>= const s
let committing_async = fun s -> Lwt.async commit; return s

let get_batch_id () =
  let (t,u) = Lwt.task () in
  request (Get_batch_id u)
  >>= fun () -> t

module Test = struct
  open Integer

  let%test "test-trivial-testdb-commits" =
    let rec test_base = 1000 * Random.int 1000
    and max_test_int  = 100

    and test_commit i =
      let key = Nat.(add (of_0x "0xdeadbeef00000000") (of_int i)
        |> to_big_endian_bits)
      in put key (string_of_int (test_base + i))
        >>= fun () -> commit ()
        >>= fun () -> Lwt.return_unit

    in run ~db_name:"testdb" @@ fun () ->
       get_batch_id ()
         >>= fun initial_batch_id ->
           Lwt.join (List.map test_commit (range 1 max_test_int))
         >>= fun () ->
           get_batch_id ()
         >>= fun final_batch_id ->
           let count = final_batch_id - initial_batch_id in
           assert (1 <= count && count < 4) ;
           Lwt.return_true
end

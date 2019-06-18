open Legilogic_lib
open Lib
open Yojsoning
open Marshaling
open Persisting
open Types
open Signing
open Action
open Lwt_exn
open Json_rpc
open Trie

open Ethereum_chain
open Ethereum_json_rpc
open Ethereum_transaction

let ethereum_user_log = false

(* TODO: A much better state machine to get wanted transactions confirmed.

   It is a very bad idea to have more than one ongoing transaction in the mempool:
   you might hope everything goes right and they are added in the correct order,
   but in practice so many things can go wrong and then the mitigations become hell,
   and attackers can get you to fail to transact, to deadlock, to replay your spending,
   to fail to meet a deadline, or more generally fail to meet your contractual obligations.

   Obvious strategies don't work:
   - If you never re-send a transaction, but for some reason one transaction doesn't go through
   because it was received out-of-order by the winning PoW nodes and dropped on the ground,
   or otherwise was lost in the shuffle of network packet drops, then you deadlock
   - If you always re-send a transaction, but never update the nonce, and some other client
   using the same private key (WHY? That should be a red alert anyway, unless it's actually
   another copy of yourself sending another variant of the same transaction due to netsplit,
   and you re-synch after netmerge), or some other transaction on the same client races me
   (if you fail to sequentialize transactions one at a time through a single thread),
   then you deadlock.
   - If you always re-send a transaction, and you update the nonce if you see yours is out-of-date,
   then you can race yourself and/or other transactions into sending multiple copies of a same
   transaction and end up spending many times over what you wanted to spend (very bad).
   - Whatever decisions you make based on what the ethereum node tells you, it can give you _hints_
   about things that are going on, but nothing it says is authoritative until it is, which is only
   30 minutes later (or say 10 minutes, if you accept the risk of lower security).
   Until confirmation, whatever it says is subject to revision.
   - The safest would be to nurse each and every transaction to either completion or definite failure
   before even attempting the next one, but then that's only one transaction per account every 30
   minutes minimum (NB: binance is OK with 36 confirmations ~10 minutes),
   and maybe much worse depending on how "definite failure" is defined.
   This suggests that having multiple accounts could be a requirement for playing safe
   with smart contracts: each "system" (itself distributed with redundant workers for reliability)
   has its own private key that won't race with other systems.

   One problem is that your local ethereum node (and/or, in a real network, whichever remote node
   will eventually issue the blocks), sometimes will just drop your signed transactions,
   for whatever reasons: not enough ether, not enough gas, gas price too low, nonce out of synch,
   network error, network split, denial-of-service attack, selective censorship, block-buying
   attack, local reverts due to PoW attacks even less than 33%, etc.
   You have to resend, sometimes with updated gas price, sometimes with updated nonce,
   sometimes even with updated contract parameters, etc.
   Yet, you should be wary of changing anything substantive (to your application)
   about a transaction being sent, or you can race yourselves, and end up paying twice
   (or many more times) to receive a counterpart only once (or not at all).

   A good strategy might take into account what did or didn't happen in not-fully-confirmed blocks,
   yet (obviously) would not consider anything confirmed until it's confirmed.
   It is unclear how best to deal with multiple queued transactions —
   the happy case of sending consecutive nonces automatically is great,
   but when things break down (including due to the aforementioned re-send issues)
   it's a hell that's hard to recover from, since new transactions will race the old ones,
   and any sequential dependency between them becomes quite tricky to enforce.

   One solution: a *batching contract*.
   Multiple transactions are sent atomically via a single call to some generic contract
   that plays them in sequence. Caveat: you better get you gas computation damn right!
   Also mind the size limits to your overall transaction, the possibly more complex gas
   price computation to convince miners to get it through in a timely fashion, etc.
   If you do it right, though, you only have to deal with a single network event,
   which makes the limit of one nursed transaction per 30 minute much more bearable.
   This strategy implies or at least suggests developing a better-than-trivial batching strategy
   to group transactions, similar to what we use in db.ml for batching database writes,
   possibly with its own notion of atomic "transaction sets" that group "transactions" together.

   Additional feature: a *replay barrier*.
   The same generic contract can also help, onerously, with avoiding to replay a transaction multiple
   time in the context where you do want to be able to race yourself.
   Good reasons to race yourself is when you have strong obligations to fulfill in a short deadline,
   but the current chain is wobbly due to some attack, particularly network splits:
   multiple of your workers might be victims of the network split (maybe targetted!),
   and would trigger racing variants of the queued transactions.
   In this case, the contract may associate a semaphore to each application-defined
   atomic set of transactions (for a shared multi-user contract, salted with sender ID);
   it would check that the semaphore wasn't set before to actually play the transaction set,
   and set the semaphore afterwards. Once again, miscompute worst-case gas and you're dead.
   You need to pay extra gas to read and write a semaphore, and will lose gas in case that
   multiple copies make it to the blockchain; but at least you won't lose the principal.
   To avoid the need for a replay barrier, you must always wait for *some* transaction
   with the given nonce to be fully confirmed before you start using the next nonce.

   When you have a tight deadline for some transactions and not others,
   you may have to up the gas price for the transactions you really want to get through,
   with an understanding of the algorithm used by the miners and of the strategy used
   by whoever is trying to bribe the miners out of including your transaction.
   Maybe you have to fill the block gas limit. Or maybe you have to split your transaction
   batch into smaller ones. An experimental study may be necessary, as well as regular
   updates to the software agents --- quite unlike the contracts that are immutable by design,
   transaction posting strategies may have to be mutable and evolving by design

   In some case, the right thing to do might be to consult back with the user,
   and ask them to add more ether, to update their gas price strategy, to deal with potentially
   or actually broken contracts, to watch the missing nodes of their personal database, etc.
   By default, we probably want to queue transactions one by one to avoid nonce-overlap issues;
   we may be more or less aggressive in terms of using nonces without partial or total confirmation
   from previous transactions, depending on the nature of the transactions.

   In any case, to definitely want a single system (even if possibly split into partitions)
   to issue transactions from a given address to minimize races.

   There is potentially a LOT of complexity, and if possible we want to partner with other people
   to define sound strategies... just that is a topic for itself, and there are sometimes games
   that people can play with lock out strategy via paying extra in gas to buy enough blocks to
   lock rivals out of a contract, etc. A generic strategy, DSL for strategies, etc., could be
   a research topic in itself. Sigh.

   Most people don't hit this issue, because they don't abide by a contract binding them to partake
   in distributed transactions across multiple blockchains with a priori untrustworthy other parties
   within tight deadlines. And most of the few who do possibly haven't thought deep enough
   about the ins and outs of these issues. Scary.

   Here, for now, we follow a very dumb strategy, of having only one active transaction per address.
   Furthermore, we only sign once, and resending blindly afterwards,
   trusting the gas computation, and trusting the nonce until it's found to be too low.

   TODO: implement an asynchronous way for the UI to peek at the status of a transaction
   while it's going along its slow progress.

   TODO: look at how OMiseGo does it, Andrew Redden tells me they have something public
   (and he has something private).
*)

module OngoingTransactionStatus = struct
  [@warning "-39"]
  type t =
    | Wanted of PreTransaction.t
    | Signed of Transaction.t * SignedTransaction.t
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
  let pre_transaction : t -> PreTransaction.t = function
    | Wanted p -> p
    | Signed (tx, _) -> Transaction.pre_transaction tx
end

module FinalTransactionStatus = struct
  [@@@warning "-39"]
  type t =
    | Confirmed of Transaction.t * SignedTransaction.t * TransactionReceipt.t
    | Failed of OngoingTransactionStatus.t * exn
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
  let pre_transaction : t -> PreTransaction.t = function
    | Confirmed (tx, _, _) -> Transaction.pre_transaction tx
    | Failed (ots, _) -> OngoingTransactionStatus.pre_transaction ots
end

module TransactionStatus = struct
  [@warning "-39"]
  type t =
    | Ongoing of OngoingTransactionStatus.t
    | Final of FinalTransactionStatus.t
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
  let of_ongoing : OngoingTransactionStatus.t -> t = fun x -> Ongoing x
  let of_final : FinalTransactionStatus.t -> t = fun x -> Final x
  let pre_transaction : t -> PreTransaction.t = function
    | Ongoing x -> OngoingTransactionStatus.pre_transaction x
    | Final x -> FinalTransactionStatus.pre_transaction x
  let operation = fun x -> (x |> pre_transaction).operation
end

exception TransactionFailed of OngoingTransactionStatus.t * exn
exception NonceTooLow

type nonce_operation = Peek | Next | Reset [@@deriving yojson]

module NonceTracker = struct
  open Lwter
  module Base = struct
    type context = unit
    module Key = Address
    type key = Key.t
    let key_prefix = "ETNT"
    module State = TrivialPersistable(struct
        type t = Nonce.t option
        [@@deriving yojson, rlp]
        let yojsoning = {to_yojson;of_yojson}
        let marshaling = marshaling_of_rlping rlping
      end)
    type state = State.t
    (* zero is often wrong, but just let it fail and resynchronize *)
    let make_default_state _ _ = None
    type t = (nonce_operation, Nonce.t) Lwter.arr

    let make_activity () address saving =
      sequentialize @@ fun op state ->
         let rec reset () = Lwt_exn.run_lwt
           (retry
              ~retry_window:0.01
              ~max_window:5.0
              ~max_retries:None
              Ethereum_json_rpc.eth_get_transaction_count)
           (address, BlockParameter.Latest)

         and continue result state =
           saving state >>= const (result, state)

         and next nonce = continue nonce (Some Nonce.(add one nonce))

         in match (op, state) with
          | (Reset, _) ->
            continue Nonce.zero None
          | (Peek, None) ->
             reset () >>= fun nonce -> continue nonce (Some nonce)
          | (Peek, Some nonce) ->
             return (nonce, Some nonce)
          | (Next, None) ->
            reset () >>= next
          | (Next, Some nonce) ->
             if ethereum_user_log then
               Logging.log "ETHUSR: NonceTracker nonce=%s" (Revision.to_string nonce);
             next nonce
  end
  include PersistentActivity(Base)
  module State = Base.State
  let reset address = get () address Reset >>= const ()
  let peek address = get () address Peek
  let next address = get () address Next
end


let make_tx_header : Address.t -> TokenAmount.t -> TokenAmount.t -> TxHeader.t Lwt_exn.t =
  fun sender value gas_limit ->
  (* TODO: get gas price and nonce from geth *)
  eth_gas_price () >>= fun gas_price ->
  of_lwt NonceTracker.next sender >>= fun nonce ->
  if ethereum_user_log then
    Logging.log "ETHUSR: make_tx_header sender=%s value=%s gas_limit=%s gas_price=%s nonce=%s" (Address.to_0x sender) (TokenAmount.to_string value) (TokenAmount.to_string gas_limit) (TokenAmount.to_string gas_price) (Nonce.to_0x nonce);
  return TxHeader.{sender; nonce; gas_price; gas_limit; value}

exception Missing_password

let sign_transaction : (Transaction.t, Transaction.t * SignedTransaction.t) Lwt_exn.arr =
  fun transaction ->
  if ethereum_user_log then
    Logging.log "ETHUSR: Beginning of sign_transaction";
  let address = transaction.tx_header.sender in
  (try return (keypair_of_address address).password with
   | Not_found ->
      if ethereum_user_log then
        Logging.log "ETHUSR: Couldn't find registered keypair for %s" (nicknamed_string_of_address address);
      fail Missing_password)
  >>= fun password ->
  if ethereum_user_log then
    Logging.log "ETHUSR: Before personal_sign_transaction";
  personal_sign_transaction (TransactionParameters.of_transaction transaction, password)
  >>= fun signed ->
  if ethereum_user_log then
    Logging.log "ETHUSR: Before final return in sign_transaction";
  return (transaction, signed)

(** Prepare a signed transaction, that you may later issue onto Ethereum network,
    from given address, with given operation, value and gas_limit *)
let make_signed_transaction : Address.t -> Operation.t -> TokenAmount.t -> TokenAmount.t -> (Transaction.t * SignedTransaction.t) Lwt_exn.t =
  fun sender operation value gas_limit ->
  make_tx_header sender value gas_limit
  >>= fun tx_header ->
  if ethereum_user_log then
    Logging.log "Before the sign_transaction";
  sign_transaction Transaction.{tx_header; operation}



(* TODO: move as many functions as possible ethereum_transaction ? *)

let nonce_too_low address =
  if ethereum_user_log then
    Logging.log "ETHUSR: nonce too low for %s" (nicknamed_string_of_address address);
  (* TODO: Send Notification to end-user via UI! *)
  Lwter.(NonceTracker.reset address >>= const (Error NonceTooLow))

let confirmed_or_known_issue : Address.t -> (Digest.t, TransactionReceipt.t) Lwt_exn.arr =
  fun sender hash ->
  let open Lwter in
  Ethereum_json_rpc.eth_get_transaction_receipt hash
  >>= function
  | Ok None ->
     if ethereum_user_log then
       Logging.log "ETHUSR: confirmed_or_nonce_too_low CASE: Ok None";
     nonce_too_low sender
  | Ok (Some receipt) ->
     if ethereum_user_log then
       Logging.log "ETHUSR: confirmed_or_nonce_too_low CASE: Ok (Some receipt)";
     check_transaction_receipt_status receipt
  | Error e ->
     if ethereum_user_log then
       Logging.log "ETHUSR: confirmed_or_nonce_too_low CASE: Error e";
     Lwt_exn.fail e

exception Replacement_transaction_underpriced

let send_raw_transaction : Address.t -> (SignedTransaction.t, Digest.t) Lwt_exn.arr =
  fun sender signed ->
    if ethereum_user_log then
      Logging.log "ETHUSR: send_raw_transaction %s" (SignedTransaction.to_yojson_string signed);
    match signed with
    | SignedTransaction.{raw;tx={hash}} ->
      Lwter.bind (eth_send_raw_transaction raw)
        (function
         | Error (Rpc_error {code= -32000; message="nonce too low"}) ->
            confirmed_or_known_issue sender hash >>= const hash
         | Error (Rpc_error {code= -32000; message})
              when message = "known transaction: " ^ Digest.to_hex_string hash ->
            return hash
         | Error (Rpc_error {code= -32000; message})
              when message = "replacement transaction underpriced" ->
            fail Replacement_transaction_underpriced
         | Error e -> fail e
         | Ok transaction_hash ->
            if transaction_hash = hash then
              return hash
            else
              bork "eth_send_raw_transaction: invalid hash %s instead of %s" (Digest.to_0x transaction_hash) (Digest.to_0x hash))

(** Wait until a transaction has been confirmed by the main chain.
    TODO: understand why an error in a previous version of this code got eaten silently,
    instead of logged and reported, causing a deadlock. *)
let send_and_confirm_transaction : (Transaction.t * SignedTransaction.t, TransactionReceipt.t) Lwt_exn.arr =
  fun (transaction, signed) ->
    if ethereum_user_log then
      Logging.log "Sending_and_confirm_transaction transaction=%s signed=%s" (Transaction.to_yojson_string transaction) (SignedTransaction.to_yojson_string signed);
    let sender = transaction.tx_header.sender in
    let hash = signed.SignedTransaction.tx.hash in
    let open Lwt_exn in
    send_raw_transaction sender signed
    >>= (fun hash ->
      if ethereum_user_log then
        Logging.log "sent txhash=%s" (Digest.to_hex_string hash);
      return hash)
    >>= Ethereum_json_rpc.eth_get_transaction_receipt
    >>= (fun receipt ->
      if ethereum_user_log then
        Logging.log "got receipt %s" (option_to_yojson TransactionReceipt.to_yojson receipt |> string_of_yojson);
      return receipt)
    >>= (function
      | Some receipt -> check_transaction_receipt_status receipt
      | None ->
        if ethereum_user_log then
          Logging.log "send_and_confirm: None case";
        let nonce = transaction.tx_header.nonce in
        Ethereum_json_rpc.eth_get_transaction_count (sender, BlockParameter.Latest)
        >>= fun sender_nonce ->
        if ethereum_user_log then
          Logging.log "sender_nonce=%s nonce=%s" (Revision.to_string sender_nonce) (Revision.to_string nonce);
        if Nonce.(compare sender_nonce nonce > 0) then
          confirmed_or_known_issue sender hash
        else
          fail Still_pending)

    >>= check_receipt_sufficiently_confirmed

module TransactionTracker = struct
  open Lwter
  module Base = struct
    type context = unit
    module Key = struct
      [@@@warning "-39"]
      type t = { user : Address.t; revision : Revision.t }
      [@@deriving yojson, rlp]
      include (YojsonMarshalable(struct
                 type nonrec t = t
                 let yojsoning = {to_yojson;of_yojson}
                 let marshaling = marshaling_of_rlping rlping
               end): YojsonMarshalableS with type t := t)
    end
    type key = Key.t
    let key_prefix = "ETTT"
    module State = TransactionStatus
    type state = State.t
    let make_default_state = persistent_actor_no_default_state key_prefix Key.to_yojson_string
    (* TODO: inspection? cancellation? *)
    type t = Key.t * FinalTransactionStatus.t Lwt.t * unit Lwt.u
    let make_activity () key saving state =
      let (ready, notify_ready) = Lwt.task () in
      let rec update (status : TransactionStatus.t) =
        saving status >>= Db.committing >>= loop
      and continue (status : OngoingTransactionStatus.t) =
        TransactionStatus.Ongoing status |> update
      and finalize (status : FinalTransactionStatus.t) =
        if ethereum_user_log then
          Logging.log "Ethereum_user: beginning of finalize operation";
        TransactionStatus.Final status |> update
      and invalidate transaction_status error =
        if ethereum_user_log then
          Logging.log "Ethereum_user: beginning of invalidate operation";
        finalize (Failed (transaction_status, error))
      and loop (status : TransactionStatus.t) : FinalTransactionStatus.t Lwt.t =
        (*Logging.log "Stepping into %s" (TransactionStatus.to_yojson_string status);*)
        match status with
        | Ongoing ongoing ->
          (match ongoing with
           | Wanted {operation; value; gas_limit} ->
             make_signed_transaction key.Key.user operation value gas_limit
             >>= (function
               | Ok (t,c) -> OngoingTransactionStatus.Signed (t,c) |> continue
               | Error error -> invalidate ongoing error)
           | Signed (transaction, signed) ->
              if ethereum_user_log then
                Logging.log "Ethereum_User: Signed";
             (transaction, signed)
             |> Lwt_exn.(run_lwt
                           (retry ~retry_window:0.05 ~max_window:30.0 ~max_retries:None
                              (trying send_and_confirm_transaction
                               >>> (function
                                 | Ok receipt ->
                                   if ethereum_user_log then
                                     Logging.log "ETHUSR: TransactionTracker, Ok receipt A";
                                   return (Ok receipt)
                                 | Error NonceTooLow ->
                                   if ethereum_user_log then
                                     Logging.log "ETHUSR: TransactionTracker, Error NonceTooLow A";
                                   return (Error NonceTooLow)
                                 | Error TransactionRejected ->
                                   if ethereum_user_log then
                                     Logging.log "ETHUSR: TransactionTracker, Error TransactionRejected";
                                   Lwt_exn.return (Error TransactionRejected)
                                 | Error e ->
                                   if ethereum_user_log then
                                     Logging.log "ETHUSR: TransactionTracker, Error e";
                                   fail e))))
             >>= (function
               | Ok receipt ->
                 if ethereum_user_log then
                   Logging.log "ETHUSR: TransactionTracker, Ok receipt B";
                 FinalTransactionStatus.Confirmed (transaction, signed, receipt) |> finalize
               | Error NonceTooLow ->
                 if ethereum_user_log then
                   Logging.log "ETHUSR: TransactionTracker, Error NonceTooLow B";
                 OngoingTransactionStatus.Wanted (Transaction.pre_transaction transaction) |> continue
               | Error error ->
                 if ethereum_user_log then
                   Logging.log "ETHUSR: TransactionTracker, Error error";
                 invalidate ongoing error))
        | Final x -> return x in
      key, (ready >>= fun () -> loop state), notify_ready
  end
  include PersistentActivity(Base)
  module Key = Base.Key
end

module UserState = struct
  [@warning "-39"]
  type t =
    { address: Address.t
    ; transaction_counter: Revision.t
    ; ongoing_transactions: RevisionSet.t }
  [@@deriving lens { prefix=true }, yojson, rlp]
  let marshaling = marshaling_of_rlping rlping
  include (TrivialPersistable (struct
             type nonrec t = t
             let marshaling = marshaling
             let yojsoning = {to_yojson;of_yojson}
           end) : PersistableS with type t := t)
end

module UserAsyncAction = AsyncAction(UserState)

module User = struct
  open Lwter
  module Base = struct
    module Key = Address
    type key = Key.t
    let key_prefix = "ETUS"
    module State = UserState
    type state = UserState.t
    type t = state SimpleActor.t
    type context = Address.t -> t
    let make_default_state _get_user user =
      UserState.
        { address= user
        ; transaction_counter= Revision.zero
        ; ongoing_transactions= RevisionSet.empty }
    let rec resume_transaction get_user user revision =
      (*Logging.log "resume_transaction %s" TransactionTracker.(Key.to_yojson_string Key.{user;revision});*)
      let (_, promise, notify_ready) = TransactionTracker.get () {user; revision} in
      Lwt.wakeup_later notify_ready ();
      Lwt.async (fun () ->
        promise >>= fun _final_status ->
        (*Logging.log "SCHEDULING a remove_transaction %s %s" TransactionTracker.(Key.to_yojson_string Key.{user;revision}) FinalTransactionStatus.(to_yojson_string final_status);*)
        SimpleActor.action (get_user user)
          (remove_transaction get_user user) revision)

    and resume_transactions get_user user (state : State.t) =
      RevisionSet.min_elt_opt state.ongoing_transactions
      |> Option.iter (resume_transaction get_user user)
    and remove_transaction : context -> Address.t -> (Revision.t, unit) UserAsyncAction.arr =
      fun get_user user revision user_state ->
        (*Logging.log "REMOVE_TRANSACTION %s" TransactionTracker.(Key.to_yojson_string Key.{user;revision});*)
        let new_state =
          user_state
          |> Lens.modify UserState.lens_ongoing_transactions (RevisionSet.remove revision) in
        resume_transactions get_user user new_state;
        UserAsyncAction.return () new_state
    let make_activity get_user user saving state =
      (* TODO: use Db.with_transaction here, or have all callers use it appropriately *)
      let wrapper transform =
        Lwter.(transform >>> saving)
        (*Lwter.(
          fun state ->
          Logging.log "Actor for %s called, state %s" (Address.to_0x user) (State.to_yojson_string state);
          state |> transform >>= fun state ->
          Logging.log "Actor for %s returned, state %s; saving..." (Address.to_0x user) (State.to_yojson_string state);
          saving state >>= fun state ->
          Logging.log "Actor for %s saved %s" (Address.to_0x user) (State.to_yojson_string state);
          return state)*)
      in
      let actor = SimpleActor.make ~wrapper state in
      (* TODO: maybe just use Lwt_mvar.create state and leave it to users to transact on it ? *)
      resume_transactions get_user user state; (* TODO: pass the actor as context to that? *)
      actor
  end
  include PersistentActivity(Base)
  let rec get_user user = get get_user user
  let add_transaction : (OngoingTransactionStatus.t, TransactionTracker.t) UserAsyncAction.arr =
    fun transaction_status user_state ->
      let open Lwt in
      let user = user_state.UserState.address in
      let revision = user_state.transaction_counter in
      (*Logging.log "add_transaction %s %s" TransactionTracker.(Key.to_yojson_string Key.{user;revision}) (OngoingTransactionStatus.to_yojson_string transaction_status);*)
      TransactionTracker.(make () Key.{user; revision}
                            (Lwter.const (TransactionStatus.Ongoing transaction_status)))
      >>= fun tracker ->
      if RevisionSet.is_empty (UserState.lens_ongoing_transactions.get user_state) then
        Base.resume_transaction get_user user revision;
      (*Logging.log "ADD_TRANSACTION %s %s => %s" TransactionTracker.(Key.to_yojson_string Key.{user;revision}) (OngoingTransactionStatus.to_yojson_string transaction_status) (TransactionTracker.Key.to_yojson_string (match tracker with (key, _, _) -> key));*)
      UserAsyncAction.return tracker
        (user_state
         |> Lens.modify UserState.lens_transaction_counter Revision.(add one)
         |> Lens.modify UserState.lens_ongoing_transactions (RevisionSet.add revision))
end

let user_action : user:Address.t -> ('i, 'o) UserAsyncAction.arr -> ('i, 'o) Lwt_exn.arr =
  fun ~user action input ->
    SimpleActor.action (User.get_user user) action input

let add_ongoing_transaction : user:Address.t -> (OngoingTransactionStatus.t, TransactionTracker.t) Lwt_exn.arr =
  fun ~user status ->
  user_action ~user User.add_transaction status

let issue_pre_transaction : Address.t -> (PreTransaction.t, TransactionTracker.t) Lwt_exn.arr =
  fun sender pre ->
  if ethereum_user_log then
    Logging.log "ETHUSR: beginning of issue_pre_transaction";
  OngoingTransactionStatus.Wanted pre |> add_ongoing_transaction ~user:sender

let track_transaction : (TransactionTracker.t, FinalTransactionStatus.t) Lwter.arr =
  fun (_key_t, promise, _unit_lwt_u) ->
  if ethereum_user_log then
    Logging.log "ETHUSR: track_transaction, returning promise";
  promise

let check_transaction_confirmed : (FinalTransactionStatus.t, Transaction.t * SignedTransaction.t * TransactionReceipt.t) Lwt_exn.arr =
  fun final_transaction_status ->
  if ethereum_user_log then
    Logging.log "ETHUSR: Beginning of check_transaction_confirmed";
  match final_transaction_status with
  | FinalTransactionStatus.Confirmed (t, s, r) ->
     if ethereum_user_log then
       Logging.log "ETHUSR: check_transaction_confirmed, Case Confirmed";
     return (t, s, r)
  | FinalTransactionStatus.Failed (t, e) ->
     if ethereum_user_log then
       Logging.log "ETHUSR: check_transaction_confirmed, Case Failed e=%s" (Printexc.to_string e);
     fail (TransactionFailed (t, e))

let confirm_pre_transaction : Address.t -> (PreTransaction.t, Transaction.t * SignedTransaction.t * TransactionReceipt.t) Lwt_exn.arr =
  fun address ->
  issue_pre_transaction address
  >>> of_lwt track_transaction
  >>> check_transaction_confirmed

(** Gas used for a transfer transaction. Hardcoded value defined in the Yellowpaper. *)
let transfer_gas_used = TokenAmount.of_int 21000

(* Used only in tests *)
let transfer_tokens ~recipient value =
  PreTransaction.{operation=(Operation.TransferTokens recipient); value; gas_limit=transfer_gas_used}

let make_pre_transaction ~sender operation ?gas_limit ~value : PreTransaction.t Lwt_exn.t =
  if ethereum_user_log then
    Logging.log "ETHUSR: Beginning of make_pre_transaction";
  (match gas_limit with
   | Some x -> return x
   | None -> eth_estimate_gas (TransactionParameters.of_operation sender operation value))
  >>= fun gas_limit ->
  if ethereum_user_log then
    Logging.log "ETHUSR: make_pre_transaction gas_limit=%s value=%s" (TokenAmount.to_string gas_limit) (TokenAmount.to_string value);
  (* TODO: The multiplication by 2 is a hack that needs to be addressed *)
  let gas_limit_n_fold = (TokenAmount.mul (TokenAmount.of_int 2) gas_limit) in
  if ethereum_user_log then
    Logging.log "ETHUSR: gas_limit_n_fold=%s" (TokenAmount.to_string gas_limit_n_fold);
  return PreTransaction.{operation; value; gas_limit=gas_limit_n_fold}

let create_contract ~sender ~code ?gas_limit ~value =
  make_pre_transaction ~sender (Operation.CreateContract code) ?gas_limit ~value

let call_function ~sender ~contract ~call ?gas_limit ~value =
  make_pre_transaction ~sender (Operation.CallFunction (contract, call)) ?gas_limit ~value

let get_status_receipt : TransactionReceipt.t -> bool =
  fun tr -> TokenAmount.equal tr.status TokenAmount.one

let post_operation_kernel : Ethereum_chain.Operation.t -> Address.t -> TokenAmount.t -> TransactionReceipt.t Lwt_exn.t =
  fun operation sender value ->
  let gas_limit_val = None in (* Some kind of arbitrary choice *)
  if ethereum_user_log then
    Logging.log "post_operation_general_kernel : before make_pre_transaction";
  make_pre_transaction ~sender operation ?gas_limit:gas_limit_val ~value
  >>= fun x_pretrans ->
  add_ongoing_transaction ~user:sender (Wanted x_pretrans)
  >>= fun (tracker_key, _, _) ->
  let (_, promise, _) = TransactionTracker.get () tracker_key in
  (Lwt.bind promise (function
  | FinalTransactionStatus.Failed (_, error) ->
     fail error (* bork "Cannot match this" *)
  | FinalTransactionStatus.Confirmed (_transaction, _signed, receipt) ->
     if ethereum_user_log then
       Logging.log "transaction status=%B" (get_status_receipt receipt);
     Lwt_exn.return receipt))

let post_operation : operation:Ethereum_chain.Operation.t -> sender:Address.t -> value:TokenAmount.t -> TransactionReceipt.t Lwt_exn.t =
  fun ~operation ~sender ~value ->
  let rec submit_operation : unit -> TransactionReceipt.t Lwt_exn.t =
    fun () ->
    Lwt_exn.bind (post_operation_kernel operation sender value)
      (fun ereceipt ->
        (if get_status_receipt ereceipt then
           Lwt_exn.return ereceipt
         else
           (if ethereum_user_log then
              Logging.log "receipt is not true, ereceipt=%s" (TokenAmount.to_string ereceipt.status);
            Lwt_exn.bind (Ethereum_watch.sleep_delay_exn 1.0) (fun () -> submit_operation ()))
        )
      ) in
  submit_operation ()

(*
let Option.default : 'a -> 'a option -> 'a =
  fun val_return val_opt ->
  match val_opt with
  | None -> val_return
  | Some x -> x
 *)

module Test = struct
  open Lwt_exn
  (* open Hex *)
  open Digesting
  open Signing.Test
  (* open Ethereum_abi *)

  let prefunded_address_mutex = Lwt_mutex.create ()
  let prefunded_address = ref None

  let get_prefunded_address () =
    Lwt_mutex.with_lock prefunded_address_mutex
      (fun () ->
         match !prefunded_address with
         | Some x -> return x
         | None ->
           Ethereum_transaction.get_first_account ()
           >>= fun address ->
           register_keypair "Croesus"
             {(keypair_of_0x (* KLUGE: Unrelated keypair, wherein we override the address *)
                 "0xd56984dc083d769701714eeb1d4c47a454255a3bbc3e9f4484208c52bda3b64e"
                 "0x0423a7cd9a03fa9c5857e514ae5acb18ca91e07d69453ed85136ea6a00361067b860a5b20f1153333aef2d1ba13b1d7a52de2869d1f62371bf81bf803c21c67aca"
                 "") with address};
           prefunded_address := Some address;
           return address)

  let display_balance display address balance =
    display
      (nicknamed_string_of_address address)
      (TokenAmount.to_0x balance)

  let ensure_address_prefunded prefunded_address amount address =
    let open TokenAmount in
    eth_get_balance (address, BlockParameter.Pending)
    >>= fun balance ->
    Logging.log "address=%s" (nicknamed_string_of_address address);
    Logging.log "Now working something balance=%s" (TokenAmount.to_string balance);
    if compare balance amount >= 0 then
      display_balance (printf "Account %s contains %s wei.\n") address balance
    else
      begin
        display_balance (printf "Account %s only contains %s wei. Funding.\n") address balance
        >>= fun () ->
        Logging.log "Before transfer_tokens";
        transfer_tokens ~recipient:address (sub amount balance)
        |> confirm_pre_transaction prefunded_address
        >>= fun _ ->
        Logging.log "Before call to eth_get_balance";
        eth_get_balance (address, BlockParameter.Pending)
        >>= fun balance -> display_balance (printf "Account %s now contains %s wei.\n") address balance
      end

  (* create accounts, fund them *)
  let ensure_test_account
        ?(min_balance=TokenAmount.of_string "1000000000000000000") prefunded_address (nickname, keypair) =
    register_keypair nickname keypair;
    Ethereum_transaction.ensure_private_key keypair
    >>= ensure_address_prefunded prefunded_address min_balance

  let fund_accounts ?min_balance () =
    get_prefunded_address ()
    >>= fun prefunded_address ->
    list_iter_s (ensure_test_account ?min_balance prefunded_address)
      [("Alice", alice_keys); ("Bob", bob_keys); ("Trent", trent_keys)]


  (** Has a transaction given by a hash successfully executed,
      and does the Ethereum network report information that match what we expected? *)
  [@@@warning "-32"]
  let check_transaction_execution (transaction_hash: digest) (transaction: Transaction.t) : bool Lwt_exn.t =
    eth_get_transaction_receipt transaction_hash
    >>= arr (function
            | Some TransactionReceipt.{status} -> TokenAmount.sign status = 1
            | _ -> false)
    >>= fun executed ->
    assert executed;
    Ethereum_json_rpc.eth_get_transaction_by_hash transaction_hash
    >>= fun info ->
    let tx_header = transaction.tx_header in
    assert (info.from = Some tx_header.sender);
    assert (info.nonce = tx_header.nonce);
    assert (TokenAmount.compare info.gas tx_header.gas_limit <= 0);
    assert (TokenAmount.compare info.gas_price tx_header.gas_price <= 0);
    assert (TokenAmount.compare info.value tx_header.value = 0);
    assert (match transaction.operation with (* operation-specific checks *)
            | TransferTokens recipient_address -> info.to_ = Some recipient_address
            | CreateContract data -> info.input = data
            | CallFunction (contract_address, call_input) ->
               info.to_ = Some contract_address && info.input = call_input) ;
    return true

  (* TODO re-enable
  let%test "Ethereum-testnet-transfer" =
    Logging.log "\nTEST: Ethereum-testnet-transfer\n";
    Lwt_exn.run
      (fun () ->
        of_lwt Db.open_connection "unit_test_db"
        >>= fun () ->
        get_prefunded_address ()
        >>= fun croesus ->
        transfer_tokens ~recipient:alice_address (TokenAmount.of_int 1000000000)
        |> confirm_pre_transaction croesus
        >>= fun (transaction, _signed_tx, TransactionReceipt.{transaction_hash}) ->
        check_transaction_execution transaction_hash transaction)
      ()
      *)

  (*
  let test_contract_code () =
    "contracts/test/HelloWorld.bin"
    |> Config.get_build_filename
    |> read_file
    |> parse_hex_string
    |> Bytes.of_string

  let list_only_element = function
    | [x] -> x
    | _ -> Lib.bork "list isn't a singleton"
  *)

(* TODO re-enable
  let%test "Ethereum-testnet-contract-failure" =
    (Logging.log "\nTEST: contract-failure-on-Ethereum-testnet!!\n";
    Lwt_exn.run
     (fun () ->
        of_lwt Db.open_connection "unit_test_db"
        >>= fun () ->
        get_prefunded_address ()
        >>= fun sender ->
        let code = test_contract_code () in
        create_contract ~sender ~code
        (* Failure due to bogus gas_limit *)
        ~gas_limit:(TokenAmount.of_int 100000) TokenAmount.zero
        >>= (trying (confirm_pre_transaction sender)
        >>> (function
        | Ok _    -> return false
        (* TransactionTracker returns a FinalTransactionStatus after TransactionRejected is thrown *)
        | Error TransactionFailed (_, _) -> return true
        | Error _ -> return false))))
    ()
 *)

  (* TODO re-enable
  let%test "Ethereum-testnet-contract-success" =
    Logging.log "\nTEST: contract-success-on-Ethereum-testnet!!\n";
    Logging.log "SUBTEST: create the contract\n";
    Lwt_exn.run
      (fun () ->
        of_lwt Db.open_connection "unit_test_db"
        >>= fun () ->
        get_prefunded_address ()
        >>= fun sender ->
        let code = test_contract_code () in
        create_contract ~sender ~code ?gas_limit:None ~value:TokenAmount.zero
        >>= confirm_pre_transaction sender
        >>= (function | (_, _, {contract_address=(Some contract)}) -> return contract
                      | _ -> bork "Failed to create contract")
        >>= fun contract ->
        Logging.log "SUBTEST: call contract function hello with no argument\n";
        let call = encode_function_call { function_name = "hello"; parameters = [] } in
        call_function ~sender ~contract ~call  ~value:TokenAmount.zero
        >>= confirm_pre_transaction sender
        >>= fun (tx, _, {block_number}) ->
        eth_call (CallParameters.of_transaction tx, Block_number Revision.(sub block_number one))
        >>= fun data ->
        Logging.log "hello replied: %s\n" (unparse_0x_data data);
        Logging.log "SUBTEST: call contract function mul42 with one number argument\n";
        let call = encode_function_call
                     { function_name = "mul42"; parameters = [ abi_uint (Z.of_int 47) ] } in
         call_function ~sender ~contract ~call ~value:TokenAmount.zero
         >>= confirm_pre_transaction sender
         >>= fun (tx, _, {block_number}) ->
         eth_call (CallParameters.of_transaction tx, Block_number Revision.(sub block_number one))
         >>= fun data ->
         Logging.log "mul42 replied: %s\n" (unparse_0x_data data);
         let mul42_encoding =
           let tuple_value, tuple_ty = abi_tuple_of_abi_values [abi_uint (Z.of_int 1974)] in
           encode_abi_value tuple_value tuple_ty in
         assert (data = Bytes.to_string mul42_encoding);

         Logging.log "SUBTEST: call contract function greetings with one string argument\n";
         let call = encode_function_call
                      { function_name = "greetings";
                        parameters = [ abi_string "Croesus" ] } in
         call_function ~sender ~contract ~call ~value:TokenAmount.zero
         >>= confirm_pre_transaction sender
         >>= fun (tx, _, {block_number; logs}) ->
         let receipt_log = list_only_element logs in
         let log_contract_address = receipt_log.address in
         assert (log_contract_address = contract) ;
         let topic_event = list_only_element receipt_log.topics in
         let greetings_croesus = (String_value "Greetings, Croesus", String) in
         let greetings_encoding =
           let tuple_value, tuple_ty = abi_tuple_of_abi_values [greetings_croesus] in
           encode_abi_value tuple_value tuple_ty in
         Logging.log "expecting:        %s\n" (unparse_0x_bytes greetings_encoding);
         let function_signature = {function_name= "greetingsEvent"; parameters= [greetings_croesus]} in
         let function_signature_digest = function_signature |> function_signature_digest in
         assert (Digest.equal topic_event function_signature_digest) ;
         (* the log data is the encoding of the parameter passed to the event *)
         let data = receipt_log.data in
         Logging.log "receipt log data: %s\n" (unparse_0x_bytes data);
         eth_call (CallParameters.of_transaction tx, Block_number Revision.(sub block_number one))
         >>= fun result ->
         Logging.log "computed reply:   %s\n" (unparse_0x_data result);
         assert (result = Bytes.to_string data);
         assert (data = greetings_encoding);
         (* TODO: add a stateful function, and check the behavior of eth_call wrt block_number *)
         return true)
     ()
     *)
end

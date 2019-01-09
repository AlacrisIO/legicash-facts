open Legilogic_lib
open Lib
open Yojsoning
open Marshaling
open Tag
open Persisting
open Types
open Signing
open Action
open Lwt_exn
open Json_rpc
open Trie

open Ethereum_chain
open Ethereum_json_rpc
open Side_chain_server_config

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
    | Confirmed of Transaction.t * Confirmation.t
    | Failed of OngoingTransactionStatus.t * exn
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
  let pre_transaction : t -> PreTransaction.t = function
    | Confirmed (tx, _) -> Transaction.pre_transaction tx
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

(** TODO: check receipt status!!! *)
let confirmation_of_transaction_receipt =
  function
    TransactionReceipt.{transaction_hash ; transaction_index; block_number; block_hash} ->
    Confirmation.{transaction_hash; transaction_index; block_number; block_hash}

(** Number of blocks required for a transaction to be considered confirmed *)
(* The value should be set in side_chain_server_config.json file. 
   For production, use 100, not 0. Put it as configuration file on input *)
(* let block_depth_for_confirmation = Revision.of_int 0 *)
let block_depth_for_confirmation = Side_chain_server_config.minNbBlockConfirm

exception Still_pending
exception TransactionFailed of OngoingTransactionStatus.t * exn
exception NonceTooLow

let check_confirmation_deep_enough (confirmation : Confirmation.t) : Confirmation.t t =
  Logging.log "ethereum_user : check_confirmation_deep_enough";
  (*Logging.log "check_confirmation_deep_enough %s" (Confirmation.to_yojson_string confirmation);*)
  eth_block_number ()
  >>= fun block_number ->
  if Revision.(is_add_valid confirmation.block_number block_depth_for_confirmation
               && compare block_number (add confirmation.block_number block_depth_for_confirmation)
                  >= 0) then
    return confirmation
  else
    fail Still_pending

let check_confirmation_deep_enough_bool (confirmation : Confirmation.t) : bool Lwt_exn.t =
  Logging.log "ethereum_user : check_confirmation_deep_enough";
  (*Logging.log "check_confirmation_deep_enough %s" (Confirmation.to_yojson_string confirmation);*)
  eth_block_number ()
  >>= fun block_number ->
  if Revision.(is_add_valid confirmation.block_number block_depth_for_confirmation
               && compare block_number (add confirmation.block_number block_depth_for_confirmation)
                  >= 0) then
    return true
  else
    return false


  
type nonce_operation = Peek | Next | Reset [@@deriving yojson]

module NonceTracker = struct
  open Lwter
  module Base = struct
    type context = unit
    module Key = Address
    type key = Key.t
    let key_prefix = "ETNT"
    module State = TrivialPersistable(struct
        type t = Nonce.t option [@@deriving yojson]
        let yojsoning = {to_yojson;of_yojson}
        let marshaling = option_marshaling Nonce.marshaling
      end)
    type state = State.t
    (* zero is often wrong, but just let it fail and resynchronize *)
    let make_default_state _ _ = None
    type t = (nonce_operation, Nonce.t) Lwter.arr
    let make_activity () address saving =
      Logging.log "ethereum_user : make_activity";
      sequentialize
        (fun op state ->
           let reset () =
             Lwt_exn.run_lwt
               (retry ~retry_window:0.01 ~max_window:5.0 ~max_retries:None
                  Ethereum_json_rpc.eth_get_transaction_count)
               (address, BlockParameter.Latest) in
           let continue result state =
             saving state >>= const (result, state) in
           let next nonce = continue nonce (Some Nonce.(add one nonce)) in
           (match (op, state) with
            | (Reset, _) ->
              continue Nonce.zero None
            | (Peek, None) ->
              reset () >>= fun nonce -> continue nonce (Some nonce)
            | (Peek, Some nonce) ->
              return (nonce, Some nonce)
            | (Next, None) ->
              reset () >>= next
            | (Next, Some nonce) -> next nonce)
           (*>>= fun (result, new_state) -> Logging.log "NonceTracker %s %s %s => %s %s" (Address.to_0x address) (op |> nonce_operation_to_yojson |> string_of_yojson) (State.to_yojson_string state) (Revision.to_0x result) (State.to_yojson_string new_state) ; return (result, new_state)*))
  end
  include PersistentActivity(Base)
  module State = Base.State
  let reset address = get () address Reset >>= const ()
  let peek address = get () address Peek
  let next address = get () address Next
end

let make_tx_header (sender, value, gas_limit) : TxHeader.t Lwt_exn.t =
  Logging.log "ethereum_user : make_tx_header";
  (* TODO: get gas price and nonce from geth *)
  eth_gas_price () >>= fun gas_price ->
  of_lwt NonceTracker.next sender >>= fun nonce ->
  (*Logging.log "make_tx_header sender=%s value=%s gas_limit=%s gas_price=%s nonce=%s" (Address.to_0x sender) (TokenAmount.to_string value) (TokenAmount.to_string gas_limit) (TokenAmount.to_string gas_price) (Nonce.to_0x nonce);*)
  return TxHeader.{sender; nonce; gas_price; gas_limit; value}

exception Missing_password

let sign_transaction : (Transaction.t, Transaction.t * SignedTransaction.t) Lwt_exn.arr =
  fun transaction ->
  Logging.log "ethereum_user : sign_transaction";
  let address = transaction.tx_header.sender in
  (try return (keypair_of_address address).password with
   | Not_found ->
      Logging.log "Couldn't find registered keypair for %s" (nicknamed_string_of_address address);
      fail Missing_password)
  >>= fun password -> personal_sign_transaction (transaction_to_parameters transaction, password)
  >>= fun signed -> return (transaction, signed)

(** Prepare a signed transaction, that you may later issue onto Ethereum network,
    from given address, with given operation, value and gas_limit *)
let make_signed_transaction (sender : Address.t) (operation : Operation.t) (value : TokenAmount.t) (gas_limit : TokenAmount.t) : (Transaction.t * SignedTransaction.t) Lwt_exn.t =
  Logging.log "ethereum_user : make_signed_transaction";
  make_tx_header (sender, value, gas_limit)
  >>= fun tx_header ->
  sign_transaction Transaction.{tx_header; operation}

(* TODO: move as many functions as possible ethereum_transaction ? *)

let nonce_too_low address =
  Logging.log "nonce too low for %s" (nicknamed_string_of_address address);
  (* TODO: Send Notification to end-user via UI! *)
  Lwter.(NonceTracker.reset address >>= const (Error NonceTooLow))

let confirmed_or_nonce_too_low : Address.t -> (Digest.t, Confirmation.t) Lwt_exn.arr =
  fun sender hash ->
  Logging.log "ethereum_user : confirmed_or_nonce_too_low";
  let open Lwter in
  Ethereum_json_rpc.eth_get_transaction_receipt hash
  >>= function
  | Ok None -> nonce_too_low sender
  | Ok (Some receipt) -> confirmation_of_transaction_receipt receipt |> Lwt_exn.return
  | Error e -> Lwt_exn.fail e

let send_raw_transaction : Address.t -> (SignedTransaction.t, Digest.t) Lwt_exn.arr =
  fun sender signed ->
  Logging.log "ethereum_user : send_raw_transaction";
    (*Logging.log "send_raw_transaction %s" (SignedTransaction.to_yojson_string signed);*)
    let open Lwter in
    match signed with
    | SignedTransaction.{raw;tx={hash}} ->
      (eth_send_raw_transaction raw
       >>= function
       | Error (Rpc_error {code= -32000; message="nonce too low"}) ->
         Lwt_exn.(confirmed_or_nonce_too_low sender hash >>= const hash)
       | Error (Rpc_error {code= -32000; message})
         when message = "known transaction: " ^ Digest.to_hex_string hash ->
         Lwt_exn.return hash
       | Error e -> Lwt_exn.fail e
       | Ok transaction_hash ->
         if transaction_hash = hash then
           Lwt_exn.return hash
         else
           Lwt_exn.bork "eth_send_raw_transaction: invalid hash %s instead of %s" (Digest.to_0x transaction_hash) (Digest.to_0x hash))

(** Wait until a transaction has been confirmed by the main chain.
    TODO: understand why an error in a previous version of this code got eaten silently,
    instead of logged and reported, causing a deadlock. *)
let send_and_confirm_transaction :
  (Transaction.t * SignedTransaction.t, Confirmation.t) Lwt_exn.arr =
  fun (transaction, signed) ->
  Logging.log "ethereum_user : send_and_confirm_transaction";
    (*Logging.log "Sending and confirming %s %s" (Transaction.to_yojson_string transaction) (SignedTransaction.to_yojson_string signed);*)
    let sender = transaction.tx_header.sender in
    let hash = signed.SignedTransaction.tx.hash in
    let open Lwt_exn in
    send_raw_transaction sender signed
    (*>>= (fun hash -> Logging.log "sent txhash=%s" (Digest.to_hex_string hash); return hash)*)
    >>= Ethereum_json_rpc.eth_get_transaction_receipt
    (*>>= (fun receipt -> Logging.log "got receipt %s" (option_to_yojson TransactionReceipt.to_yojson receipt |> string_of_yojson); return receipt)*)
    >>= (function
      | None ->
        let nonce = transaction.tx_header.nonce in
        Ethereum_json_rpc.eth_get_transaction_count (sender, BlockParameter.Latest)
        >>= fun sender_nonce ->
        if Nonce.(compare sender_nonce nonce > 0) then
          confirmed_or_nonce_too_low sender hash
        else
          fail Still_pending
      | Some receipt -> confirmation_of_transaction_receipt receipt |> return)
    >>= check_confirmation_deep_enough

module TransactionTracker = struct
  open Lwter
  module Base = struct
    type context = unit
    module Key = struct
      [@@@warning "-39"]
      type t= { user : Address.t; revision : Revision.t } [@@deriving yojson]
      include (YojsonMarshalable(struct
                 type nonrec t = t
                 let yojsoning = {to_yojson;of_yojson}
                 let marshaling = marshaling2
                                    (fun {user;revision} -> user,revision)
                                    (fun user revision -> {user;revision})
                                    Address.marshaling Revision.marshaling
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
        TransactionStatus.Final status |> update
      and invalidate transaction_status error =
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
             (transaction, signed)
             |> Lwt_exn.(run_lwt
                           (retry ~retry_window:0.05 ~max_window:30.0 ~max_retries:None
                              (trying send_and_confirm_transaction
                               >>> (function
                                 | Ok confirmation ->
                                   return (Ok confirmation)
                                 | Error NonceTooLow ->
                                   return (Error NonceTooLow)
                                 | Error e -> fail e))))
             >>= (function
               | Ok confirmation ->
                 FinalTransactionStatus.Confirmed (transaction, confirmation) |> finalize
               | Error NonceTooLow ->
                 OngoingTransactionStatus.Wanted (Transaction.pre_transaction transaction) |> continue
               | Error error -> invalidate ongoing error))
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
  [@@deriving lens { prefix=true }, yojson]
  let marshaling =
    marshaling3
      (fun { address; transaction_counter; ongoing_transactions} ->
         address, transaction_counter, RevisionSet.elements ongoing_transactions)
      (fun address transaction_counter ongoing_transactions_keys ->
         {address; transaction_counter;
          ongoing_transactions= RevisionSet.of_list ongoing_transactions_keys})
      Address.marshaling Revision.marshaling (list_marshaling Revision.marshaling)
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

let user_action : Address.t -> ('i, 'o) UserAsyncAction.arr -> ('i, 'o) Lwt_exn.arr =
  fun address action input ->
    SimpleActor.action (User.get_user address) action input

let add_ongoing_transaction user status =
  user_action user User.add_transaction status

let issue_pre_transaction : Address.t -> (PreTransaction.t, TransactionTracker.t) Lwt_exn.arr =
  fun sender pre -> OngoingTransactionStatus.Wanted pre |> add_ongoing_transaction sender

let track_transaction : (TransactionTracker.t, FinalTransactionStatus.t) Lwter.arr =
  fun (_, promise, _) -> promise

let check_transaction_confirmed :
  (FinalTransactionStatus.t, Transaction.t * Confirmation.t) Lwt_exn.arr
  = function
    | FinalTransactionStatus.Confirmed (t, c) -> return (t, c)
    | FinalTransactionStatus.Failed (t, e) -> fail (TransactionFailed (t, e))

let confirm_pre_transaction (address : Address.t) : (PreTransaction.t, Transaction.t * Confirmation.t) Lwt_exn.arr = 
  issue_pre_transaction address
  >>> of_lwt track_transaction
  >>> check_transaction_confirmed

(* Used only in tests *)
let transfer_tokens ~recipient value =
  PreTransaction.{operation=(Operation.TransferTokens recipient); value; gas_limit=Side_chain_server_config.transfer_gas_limit}

let make_pre_transaction ~sender (operation : Operation.t) ?gas_limit (value : TokenAmount.t) : PreTransaction.t Lwt_exn.t =
  Logging.log "ethereum_user : make_pre_transaction";
  (match gas_limit with
   | Some x -> return x
   | None -> eth_estimate_gas (operation_to_parameters sender operation))
  >>= fun gas_limit ->
  Logging.log "make_pre_transaction : gas_limit=%s" (TokenAmount.to_string gas_limit);
  return PreTransaction.{operation; value; gas_limit}

let create_contract ~sender ~code ?gas_limit value =
  make_pre_transaction ~sender (Operation.CreateContract code) ?gas_limit value

let call_function ~sender ~contract ~call ?gas_limit value =
  make_pre_transaction ~sender (Operation.CallFunction (contract, call)) ?gas_limit value

module Test = struct
  open Hex
  open Digesting
  open Signing.Test

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
    let open Lwt_exn in
    let open TokenAmount in
    eth_get_balance (address, BlockParameter.Pending)
    >>= fun balance ->
    if compare balance amount >= 0 then
      display_balance (printf "Account %s contains %s wei.\n") address balance
    else
      begin
        display_balance (printf "Account %s only contains %s wei. Funding.\n") address balance
        >>= fun () ->
        transfer_tokens ~recipient:address (sub amount balance)
        |> confirm_pre_transaction prefunded_address
        >>= fun _ -> eth_get_balance (address, BlockParameter.Pending)
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

  let%test "transfer-on-Ethereum-testnet" =
    Lwt_exn.run
      (fun () ->
         of_lwt Db.open_connection "unit_test_db"
         >>= fun () ->
         get_prefunded_address ()
         >>= fun sender ->
         Ethereum_json_rpc.personal_new_account ""
         >>= fun recipient ->
         (* we don't check opening balance, which may be too large to parse *)
         let transfer_amount = 22 in
         transfer_tokens ~recipient (TokenAmount.of_int transfer_amount)
         |> confirm_pre_transaction sender
         >>= fun (transaction, Confirmation.{transaction_hash}) ->
         Ethereum_transaction.transaction_execution_matches_transaction transaction_hash transaction)
      ()

  let%test "create-contract-on-Ethereum-testnet" =
    Lwt_exn.run
      (fun () ->
         of_lwt Db.open_connection "unit_test_db"
         >>= fun () ->
         get_prefunded_address ()
         >>= fun sender ->
         (* NB: this contract is bogus enough that eth_estimateGas yields bad answers.
            TODO: Check whether the contract actually succeeds? *)
         create_contract ~sender ~code:(Bytes.create 128)
           ~gas_limit:(TokenAmount.of_int 100000)
           TokenAmount.zero
         >>= confirm_pre_transaction sender
         >>= fun (tx, {transaction_hash}) ->
         Ethereum_transaction.transaction_execution_matches_transaction transaction_hash tx)
      ()

  let%test "call-contract-on-Ethereum-testnet" =
    let open Ethereum_chain in
    Lwt_exn.run
      (fun () ->
         of_lwt Db.open_connection "unit_test_db"
         >>= fun () ->
         get_prefunded_address ()
         >>= fun sender ->
         (* for CallFunction:

            address should be a valid contract address
            for testing, it's a dummy address

            the bytes are a 4-byte prefix of the Keccak256 hash of the encoding of a method
            signature, followed by the encoding of the method parameters, as described at:

            https://solidity.readthedocs.io/en/develop/abi-spec.html

            This data tells the EVM which method to call, with what arguments, in the contract

            in this test, we just use a dummy hash to represent all of that
         *)
         let hashed = digest_of_string "some arbitrary string" in
         call_function ~sender
           ~contract:(Address.of_0x "0x2B1c40cD23AAB27F59f7874A1F454748B004C4D8")
           ~call:(Bytes.of_string (Digest.to_big_endian_bits hashed))
           TokenAmount.zero
         >>= confirm_pre_transaction sender
         >>= fun (transaction, Confirmation.{transaction_hash}) ->
         Ethereum_transaction.transaction_execution_matches_transaction transaction_hash transaction)
      ()

  let%test "hello-solidity" =
    let open Ethereum_abi in
    Lwt_exn.run
      (fun () ->
         of_lwt Db.open_connection "unit_test_db"
         >>= fun () ->
         (* code is result of running "solc --bin hello.sol", and prepending "0x" *)
         let code = parse_0x_bytes "0x608060405234801561001057600080fd5b506101a7806100206000396000f300608060405260043610610041576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff16806339a7aa4814610046575b600080fd5b34801561005257600080fd5b5061005b6100d6565b6040518080602001828103825283818151815260200191508051906020019080838360005b8381101561009b578082015181840152602081019050610080565b50505050905090810190601f1680156100c85780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b60607fcde5c32c0a45fd8aa4b65ea8003fc9da9acd5e2c6c24a9fcce6ab79cabbd912260405180806020018281038252600d8152602001807f48656c6c6f2c20776f726c64210000000000000000000000000000000000000081525060200191505060405180910390a16040805190810160405280600881526020017f476f6f64627965210000000000000000000000000000000000000000000000008152509050905600a165627a7a7230582024923934849b0e74a5091ac4b5c65d9b3b93d74726aff49fd5763bc136dac5c60029" in
         (* create a contract using "hello, world" EVM code *)
         get_prefunded_address ()
         >>= fun sender ->
         (* a valid contract contains compiled EVM code
            for testing, we just use a buffer with arbitrary contents
         *)
         create_contract ~sender ~code TokenAmount.zero
         >>= confirm_pre_transaction sender
         >>= fun (transaction, Confirmation.{transaction_hash}) ->
         Ethereum_transaction.transaction_execution_matches_transaction transaction_hash transaction
         >>= fun matches ->
         assert matches ;
         (* call the contract we've created *)
         eth_get_transaction_receipt transaction_hash
         >>= arr Option.get
         >>= fun receipt ->
         let contract = Option.get receipt.TransactionReceipt.contract_address in
         let call = encode_function_call {function_name= "printHelloWorld"; parameters= []} in
         call_function ~sender ~contract ~call TokenAmount.zero
         >>= confirm_pre_transaction sender
         >>= fun (_transaction, Confirmation.{transaction_hash}) ->
         eth_get_transaction_receipt transaction_hash
         >>= arr Option.get
         >>= fun receipt1 ->
         (* verify that we called "printHelloWorld" *)
         let receipt_log = match receipt1.TransactionReceipt.logs with [x] -> x | _ -> Lib.bork "blah" in
         (* we called the right contract *)
         let log_contract_address = receipt_log.LogObject.address in
         assert (log_contract_address = contract) ;
         (* we called the right function within the contract *)
         let topic_event = match receipt_log.LogObject.topics with [x] -> x | _ -> Lib.bork "bloh" in
         let hello_world = (String_value "Hello, world!", String) in
         let function_signature = {function_name= "showResult"; parameters= [hello_world]} in
         let function_signature_digest = function_signature |> function_signature_digest in
         assert (Digest.equal topic_event function_signature_digest) ;
         (* the log data is the encoding of the parameter passed to the event *)
         let data = receipt_log.data in
         let hello_encoding =
           let tuple_value, tuple_ty = abi_tuple_of_abi_values [hello_world] in
           unparse_0x_bytes (encode_abi_value tuple_value tuple_ty)
         in
         return (unparse_0x_bytes data = hello_encoding))
      ()

  let%test "fallback-with-operator-address" =
    (* we call the fallback function in a contract by using the operator address as "code" *)
    let open Ethereum_abi in
    Lwt_exn.run
      (fun () ->
         of_lwt Db.open_connection "unit_test_db"
         >>= fun () ->
         (* code is result of running "solc --bin operator-fallback.sol", and prepending "0x" *)
         let code =
           "0x608060405234801561001057600080fd5b50610108806100206000396000f300608060405260146000369050141515601657600080fd5b7facfada45e09e5bb4c2c456febe99efe38be8bfc67a25cccdbb4c93ec56f661a560716000368080601f01602080910402602001604051908101604052809392919081815260200183838082843782019150505050505060bc565b34604051808373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018281526020019250505060405180910390a1005b6000602082015190506c01000000000000000000000000810490509190505600a165627a7a7230582098fc57c39988f3dcf9f7168b876b9f491273775ea6b44db8cb9483966fa1adc10029"
         in
         let code = parse_0x_bytes code in
         (* create the contract *)
         get_prefunded_address ()
         >>= fun sender ->
         create_contract ~sender ~code TokenAmount.zero
         >>= confirm_pre_transaction sender
         >>= fun (transaction, Confirmation.{transaction_hash}) ->
         eth_get_transaction_receipt transaction_hash
         >>= arr Option.get
         >>= fun receipt ->
         let contract = Option.get receipt.contract_address in
         (* check balance of new contract *)
         eth_get_balance (contract, Latest)
         >>= fun starting_balance ->
         assert (TokenAmount.sign starting_balance = 0) ;
         Ethereum_transaction.transaction_execution_matches_transaction transaction_hash transaction
         >>= fun matches ->
         assert matches;
         (* Call the fallback in the contract we've created.
            It bypasses the regular ABI to access this address directly. *)
         let amount_to_transfer = TokenAmount.of_int 93490 in
         let operator_address = Address.of_0x "0x9797809415e4b8efea0963e362ff68b9d98f9e00" in
         let call = Ethereum_util.bytes_of_address operator_address in
         call_function ~sender ~contract ~call amount_to_transfer
         >>= confirm_pre_transaction sender
         >>= fun (_transaction, Confirmation.{transaction_hash}) ->
         eth_get_transaction_receipt transaction_hash
         >>= arr Option.get
         >>= fun receipt1 ->
         (* verify that we called the fallback *)
         let log = match receipt1.logs with [x] -> x | _ -> Lib.bork "bloh" in
         (* the log is for this contract *)
         let receipt_address = log.address in
         assert (receipt_address = contract) ;
         (* we saw the expected event *)
         let logged_event = match log.LogObject.topics with [x] -> x | _ -> Lib.bork "bluh" in
         let event_parameters =
           [(Address_value operator_address, Address); abi_token_amount amount_to_transfer]
         in
         let event_signature =
           {function_name= "logTransfer"; parameters= event_parameters}
           |> function_signature_digest
         in
         assert (logged_event = event_signature) ;
         (* the operator address is visible as data *)
         let data = unparse_0x_bytes log.data in
         let logged_encoding =
           let tuple_value, tuple_ty = abi_tuple_of_abi_values event_parameters in
           unparse_0x_bytes (encode_abi_value tuple_value tuple_ty)
         in
         assert (logged_encoding = data) ;
         (* confirm contract has received amount transferred *)
         eth_get_balance (contract, Latest)
         >>= fun ending_balance ->
         assert (ending_balance = amount_to_transfer) ;
         (* now try invalid address, make sure it's not logged *)
         let bogus_address_bytes = parse_0x_bytes "0xFF" in
         call_function ~sender ~contract ~call:bogus_address_bytes ~gas_limit:(TokenAmount.of_int 1000000) amount_to_transfer
         >>= confirm_pre_transaction sender
         >>= fun (_transaction, Confirmation.{transaction_hash}) ->
         eth_get_transaction_receipt transaction_hash
         >>= arr Option.get
         >>= fun receipt2 ->
         let logs2 = receipt2.logs in
         return (logs2 = []))
      ()
end

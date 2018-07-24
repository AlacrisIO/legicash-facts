(* Types for LegiCash Facilitator side-chains *)
(* NB: Comments are in the .mli file *)
open Lib
open Action
open Marshaling
open Crypto
open Trie

module TokenAmount = Main_chain.TokenAmount

type fraud_proof

module KnowledgeStage = struct
  type t = Unknown | Pending | Confirmed | Rejected
  module Marshalable = struct
    type nonrec t = t
    let to_char = function Unknown -> 'U' | Pending -> 'P' | Confirmed -> 'C' | Rejected -> 'R'
    let marshal b x = Buffer.add_char b (to_char x)
    let unmarshal = unmarshal_not_implemented
  end
  include (DigestibleOfMarshalable (Marshalable) : DigestibleS with type t := t)
end

type memo = string option

module Invoice = struct
  type t = {recipient: Address.t; amount: TokenAmount.t; memo: memo} [@@deriving lens]
  module Marshalable = OCamlMarshaling (struct type nonrec t = t end)
  include (DigestibleOfMarshalable (Marshalable) : DigestibleS with type t := t)
end

module Operation = struct
  type payment_details =
    {payment_invoice: Invoice.t; payment_fee: TokenAmount.t; payment_expedited: bool}
  [@@deriving lens]

  type deposit_details =
    { deposit_amount: TokenAmount.t
    ; deposit_fee: TokenAmount.t
    ; main_chain_deposit_signed: Main_chain.TransactionSigned.t
    ; main_chain_deposit_confirmation: Main_chain.Confirmation.t
    ; deposit_expedited: bool }
  [@@deriving lens]

  type withdrawal_details =
    {withdrawal_amount: TokenAmount.t; withdrawal_fee: TokenAmount.t}
  [@@deriving lens]

  type t =
    | Deposit of deposit_details
    | Payment of payment_details
    | Withdrawal of withdrawal_details

  module Marshalable = struct
    type nonrec t = t
    let marshal b = function
      | Deposit { deposit_amount
                ; deposit_fee
                ; main_chain_deposit_signed
                ; main_chain_deposit_confirmation
                ; deposit_expedited } ->
        Buffer.add_char b 'D' ;
        TokenAmount.marshal b deposit_amount ;
        TokenAmount.marshal b deposit_fee ;
        marshal_signed Main_chain.Transaction.marshal b main_chain_deposit_signed ;
        Main_chain.Confirmation.marshal b main_chain_deposit_confirmation ;
        marshal_bool b deposit_expedited
      | Payment {payment_invoice; payment_fee; payment_expedited} ->
        Buffer.add_char b 'P' ;
        Invoice.marshal b payment_invoice;
        TokenAmount.marshal b payment_fee;
        marshal_bool b payment_expedited
      | Withdrawal {withdrawal_amount; withdrawal_fee} ->
        Buffer.add_char b 'W' ;
        TokenAmount.marshal b withdrawal_amount;
        TokenAmount.marshal b withdrawal_fee
    let unmarshal = unmarshal_not_implemented
  end
  include (DigestibleOfMarshalable (Marshalable) : DigestibleS with type t := t)
end

module RxHeader = struct
  type t =
    { facilitator: Address.t
    ; requester: Address.t
    ; requester_revision: Revision.t
    ; confirmed_main_chain_state_digest: Main_chain.State.t digest
    ; confirmed_main_chain_state_revision: Revision.t
    ; confirmed_side_chain_state_digest: Digest.t
    ; confirmed_side_chain_state_revision: Revision.t
    ; validity_within: Duration.t }
  [@@deriving lens]
  let marshal b { facilitator
                ; requester
                ; requester_revision
                ; confirmed_main_chain_state_digest
                ; confirmed_main_chain_state_revision
                ; confirmed_side_chain_state_digest
                ; confirmed_side_chain_state_revision
                ; validity_within } =
    Address.marshal b facilitator ;
    Address.marshal b requester ;
    Revision.marshal b requester_revision ;
    Digest.marshal b confirmed_main_chain_state_digest ;
    Revision.marshal b confirmed_main_chain_state_revision ;
    Digest.marshal b confirmed_side_chain_state_digest ;
    Revision.marshal b confirmed_side_chain_state_revision ;
    Duration.marshal b validity_within
end

module Request = struct
  type t = {rx_header: RxHeader.t; operation: Operation.t} [@@deriving lens]
  module Marshalable = struct
    type nonrec t = t
    let marshal b {rx_header; operation} =
      RxHeader.marshal b rx_header ; Operation.marshal b operation
    let unmarshal = unmarshal_not_implemented
  end
  include (DigestibleOfMarshalable (Marshalable) : DigestibleS with type t := t)
end

module TxHeader = struct
  type t = {tx_revision: Revision.t; updated_limit: TokenAmount.t} [@@deriving lens]
  let marshal b {tx_revision; updated_limit} =
    Revision.marshal b tx_revision ; TokenAmount.marshal b updated_limit
end

module Confirmation = struct
  type t = {tx_header: TxHeader.t; signed_request: Request.t signed} [@@deriving lens]
  module Marshalable = struct
    type nonrec t = t
    let marshal b {tx_header; signed_request} =
      TxHeader.marshal b tx_header ; marshal_signed Request.marshal b signed_request
    let unmarshal = unmarshal_not_implemented
  end
  include (DigestibleOfMarshalable (Marshalable) : DigestibleS with type t := t)
end

module AccountState = struct
  type t = {balance: TokenAmount.t; account_revision: Revision.t} [@@deriving lens]
  module Marshalable = struct
    type nonrec t = t
    let marshal buffer {balance; account_revision} =
      TokenAmount.marshal buffer balance;
      Revision.marshal buffer account_revision
    let unmarshal ?(start=0) bytes =
      let balance,balance_offset = TokenAmount.unmarshal ~start bytes in
      let account_revision,final_offset = Revision.unmarshal ~start:balance_offset bytes in
      ( { balance
        ; account_revision
        }
        ,
        final_offset
      )
  end
  include (DigestibleOfMarshalable (Marshalable) : DigestibleS with type t := t)
end

(* Module for Maps from Side_chain.TxHeader.tx_revision to (unsigned) Confirmation *)
module ConfirmationMap = MerkleTrie (Revision) (Confirmation)

module AccountMap = MerkleTrie (Address) (AccountState)

module State = struct
  type t = { previous_main_chain_state: Main_chain.State.t digest
           ; previous_side_chain_state: t digest
           ; facilitator_revision: Revision.t
           ; spending_limit: TokenAmount.t
           ; bond_posted: TokenAmount.t
           ; accounts: AccountMap.t
           ; operations: ConfirmationMap.t
           ; main_chain_transactions_posted: DigestSet.t }
  [@@deriving lens]

  module type DbNameS = sig val db_name : string end

  module Persistence (Trie : MerkleTrieS) (DbName : DbNameS) = struct
    open Trie
    open MarshalNode

    let get_db =
      let db = ref None in
      function () ->
        (* open database on first use; finalizer closes it automatically *)
        if !db == None then
          db := Some (LevelDB.open_db DbName.db_name);
        option_get !db

    let node_saved key =
      let db = get_db () in
      LevelDB.mem db key

    let save_node key data =
      let db = get_db () in
      LevelDB.put db key data

    let recursek ~i:_ ~tree ~k = k (get_synth tree)

    let branchk ~i:_ ~height ~leftr ~rightr ~synth ~k =
      let node_key = Synth.marshal_string synth in
      if node_saved node_key then (
        synth
      )
      else (
        let buffer = Buffer.create 256 in
        marshal_branch buffer leftr rightr height synth;
        save_node node_key (Buffer.contents buffer);
        k synth
      )

    let skipk ~i:_ ~height ~length ~bits ~childr ~synth ~k =
      let node_key = Synth.marshal_string synth in
      if node_saved node_key then (
        synth
      )
      else (
        let buffer = Buffer.create 256 in
        marshal_skip buffer childr bits length height synth;
        save_node node_key (Buffer.contents buffer);
        k synth
      )

    let leafk ~i:_ ~value ~synth ~k =
      let node_key = Synth.marshal_string synth in
      if node_saved node_key then (
        synth
      )
      else (
        let buffer = Buffer.create 256 in
        marshal_leaf buffer value synth;
        save_node node_key (Buffer.contents buffer);
        k synth
      )

    let emptyk ~k =
      let node_key = Synth.marshal_string Synth.empty in
      if not (node_saved node_key) then (
        let buffer = Buffer.create 1 in
        marshal_empty buffer;
        save_node node_key (Buffer.contents buffer)
      );
      k Synth.empty

    let root_key = "ROOT"

    let save tree =
      let root_synth =
        iterate_over_tree
          ~recursek ~branchk ~skipk ~leafk ~emptyk
          ~i:empty_key ~tree ~k:identity
      in
      let db = get_db () in
      LevelDB.put db root_key (Synth.marshal_string root_synth)

    let retrieve_node key =
      let db = get_db () in
      match LevelDB.get db key with
      | Some data ->
        let node,_ = unmarshal_to_node (Bytes.of_string data) in
        node
      | None -> raise (Internal_error (Format.sprintf "Could not retrieve node with key: %s" (unparse_hex_string key)))

    let retrieve () =
      let db = get_db () in
      let root_key =
        match LevelDB.get db root_key with
        | Some s -> s
        | None -> raise (Internal_error (Format.sprintf "Could not get trie root in database %s" DbName.db_name))
      in
      let rec find_node key =
        match retrieve_node key with
        | NodeEmpty -> Empty
        | NodeLeaf {value; synth} -> Leaf {value; synth}
        | NodeBranch { left; right; height; synth } ->
          (* TODO: will recursion blow the stack here? *)
          let left_trie = find_node (Synth.marshal_string left) in
          let right_trie = find_node (Synth.marshal_string right) in
          Branch { left=left_trie; right=right_trie; height; synth }
        | NodeSkip { child; bits; length; height; synth } ->
          let child_trie = find_node (Synth.marshal_string child) in
          Skip { child=child_trie; bits; length; height; synth }
      in
      find_node root_key
  end

  module Marshalable = struct
    type nonrec t = t

    module AccountsDb = struct let db_name = "accounts" end
    module AccountMapPersist = Persistence (AccountMap) (AccountsDb)

    module ConfirmationsDb = struct let db_name = "confirmations" end
    module ConfirmationMapPersist = Persistence (ConfirmationMap) (ConfirmationsDb)

    let marshal buffer t =
      Digest.marshal buffer t.previous_main_chain_state;
      Digest.marshal buffer t.previous_side_chain_state;
      Revision.marshal buffer t.facilitator_revision;
      TokenAmount.marshal buffer t.spending_limit;
      TokenAmount.marshal buffer t.bond_posted;
      let num_elements = DigestSet.cardinal t.main_chain_transactions_posted in
      UInt64.marshal buffer (UInt64.of_int num_elements);
      DigestSet.iter
        (fun elt ->
           Digest.marshal buffer elt)
        t.main_chain_transactions_posted;
      (* save nodes to database *)
      AccountMapPersist.save  t.accounts;
      ConfirmationMapPersist.save t.operations

    let unmarshal ?(start=0) bytes =
      let previous_main_chain_state,previous_main_chain_state_offset =
        Digest.unmarshal ~start bytes in
      let previous_side_chain_state,previous_side_chain_state_offset =
        Digest.unmarshal ~start:previous_main_chain_state_offset bytes in
      let facilitator_revision,facilitator_revision_offset =
        Revision.unmarshal ~start:previous_side_chain_state_offset bytes in
      let spending_limit,spending_limit_offset =
        TokenAmount.unmarshal ~start:facilitator_revision_offset bytes in
      let bond_posted,bond_posted_offset =
        TokenAmount.unmarshal ~start:spending_limit_offset bytes in
      let num_elements64,num_elements64_offset =
        UInt64.unmarshal ~start:bond_posted_offset bytes in
      let num_elements = UInt64.to_int num_elements64 in
      let rec get_digest_set_elements count set offset =
        if count >= num_elements then
          set, offset
        else
          let digest,new_offset = Digest.unmarshal ~start:offset bytes in
          get_digest_set_elements (count + 1) (DigestSet.add digest set) new_offset
      in
      let main_chain_transactions_posted,final_offset =
        get_digest_set_elements 0 DigestSet.empty num_elements64_offset
      in
      (* restore nodes from database *)
      let accounts = AccountMapPersist.retrieve () in
      let operations = ConfirmationMapPersist.retrieve () in
      ( { previous_main_chain_state
        ; previous_side_chain_state
        ; facilitator_revision
        ; spending_limit
        ; bond_posted
        ; accounts
        ; operations
        ; main_chain_transactions_posted
        }
        ,
        final_offset
      )
  end
  include (DigestibleOfMarshalable (Marshalable) : DigestibleS with type t := t)
end

type episteme =
  { request: Request.t signed
  ; confirmation_option: Confirmation.t signed option
  ; main_chain_confirmation_option: Main_chain.Confirmation.t option }
[@@deriving lens]

module UserAccountStatePerFacilitator = struct
  type t =
    { facilitator_validity: KnowledgeStage.t
    ; confirmed_state: AccountState.t
    ; pending_operations: episteme list }
  [@@deriving lens]
  module Marshalable = struct
    type nonrec t = t
    let marshal b { facilitator_validity
                  ; confirmed_state
                  ; pending_operations=_ } =
      KnowledgeStage.marshal b facilitator_validity ;
      AccountState.marshal b confirmed_state ;
      () (* TODO: handle the list pending_operation *)
    let unmarshal = unmarshal_not_implemented
  end
  include (DigestibleOfMarshalable (Marshalable) : DigestibleS with type t := t)
end

module UserAccountStateMap = MerkleTrie (Address) (UserAccountStatePerFacilitator)

type user_state =
  { main_chain_user_state: Main_chain.user_state
  ; facilitators: UserAccountStateMap.t }
[@@deriving lens]

type ('input, 'action) user_action = ('input, 'action, user_state) action

type ('input, 'action) user_async_action = ('input, 'action, user_state) async_action

type verifier_state

type ('input, 'output) verifier_action = ('input, 'output, verifier_state) action

module FacilitatorFeeSchedule = struct
  type t =
    { deposit_fee: TokenAmount.t
    ; withdrawal_fee: TokenAmount.t
    ; per_account_limit: TokenAmount.t
    ; fee_per_billion: TokenAmount.t }
  [@@deriving lens]
  module Marshalable = struct
    type nonrec t = t

    let marshal buffer t =
      TokenAmount.marshal buffer t.deposit_fee;
      TokenAmount.marshal buffer t.withdrawal_fee;
      TokenAmount.marshal buffer t.per_account_limit;
      TokenAmount.marshal buffer t.fee_per_billion

    let unmarshal ?(start = 0) bytes =
      let deposit_fee,deposit_fee_offset =
        TokenAmount.unmarshal ~start bytes in
      let withdrawal_fee,withdrawal_fee_offset =
        TokenAmount.unmarshal ~start:deposit_fee_offset bytes in
      let per_account_limit,per_account_limit_offset =
        TokenAmount.unmarshal ~start:withdrawal_fee_offset bytes in
      let fee_per_billion,final_offset =
        TokenAmount.unmarshal ~start:per_account_limit_offset bytes in
      ( { deposit_fee
        ; withdrawal_fee
        ; per_account_limit
        ; fee_per_billion
        }
        ,
        final_offset
      )
  end
  include (DigestibleOfMarshalable (Marshalable) : DigestibleS with type t := t)
end

module FacilitatorState = struct
  type t = { keypair: Keypair.t
           ; previous: State.t option
           ; current: State.t
           ; fee_schedule: FacilitatorFeeSchedule.t
           } [@@deriving lens]

  module Marshalable = struct
    type nonrec t = t

    let marshal buffer t =
      Keypair.marshal buffer t.keypair;
      (* use tag to mark option *)
      begin
        match t.previous with
        | None -> Marshaling.marshal_char buffer 'N'
        | Some state ->
          Marshaling.marshal_char buffer 'S';
          State.marshal buffer state
      end;
      State.marshal buffer t.current;
      FacilitatorFeeSchedule.marshal buffer t.fee_schedule

    let unmarshal ?(start=0) bytes =
      let keypair,keypair_offset = Keypair.unmarshal ~start:start bytes in
      let option_tag = Bytes.get bytes keypair_offset in
      let previous,previous_offset =
        match option_tag with
        | 'N' -> None,keypair_offset + 1
        | 'S' ->
          let prev,offs = State.unmarshal ~start:(keypair_offset + 1) bytes in
          Some prev,offs
        | _ -> raise (Internal_error "Unexpected tag for State within FacilitatorState")
      in
      let current,current_offset = State.unmarshal ~start:previous_offset bytes in
      let fee_schedule,final_offset =
        FacilitatorFeeSchedule.unmarshal ~start:current_offset bytes in
      ({ keypair
       ; previous
       ; current
       ; fee_schedule
       },
       final_offset)
  end

  include (DigestibleOfMarshalable (Marshalable) : DigestibleS with type t := t)

  module Persistence = struct
    open LevelDB

    let db_key_of_facilitator_address address =
      Format.sprintf "facilitator-0x%s" (Address.to_hex_string address)

    let db_name = "facilitator_state"

    let get_db =
      let db = ref None in
      function () ->
        (* open database on first use; finalizer closes it automatically *)
        if !db == None then
          db := Some (open_db db_name);
        option_get !db

    let save facilitator_state =
      let db = get_db () in
      let db_key = db_key_of_facilitator_address facilitator_state.keypair.address in
      put db db_key (marshal_string facilitator_state)

    let retrieve facilitator_address =
      let db = get_db () in
      let db_key = db_key_of_facilitator_address facilitator_address in
      let unmarshaled =
        match get db db_key with
        | Some s -> Bytes.of_string s (* TODO: can we avoid copying? *)
        | None -> raise (Internal_error
                           (Format.sprintf "No facilitator state saved for address 0x%s"
                              (Address.to_hex_string facilitator_address)))
      in
      unmarshal_bytes unmarshaled
  end
end

type ('input, 'output) facilitator_action = ('input, 'output, FacilitatorState.t) action

type court_clerk_confirmation = {clerk: public_key; signature: signature} [@@deriving lens]

type update = {current_state: State.t digest; availability_proof: court_clerk_confirmation list}

type user_to_user_message

type user_to_facilitator_message

type facilitator_to_user_message

type facilitator_to_facilitator_message

exception No_facilitator_yet

exception Already_open

exception Already_closed

exception Account_closed_or_nonexistent

exception Invalid_confirmation

exception Invalid_operation of Operation.t

let one_second = Duration.of_int 1000000000

let challenge_duration = Duration.mul one_second (Duration.of_int 7200)

module Test = struct

  open Keypair.Test

  (* a sample facilitator state *)

  let trent_fee_schedule : FacilitatorFeeSchedule.t =
    { deposit_fee= TokenAmount.of_int 5
    ; withdrawal_fee= TokenAmount.of_int 5
    ; per_account_limit= TokenAmount.of_int 20000
    ; fee_per_billion= TokenAmount.of_int 42 }

  let confirmed_trent_state =
    State.{ previous_main_chain_state= Digest.zero
          ; previous_side_chain_state= Digest.one
          ; facilitator_revision= Revision.of_int 0
          ; spending_limit= TokenAmount.of_int 1000000
          ; bond_posted= TokenAmount.of_int 5000000
          ; accounts= AccountMap.empty
          ; operations= ConfirmationMap.empty
          ; main_chain_transactions_posted= DigestSet.empty }

  let trent_state =
    let open FacilitatorState in
    { keypair= trent_keys
    ; previous= None
    ; current= confirmed_trent_state
    ; fee_schedule= trent_fee_schedule }

  let%test "db-save-retrieve" =
    FacilitatorState.Persistence.save trent_state;
    let retrieved_state = FacilitatorState.Persistence.retrieve trent_address in
    retrieved_state = trent_state

end

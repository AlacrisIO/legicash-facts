open Legilogic_lib
open Action
open Lwt_exn
open Signing

open Ethereum_chain
open Ethereum_json_rpc
open Assembly
open Ethereum_user
open Contract_config

(** EVM Contract for batch transfers.

    Instead of sending a series of N transactions, have a single transaction that does it all.
    The advantage is that you have only one transaction to nurse to completion,
    which is a much less complex thing to do, with fewer and simpler failure scenarios to handle.
    This is especially important if your transaction posting code needs to deal with
    FOMO3D-style block-buying attacks, or other rapid gas price increase events.

    The input data format does not use the Solidity ABI; we use a simpler and cheaper style:
    Just a raw vector of 32-byte records each comprised of a 20-byte address and a 12-byte value.
    No 4-byte header to identify a "function" to call; there's only one "function".
    No 32-byte vector count as first implicit argument; the size is taken from CALLDATASIZE.
    We never copy anything to memory; we just extract data to the stack.

    Before we execute any transfer, we check that the sender matches the declared owner.
    Thus, if any money is sent to the contract or left in it, only the sender can later
    transfer that money out of the contract.

    If any single transfer in the batch fails (because of lack of either funds or gas),
    revert the entire transaction (which cancels all the transfers that previously succeeded).
    *)

let batch_log = true

(** Create the runtime code for a batch contract associated to given owner *)
let batch_contract_runtime (owner : Address.t) : string =
  assemble [
      (* At instruction 0, so push 0 on stack while it's extra cheap! *)
      eGETPC; (* -- 0 *)

      (* Abort if the caller isn't the contract's owner *)
      push_address owner; eCALLER; eEQ; jumpi1 "loop_init" 29;
      eDUP1; eREVERT;

      (* Initialize the loop invariant *)
      jumplabel "loop_init"; (* @ 29 -- 0 *);
      push_int 1; push_int 96; push_int 2; eEXP; eDUP2; eDUP2; eSUB; eCALLDATASIZE; eDUP5;
      (* -- 0 size 2**96-1 2**96 1 0 *)
      jump1 "loop_entry" 49; (* jump to entry, skipping inter-loop action *)

      (* The loop: inter-loop action *)
      jumplabel "loop"; (* @ 45 *);
      push_int 32; eADD;

      (* The entry point of the loop: check condition *)
      jumplabel "loop_entry"; (* @ 49 -- cursor size 2**96-1 2**96 1 0 *)
      (* If less then continue to loop_body, else return *)
      eDUP2; eDUP2; eLT; jumpi1 "loop_body" 57; eSTOP;

      (* Loop body: take the next 256-bit argument.
         Top 160 are address, lower 96 are value in wei.
         Prepare the arguments to a transfer call. *)
      jumplabel "loop_body"; (* @ 57 -- cursor size 2**96-1 2**96 1 0 *)
      eDUP6; eDUP1; eDUP1; eDUP1; eDUP5; eCALLDATALOAD; eDUP1; eDUP9; eAND;
      (* -- value data 0 0 0 0 cursor size 2**96-1 2**96 1 0 *)
      eSWAP1; eDUP10; eSWAP1; eDIV; eGAS;
      (* Transfer! -- gas address value 0 0 0 0 cursor size 2**96-1 2**96 1 0 *)
      eCALL;

      (* loop if successful, revert everything if failed. *)
      jumpi1 "loop" 45;
      (* -- cursor size 2**96-1 2**96 1 0 *)
      eDUP6; eDUP1; eREVERT]

(** Given a constant contract runtime of length below 255,
    that doesn't need any memory initialization,
    return a contract initialization string, to be passed as parameter
    to a CreateContract operation, to register the contract.
    Beware: the code produced is not relocatable.
 *)
let constant_contract_init_1 : string -> string =
  fun contract_runtime ->
  assemble [
    (* Push args for RETURN; doing it in this order saves one byte and some gas *)
    push_int (String.length contract_runtime); push_int 0 (* memory address for the code *);
    (* -- 0 length *)

    (* Push args for CODECOPY; the DUP's for length and memory target are where the savings are *)
    eDUP2 (* length *); pushlabel1 "runtime_start" 10; eDUP3 (* memory target address: 0 *);
    (* -- 0 start length 0 length *)

    (* Initialize the contract by returning the memory array containing the runtime code *)
    eCODECOPY; eRETURN;

    (* Inline code for the runtime as a code constant in the init code *)
    label "runtime_start" (* @ 10 *); string contract_runtime]

(** Create the runtime code for a batch contract associated to given owner *)
let batch_contract_init = batch_contract_runtime >> constant_contract_init_1

(** Register "the" batch transfer contract associated with the owner,
    and return the transaction hash for the contract creation. *)
let register_batch_contract owner =
  if batch_log then
    Logging.log "creating batch contract for %s" (Address.to_0x owner);
  post_operation
    ~operation:(Operation.CreateContract (Bytes.of_string @@ batch_contract_init owner))
    ~sender:owner
    ~value:TokenAmount.zero
  >>= fun receipt -> return receipt.transaction_hash

(** Ensure that there is a batch transfer contract associated with the owner
    on the blockchain and saved to the working database, and
    return the ContractConfig for that contract. *)
let ensure_batch_contract owner =
  ensure_contract_of_db ("BATC" ^ Address.to_big_endian_bits owner)
    (fun () -> register_batch_contract owner)
  >>= fun config ->
  if batch_log then
    Logging.log "batch contract for %s is %s"
      (Address.to_0x owner) (ContractConfig.to_yojson_string config);
  return config

let batch_transfer sender transfers =
  let fmt (address, amount) =
    Address.to_big_endian_bits address ^
      Integer.big_endian_bits_of_nat 96 (TokenAmount.z_of amount) in
  let data = List.map fmt transfers |> String.concat "" |> Bytes.of_string in
  let value = List.fold_left TokenAmount.add TokenAmount.zero (List.map snd transfers) in
  if batch_log then
    Logging.log "batch transfer value: %s data: %s"
      (TokenAmount.to_string value) (Hex.unparse_0x_bytes data);
  ensure_batch_contract sender >>= fun config ->
  post_operation
    ~operation:(Operation.CallFunction (config.contract_address, data))
    ~sender ~value

module Test = struct
  open Lwt_exn
  open Signing.Test
  open Ethereum_user.Test

  (** display an account having the given balance given a way to print address, optional name and balance *)
  let display_balance : (string -> string -> 'a) -> Address.t -> TokenAmount.t -> 'a =
    fun display address balance ->
    display
      (nicknamed_string_of_address address)
      (TokenAmount.to_0x balance)

  let get_address_missing_amount amount address =
    eth_get_balance (address, BlockParameter.Pending) >>= fun balance ->
    let open TokenAmount in
    if compare balance amount >= 0 then
      display_balance (printf "Account %s contains %s wei already. Good.\n") address balance
      >>= const []
    else
      display_balance (printf "Account %s contains %s wei only. Funding.\n") address balance
      >>= const [address, (sub amount balance)]

  let ensure_addresses_prefunded prefunded_address amount addresses =
    list_map_s (get_address_missing_amount amount) addresses
    >>= arr List.concat
    >>= batch_transfer prefunded_address

  let test_accounts = [("Alice", alice_keys); ("Bob", bob_keys); ("Trent", trent_keys)]

  let%test "batch_transfer_works" =
    Lwt_exn.run
      (fun () ->
        of_lwt Db.open_connection "unit_test_db" >>= fun () ->
        get_prefunded_address () >>= fun prefunded_address ->
        let addresses = [alice_address;bob_address;trent_address] in
        let get_balances () =
          list_map_s (fun address -> eth_get_balance (address, BlockParameter.Pending)) addresses in
        get_balances () >>= arr (List.fold_left TokenAmount.max TokenAmount.zero) >>= fun amount ->
        let target_amount = TokenAmount.(add amount (div one_ether (of_int 1000))) in
        Logging.log "target_amount: %s" (TokenAmount.to_string target_amount);
        ensure_addresses_prefunded prefunded_address target_amount addresses >>= fun _ ->
        get_balances () >>= fun balances ->
        List.for_all (TokenAmount.equal target_amount) balances |> return) ()

  let ensure_test_accounts ?(min_balance=one_ether) ?(accounts=test_accounts) () =
    get_prefunded_address ()
    >>= fun prefunded_address ->
    list_iter_s (Ethereum_transaction.ensure_private_key >>> const ())
      (List.map snd accounts) >>= fun () ->
    let addresses = List.map (fun (nickname, keypair) ->
                        register_keypair nickname keypair; keypair.address) accounts in
    ensure_addresses_prefunded prefunded_address min_balance addresses
end

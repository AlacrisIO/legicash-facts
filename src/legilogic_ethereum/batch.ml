open Legilogic_lib
open Action
open Lwt_exn
open Signing

open Ethereum_chain
open Ethereum_json_rpc
open Assembly
open Ethereum_user
open Contract_config

let batch_log = true

let batch_contract owner =
  assemble [
      eGETPC; (* at instruction 0, so push 0 on stack while it's cheap! -- 0 *)
      (* check that the caller is the contract's owner *)
      ePUSH20; string (Address.to_big_endian_bits owner); eCALLER; eEQ;
      ePUSH1; fixup 1 (Label "loop_init") 29; eJUMPI;
      eDUP1; eREVERT; (* abort *)
      label "loop_init"; (* @ 30 -- 0 *) eJUMPDEST;
      push_int 1; push_int 96; push_int 2; eEXP; eDUP2; eDUP2; eSUB; eCALLDATASIZE; eDUP5;
      (* -- 0 1 2**96 2**96-1 datasize 0 *)
      ePUSH1; fixup 1 (Label "loop_entry") 49; eJUMP;
      label "loop"; (* @ 45 *) eJUMPDEST;
      push_int 32; eADD;
      label "loop_entry"; (* @ 49 -- cursor size 2**96-1 2**96 1 0 *) eJUMPDEST;
      eDUP2; eDUP2; eLT; (* -- size-current cursor size 2**96-1 2**96 1 0 *)
      ePUSH1; fixup 1 (Label "loop_body") 57; eJUMPI; (* if less then loop_body, else return *)
      eSTOP; (* 0 1 2**96 2**96-1 datasize current_index -- return *)
      label "loop_body"; (* @ 57 -- 0 1 2**96 2**96-1 datasize current_index *) eJUMPDEST;
      eDUP6; eDUP1; eDUP1; eDUP1; eDUP5; eCALLDATALOAD; eDUP1; eDUP9; eAND;
      (* -- 0 1 2**96 2**96-1 datasize current_index 0 0 0 0 data value *)
      eSWAP1; eDUP10; eSWAP1; eDIV; eGAS;
      (* -- 0 1 2**96 2**96-1 datasize current_index 0 0 0 0 value address gas *)
      eCALL; ePUSH1; fixup 1 (Label "loop") 45; eJUMPI; (* transfer, loop if successful *)
      (* 0 1 2**96 2**96-1 datasize current_index -- *)
      eDUP6; eDUP1; eREVERT]

let batch_contract_init owner =
  let runtime = batch_contract owner in
  assemble [
    (* Push args for RETURN -- 0 length *)
    push_int (String.length runtime); push_int 0;
    (* Push args for CODECOPY -- 0 start length 0 length *)
    eDUP2; ePUSH1; fixup 1 (Label "runtime_start") 10; eDUP3;
    eCODECOPY; eRETURN;
    label "runtime_start"; (* @ 10 *)
    string runtime]

let register_batch_contract owner =
  if batch_log then
    Logging.log "creating batch contract for %s" (Address.to_0x owner);
  post_operation
    ~operation:(Operation.CreateContract (Bytes.of_string @@ batch_contract_init owner))
    ~sender:owner
    ~value:TokenAmount.zero
  >>= fun receipt -> return receipt.transaction_hash

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

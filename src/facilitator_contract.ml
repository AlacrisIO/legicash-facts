open Crypto
open Main_chain

let contract_address = ref Address.zero

let set_contract_address address = contract_address := address

let make_deposit_call facilitator_address =
  let open Ethereum_abi in
  assert (!contract_address != Address.zero );
  let parameters = [ (Address_value facilitator_address, Address)
                   ; (Bytes_value (Bytes.of_string "Deposit"), BytesDynamic)
                   ]
  in
  let signature = make_signature_bytes { function_name = "deposit"
                                       ; parameters
                                       }
  in
  Operation.CallFunction
    ( !contract_address
    , signature
    )

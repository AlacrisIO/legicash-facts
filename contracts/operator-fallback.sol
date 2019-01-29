pragma solidity ^0.5.2;
/* This is a test contract, used in tests in src/ethereum_transaction.ml
   to demonstrate that using an address instead of an ABI-encoded
   function call will call the fallback, with the address as data.
   This technique can be used to accept payments using a simple TransferToken operation.
   In a one-contract-per-operator setup, the amount can be directly credited
   to the single operator of the contract.
   In a one-contract-per-court-registry setup, this could still work, but with extra steps:
   the operator with the first confirmed state with a claim signed by the depositor is
   the operator that gets credited, which requires a confirmation of the claim on the side-chain.
*/

contract Operator {
  event logTransfer(address operator,uint amount);

  // fallback receives a value of 'bytes' type, want to log it as an address
  function bytesToAddress(bytes bys) private pure returns (address addr) {
    assembly {
      // skip past length prefix to load address bytes
      addr := mload(add(bys,32))
      // shift right 12 bytes
      addr := div(addr,0x1000000000000000000000000)
    }
  }

  function () public payable {
    // valid addresses are 20 bytes
    require (msg.data.length == 20);
    emit logTransfer(bytesToAddress(msg.data),msg.value);
  }
}

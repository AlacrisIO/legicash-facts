pragma solidity ^0.4.22;

/* contract to demonstrate that using an address instead of an ABI-encoded
   function call will call the fallback, with the address as data
*/

contract Facilitator {
  event logTransfer(address facilitator,uint amount);

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
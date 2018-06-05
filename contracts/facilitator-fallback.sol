pragma solidity ^0.4.22;

// contract to demonstrate that using an address instead of an ABI-encoded
// function call will call the fallback, with the address as data

contract Facilitator {
  event logTransfer(address facilitator,uint amount);
  event invalidTransfer(bytes invalidAddress,uint amount);

  // fallback receives a value of 'bytes' type, want to log it as an address
  function bytesToAddress(bytes bys) private pure returns (address addr) {
    assembly {
      addr := mload(add(bys,20))
    }
  }

  function () public payable {
    if (msg.data.length != 20) {
      emit invalidTransfer(msg.data,msg.value);
    }
    else {
      emit logTransfer(bytesToAddress(msg.data),msg.value);
    }
  }
}

pragma solidity ^0.4.22;

// contract to demonstrate that using an address instead of an ABI-encoded
// function call will call the fallback, with the address as data

contract Facilitator {
  event logTransfer(address facilitator,uint amount);
  event invalidTransfer(bytes not_an_address,uint amount);

  function bytesToAddress(bytes bys) private pure returns (address addr) {
    assembly {
      addr := mload(add(32,bys))
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

pragma solidity ^0.4.22;

// contract to demonstrate that using an address instead of an ABI-encoded
// function call will call the fallback, with the address as data

contract Facilitator {
  event logTransfer(bytes facilitator,uint amount);
  function () public payable {
    emit logTransfer(msg.data,msg.value);
  }
}

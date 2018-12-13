// contract with just deposit, withdrawal, for purposes of endpoints demo
// emit events, don't do anything else

pragma solidity ^0.4.24;

contract Operators {

  event Deposited(address operator, address recipient, uint value, bytes memo);

  function deposit(address operator, bytes memo) public payable {
    emit Deposited(operator, msg.sender, msg.value, memo);
  }

  event Withdrawal(address operator, uint64 ticket);

  function withdraw(address operator, uint64 ticket, uint _bond, bytes32 _confirmed_state) public {
    emit Withdrawal(operator, ticket);
    _bond+0;
    _confirmed_state[0];
    // msg.sender.transfer(_value + _bond);
  }
}

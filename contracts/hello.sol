// This contract is used by a test in src/ethereum_transaction.ml
// TODO: move it to a test directory?
pragma solidity ^0.4.22;

contract HelloWorld {
  event showResult(string result);
  function printHelloWorld () public returns (string) {
    emit showResult('Hello, world!');
    return 'Goodbye!';
  }
}

pragma solidity ^0.4.22;

contract HelloWorld {
  event showResult(string result);
  function printHelloWorld () public returns (string) {
    emit showResult('Hello, world!');
    return 'Goodbye!';
  }
}

// TODO: move it to a test directory?
pragma solidity ^0.5.2;

contract HelloWorld {
    event showResult(string result);
    function printHello() public pure returns (string memory) {
        return "Goodbye!";
    }
}

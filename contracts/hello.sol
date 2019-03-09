// TODO: move it to a test directory?
pragma solidity ^0.5.2;

contract HelloWorld {
    event showResult(string result);
    function printHello() public pure returns (string memory) {
        return "Goodbye!";
    }

    function printHelloNum(int num) public pure returns (int) {
        return num;
    }

    function printHelloName(string memory _name) public returns (string memory) {
        string memory s = strConcat("Hello, ", _name);
        emit showResult(s);
        return "Goodbye!";
    }

    function strConcat(string memory _a, string memory _b) internal pure returns (string memory) {
        bytes memory _ba = bytes(_a);
        bytes memory _bb = bytes(_b);
        string memory ab = new string(_ba.length + _bb.length);
        bytes memory bab = bytes(ab);
        uint k = 0;
        for (uint i = 0; i < _ba.length; i++) bab[k++] = _ba[i];
        for (uint i = 0; i < _bb.length; i++) bab[k++] = _bb[i];
        return string(bab);
    }
}

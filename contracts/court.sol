pragma solidity ^0.6.4;

/**
 * Contract for a Court that adjudicates claims about side-chains managed by operators.
 *
 * This file is the main entry point of the Alacris contract.
 * The contract is defined in parts imported from the files below:
 */
import "claims.sol";
import "claimtypes.sol";
import "bonds.sol";
import "operator.sol";

contract Court is Claims, ClaimTypes, Bonds, Operators {
}

// TODO: For patricia tree verification, import code from:
//   https://github.com/chriseth/patricia-trie
//   https://github.com/ethereum/solidity-examples

// NB: We currently use abi.encodePacked because it's simpler to support, both
// on the Solidity and the OCaml side. If we have time and find that RLP encoding or some such
// is more efficient on the Solidity side, we may adopt it more widely.
// We still have to support RLP somewhat, to check the validity of deposit claims.

pragma solidity ^0.4.23;

/**
 * Contract for a Court that adjudicates claims about side-chains managed by facilitators.
 *
 * This file is the main entry point of the Alacris contract.
 * The contract is defined in parts imported from the files below:
 */
import "claims.sol";
import "claimtypes.sol";
import "bonds.sol";
import "ethereum-blocks.sol";
import "facilitator.sol";

contract Court is Claims, ClaimTypes, Bonds, EthereumBlocks, Facilitators {
}

// TODO: For patricia tree verification, import code from:
//   https://github.com/chriseth/patricia-trie
//   https://github.com/ethereum/solidity-examples

// NB: We currently use abi.encodePacked because it's simpler to support, both
// on the Solidity and the OCaml side. If we have time and find that RLP encoding or some such
// is more efficient on the Solidity side, we may adopt it more widely.
// We still have to support RLP somewhat, to check the validity of deposit claims.

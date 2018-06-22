pragma solidity ^0.4.23;
//pragma experimental ABIEncoderV2;

// TODO: For patricia tree verification, import code from:
//   https://github.com/chriseth/patricia-trie
//   https://github.com/ethereum/solidity-examples

// TODO: For ethereum block verification, have part of the contract consist in validating
// a patricia tree of Ethereum blocks, with a recent one (directly checkable) as root,
// and all the subsequent ones verifiably linking each to its parent.


/**
 * Contract for publishing a trie of ethereum blocks
 */
contract EthereumBlocks {
    // As part of a state update, a facilitator will publish a trie of all ethereum blocks.
    //
    // The trie must be of a height close enough to the top that the top can be validated,
    // either [TODO: we have to choose statically]
    // (1) based on the state update preemptively extracting the top from the 256
    // most recent blocks, which can be used later as evidence, if there is a challenge,
    // but cost some gas immediately, or
    // (2) based on a state update posted by the challenger, which means the challenge window
    // has to be extremely short for this particular challenge, and/or the bond has to be great enough
    // to cover stupid linear groveling of a long Merkle tree, and/or we use zkSNARKs of some kind
    // (if available).
    //
    // Also, we must follow the trivial property that table[i].preimage.parent = table[i-1]
    // for all i>0
    //
    // Alternatively, we could require the Court Registry to check
    // the well-formedness of this index as well as its general shape.
    /*
    function dispute_ethereum_blocks(
            bytes32 disputed_root, uint64 disputed_height,
            bytes32[] MerklePath
            ) {
    }
   */
}


pragma solidity ^0.4.23;
import "claims.sol";

/**
 * Contract defining an enum for all the types of claim in the Alacris contract
 */
contract ClaimTypes is Claims {

    // Solidity won't hash a struct, so we manually use abi.encodePacked in an untyped way.
    // Moreover, we want to distinguish between struct's, so we tag them with a type tag.

    enum ClaimType {
        STATE_UPDATE, // for a operator, claim an update to the side chain state.
        WITHDRAWAL_CLAIM, // for a user, claim a withdrawal
        WITHDRAWAL // for a user, exercise a valid withdrawal claim and withdraw the money
    }

    /**
     * Digest a claim.
     *
     * Usage pattern: digest_claim(operator, tag, abi.encodePacked(struct_values...))
     * Unhappily, that pattern can't be made a function (bad types) and can't be made a macro (no macros).
     *
     * @param _operator the operator for the side-chain.
     * @param _tag identification of the type of data structure being claimed.
     * @param _data data for the claim, typically itself the digest of a larger data structure.
     * @return the digest for the claim.
     */
    function digest_claim(address _operator, ClaimType _tag, bytes32 _data)
            internal pure returns(bytes32) {
        return keccak256(abi.encodePacked(_operator, _tag, _data));
    }

    /**
     * Forget about some expired claim.
     *
     * Must be called by the operator only, with a known tag.
     * This allows the operator to get a partial gas refund,
     * But is also a precursor to releasing his bond.
     */
    function expire_claim(ClaimType _tag, bytes32 _data) internal {
        bytes32 claim = digest_claim(msg.sender, _tag, _data);
        require(is_claim_status_expired(claim_status[claim]));
        claim_status[claim] = 0;
    }
}

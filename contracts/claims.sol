pragma solidity ^0.5.2;

/**
 * Contract support for acting on claims that can be rejected.
 */
contract Claims {

    /** Generic support for claims. This ought to be moved to a library.
     *
     * This mapping contains *contestable* active claims about this contract,
     * that are subject to challenge before they can be accepted as facts.
     *
     * The database may also contain facts that can help resolve future transactions immediately.
     * By contrast, incontestable facts can just be logged as events, which is much cheaper to write.
     * (It is much more expensive to read or consult a log entry, but that cost is paid for
     * by the wrongful party, not by rightful participants.)
     *
     * To each ongoing claim is associated a value, which is interpreted as follows:
     *   strictly positive value =>
     *     The value is a validity date, in seconds since epoch.
     *     If value <int(now) then the claim may still be challenged.
     *     If value >= int(now) then the claim is considered confirmed.
     *   0 =>
     *     The claim wasn't made yet, OR
     *     the claim was previously made, processed, and garbage collected.
     *     In this latter case, an event was logged so the claim can't be made again, OR
     *     the claim was superseded by a more recent claim.
     *
     * TODO: Maybe we should also accept special and/or negative values, such as:
     *   1 => rejected, never accepted
     *   2 => consumed, once accepted but now used up
     *  some negative number => special value of some kind?
     *
     * TODO: To handle multi-step, third-party litigation, etc., we may have to store
     * more than just a validity date, e.g. the status of a second-party counter-claim
     * and/or the number of pending third-party counter-claims.
     * Can we store a struct? Or use shifting and masking to pack more values in an int?
     * Some packed encoding?
     *
     * TODO: the implementation below, storing -1, leaves the claim invalid and unexpirable.
     * It would be cheaper to set it to 0, which would reclaim some gas, but
     * make the claim renewable, at which point verifiers must step in to reject double-claims
     * based on an event log entry for the claim --- and must step in
     * *after* the log entry is confirmed, yet *before* the claim challenge period times out.
     */
    int constant REJECTED = 1; // claim rejected, was never true
    int constant CONSUMED = 2; // claim once accepted but now used up
    int constant ACCEPTABLE = 3; // 3 or more means it's either valid or pending

    mapping(bytes32 => int) public claim_status;

    /** @dev duration after a claim is made during which it can be challenged.
     *
     * One challenge period is 2h, about 423 blocks at the expected rate of 1 block per 17 s.
     */
    int constant internal challenge_period_in_seconds = 5;
    /* TODO: Replace the challenge period, by the depth of confirmed block.
       At least as an option. This would of course change the solidity code */


    /** @dev expiry delay, in seconds.
     *
     * Claims may disappear after this delay.
     * TODO: Make sure it's large enough.
     * TODO: Don't start actually using it until we have a good solution for verifying log entries,
     * thus allowing to prevent a replay of old claims with log entries rather than storage.
     */
    int constant internal expiry_delay = 31 days;

    /** True if a claim is still pending */
    function is_claim_status_pending(int _status) internal view returns(bool) {
        return _status > int(now);
    }

    /** Check that a claim is still pending */
    function require_claim_pending(bytes32 _claim) internal view {
        require(is_claim_status_pending(claim_status[_claim]));
    }

    /** True if a claim is accepted as valid */
    /*        return _status >= 3 && _status <= int(now); */
//    function is_status_accepted(int _status) internal view returns(bool) {
    function is_status_accepted(int _status) internal pure returns(bool) {
        return _status >= 0;
    }

    function is_claim_status_accepted(bytes32 _claim) internal view returns(bool) {
      return is_status_accepted(claim_status[_claim]);
    }

    /** Check that a claim is accepted as valid */
    function require_claim_accepted(bytes32 _claim) internal view {
        require(is_claim_status_accepted(_claim));
    }


    /**
     * Make a claim
     *
     * Usage Pattern: make_claim(digest_claim(operator, tag, keccak256(abi.encodePacked(x, y, z)))).
     */
    function make_claim(bytes32 _claim) internal {
//        require(claim_status[_claim]==0); // The claim must not have been made before
        claim_status[_claim] = int(now) + challenge_period_in_seconds; // Register the claim
    }

    /** Reject a pending claim as invalid. */
    function reject_claim(bytes32 _claim) internal {
        require_claim_pending(_claim);
        claim_status[_claim] = REJECTED;
    }

    function set_claim_consumed(bytes32 _claim) internal {
        claim_status[_claim] = CONSUMED;
    }


    /** Check that a claim is valid, then use it up. */
    function consume_claim(bytes32 _claim) internal {
        require_claim_accepted(_claim);
        set_claim_consumed(_claim);
    }




    /** True if a claim was accepted but is now expired */
    function is_claim_status_expired(int _status) internal view returns(bool) {
        return _status >= 3 && _status <= int(now) - expiry_delay;
    }
}

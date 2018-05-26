pragma solidity ^0.4.23;

/**
 * Contract for a number of facilitator using the same court registry
 */
contract Facilitators {

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
    int constant challenge_period_in_seconds = 2 hours;

    /** @dev expiry delay, in seconds.
     *
     * Claims may disappear after this delay.
     * TODO: Make sure it's large enough.
     */
    int constant expiry_delay = 31 days;


    /**
     * Digest a claim.
     *
     * Usage pattern: digest_claim(facilitator, tag, abi.encodePacked(struct_values...))
     * Unhappily, that pattern can't be made a function (bad types) and can't be made a macro (no macros).
     *
     * @param _facilitator the facilitator for the side-chain.
     * @param _tag identification of the type of data structure being claimed.
     * @param _data data for the claim, typically itself the digest of a larger data structure.
     * @return the digest for the claim.
     */
    function digest_claim(address _facilitator, ClaimType _tag, bytes32 _data)
            private pure returns(bytes32) {
        return keccak256(abi.encodePacked(_facilitator, _tag, _data));
    }

    /** True if a claim is still pending */
    function is_claim_status_pending(int _status) private view returns(bool) {
        return _status > int(now);
    }

    /** Check that a claim is still pending */
    function require_claim_pending(bytes32 _claim) private view {
        require(is_claim_status_pending(claim_status[_claim]));
    }

    /** True if a claim is accepted as valid */
    function is_claim_status_accepted(int _status) private view returns(bool) {
        return _status >= 3 && _status <= int(now);
    }

    /** Check that a claim is accepted as valid */
    function require_claim_accepted(bytes32 _claim) private view {
        require(is_claim_status_accepted(claim_status[_claim]));
    }

    /**
     * Make a claim
     *
     * Usage Pattern: make_claim(digest_claim(facilitator, tag, keccak256(abi.encodePacked(x, y, z)))).
     */
    function make_claim(bytes32 _claim) private {
        require(claim_status[_claim]==0); // The claim must not have been made before
        claim_status[_claim] = int(now) + challenge_period_in_seconds; // Register the claim
    }

    /** Reject a pending claim as invalid. */
    function reject_claim(bytes32 _claim) private {
        require_claim_pending(_claim);
        claim_status[_claim] = REJECTED;
    }

    /** Check that a claim is valid, then use it up. */
    function consume_claim(bytes32 _claim) private {
        require_claim_accepted(_claim);
        claim_status[_claim] = CONSUMED;
    }

    /** True if a claim was accepted but is now expired */
    function is_claim_status_expired(int _status) private view returns(bool) {
        return _status >= 3 && _status <= int(now) - expiry_delay;
    }

    /**
     * Forget about some expired claim.
     *
     * Must be called by the operator only, with a known tag.
     * This allows the operator to get a partial gas refund,
     * But is also a precursor to releasing his bond.
     */
    function expire_claim(ClaimType _tag, bytes32 _data) private {
        bytes32 claim = digest_claim(msg.sender, _tag, _data);
        require(is_claim_status_expired(claim_status[claim]));
        claim_status[claim] = 0;
    }

    /**
     * Support for computing bonds: get gas cost estimate
     *
     * The minimum bond for each operation should be low enough to be affordable,
     * yet large enough to cover the gas costs of an entire trial over the duration of
     * the challenge window.
     *
     * The contract ought to comfortably and conservatively overestimate of the price of gas
     * for the duration of any upcoming legal argument.
     * TODO: Ideally, gas cost estimates should be dynamically computed from the environment.
     */
    function get_gas_cost_estimate () pure private returns(int) {
        // 100 shannon (= 100 gwei, .1 szabo),
        // a somewhat conservative value for May 2018 (when the median is about 10).
        // But NOT a future-proof value.
        // TODO: dynamic configuration? Migrate to a new contract before this gets too bad?
        return 100*1000*1000*1000 wei;
    }

    /**
     * Compute the minimum bond to stake when making a given claim.
     *
     * The parameter should be larger than the maximum gas necessary
     * for one honest party to challenge the claim.
     */
    function minimum_bond(int _maximum_gas) pure private returns(int) {
        // TODO: Should we worry about overflow? If so, prevent it as follows:
        //     require(maximum_gas) < 2**254 / get_gas_cost_estimate())
        return _maximum_gas*get_gas_cost_estimate();
    }

    /**
     * Require that the posted bond be sufficient to cover the gas required to challenge the claim.
     */
    function require_bond(uint _bond, int _maximum_gas) private pure {
        require(_bond >= uint(minimum_bond(_maximum_gas)));
    }

    // Solidity won't hash a struct, so we manually use abi.encodePacked in an untyped way.
    // Moreover, we want to distinguish between struct's, so we tag them with a type tag.

    enum ClaimType {
        STATE_UPDATE, // for a facilitator, claim an update to the side chain state.
        WITHDRAWAL_CLAIM, // for a user, claim a withdrawal
        WITHDRAWAL // for a user, exercise a valid withdrawal claim and withdraw the money
    }


    // DEPOSITS
    //
    // NB: We do NOT need a deposit function that is called to deposit money.
    // Instead, depositors will do a transfer transaction to the contract's address.
    // When the transaction is confirmed, they can credit their account on the side-chain.
    // A transfer done by a contract isn't valid (it's just free money sent to the contract).
    // A valid deposit must be done as a transfer transaction.
    // This constraint allows for much lower GAS costs for the same guarantee.
    //
    //event Deposited(address _recipient, uint _value, bytes memo);
    //function deposit(bytes memo) public {
    //    emit Deposited(msg.sender, msg.value, memo);
    //}


    // STATE UPDATE

    // struct StateUpdateClaim {
    //     address _facilitator; // account of the facilitator making the claim for his side-chain
    //     bytes32 _new_state; // new state of the side-chain
    // }

    function claim_state_update(bytes32 _new_state) public {
        make_claim(keccak256(abi.encodePacked(ClaimType.STATE_UPDATE, msg.sender, _new_state)));
    }


    // WITHDRAWALS

    function withdrawal_claim_data(
        address _account, // account making the claim
        uint64 _ticket, // claimed ticket number (revision in the side chain)
        uint _value, // claimed value in the ticket
        uint _bond, // bond deposited with the claim
        bytes32 _confirmed_state) // digest of a confirmed state of the side-chain
            private pure returns(bytes32) {
        return keccak256(abi.encodePacked(_account, _ticket, _value, _bond, _confirmed_state));
    }

    function withdrawal_claim(
        address _facilitator, address _account,
        uint64 _ticket, uint _value, uint _bond, bytes32 _confirmed_state)
            private pure returns(bytes32) {
        return digest_claim(
                _facilitator, ClaimType.WITHDRAWAL_CLAIM,
                withdrawal_claim_data(_account, _ticket, _value, _bond, _confirmed_state));
    }

    // TODO: The cost of a legal argument in gas should be statically deduced
    // from the structure of the contract itself.
    int maximum_withdrawal_challenge_gas = 100*1000;

    function claim_withdrawal(address _facilitator, uint64 _ticket, uint _value, bytes32 _confirmed_state)
            public payable {
        require_bond(msg.value, maximum_withdrawal_challenge_gas);
        make_claim(withdrawal_claim(
            _facilitator, msg.sender, _ticket, _value, msg.value, _confirmed_state));
    }

    // event Withdrawal(address facilitator, uint64 ticket);

    // Logging a Withdrawal event allows validators to reject double-withdrawal
    // without keeping the withdrawal claim alive indefinitely.
    function withdrawal_confirmation(address _facilitator, uint64 _ticket)
            private pure returns(bytes32) {
        return digest_claim(_facilitator, ClaimType.WITHDRAWAL, bytes32(_ticket));
    }

    function withdraw(address _facilitator, uint64 _ticket, uint _value, uint _bond, bytes32 _confirmed_state)
            public payable {
        // Consume a valid withdrawal claim.
        consume_claim(withdrawal_claim(
            _facilitator, msg.sender, _ticket, _value, _bond, _confirmed_state));

        // TODO: As a more elaborate measure that is cheaper on-chain but requires
        // more careful watching off-chain,
        // Log the withdrawal so future double-claim attempts can be duly rejected.
        //        emit Withdrawal(_facilitator, _ticket);
        // Until we get the verification of logged events just right, here we make do with
        // the more expensive (in gas) but simpler solution of consume_claim storing
        // an unexpirable entry for the withdrawal.

        // NB: Always transfer money LAST!
        // TODO: Should we allow a recipient different from the sender?
        msg.sender.transfer(_value + _bond);
    }


    // TODO: challenges and counter-challenges for withdrawal (and then for all other claims)
    //
    // proof (tree, with + for disjunction, * for conjunction)
    // + the claimed confirmed offered state isn't valid:
    //    + it is not present as an active claim with a validity date in the past.
    //    + the known preimage to the claim, shown as evidence, does not start with STATE_UPDATE.
    //      NB: if the claim is in the data base, the preimage comes from some public transaction.
    // + The _ticket number is too new compared to side-chain revision of latest confirmed state.
    //    * as evidence, a preimage for the state,
    //    * the maximum revision of which is less than the ticket.
    // + The actual ticket entry content does not match:
    //    * as evidence, a preimage for the state, a merkle tree path to the ticket entry,
    //    * and the ticket entry, which does not match the account and value
    // + The _ticket was already withdrawn:
    //    * as current evidence (temporary), check the claim in permanent storage.
    //   TODO: Instead,
    //    * as evidence, access the log entry for the Withdrawal
    //      https://ethereum.stackexchange.com/questions/49441/verifying-that-an-event-did-happen
    //    * a path to the log entry within a block
    //    * a path to the block from a known ethereum block
    //      (using https://github.com/amiller/ethereum-blockhashes if needs be)

    function challenge_withdrawal__confirmed_state_not_accepted (
        address _facilitator, address _account,
        uint64 _ticket, uint _value, uint _bond, bytes32 _confirmed_state)
        public {
        require(!is_claim_status_accepted(claim_status[_confirmed_state]));
        reject_claim(withdrawal_claim(_facilitator, _account, _ticket, _value, _bond, _confirmed_state));
        // LAST, send the bond as reward to the sender.
        // TODO: should we send only half the bond, and burn the rest and/or donate it to a foundation?
        msg.sender.transfer(_bond);
    }

    function challenge_withdrawal__confirmed_state_preimage_wrong_kind(
        address _facilitator, address _account,
        uint64 _ticket, uint _value, uint _bond, bytes32 _confirmed_state,
        address _preimage_facilitator, ClaimType _preimage_tag, bytes32 _preimage_data)
            public {
        require(_facilitator != _preimage_facilitator || _preimage_tag != ClaimType.STATE_UPDATE);
        require(_confirmed_state == digest_claim(_preimage_facilitator, _preimage_tag, _preimage_data));
        reject_claim(withdrawal_claim(
            _facilitator, _account, _ticket, _value, _bond, _confirmed_state));
        // LAST, send the bond as reward to the sender.
        // TODO: should we send only half the bond, and burn the rest and/or donate it to a foundation?
        msg.sender.transfer(_bond);
    }

}

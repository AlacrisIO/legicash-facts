pragma solidity ^0.5.2;
//pragma experimental ABIEncoderV2;

import "claims.sol";
import "claimtypes.sol";
import "bonds.sol";
import "ethereum-blocks.sol";

/**
 * Contract for a number of operator using the same court registry
 */
contract Operators is Claims, ClaimTypes, Bonds, EthereumBlocks {

    // DEPOSITS
    //
    // See operator-fallback.sol for an alternate strategy for deposits, not currently implemented.
    // Question: should we allow the depositor to specify the recipient as well, for a few extra GAS?
    //
    event Deposited(address _operator, address _recipient, uint _value, bytes memo);
    function deposit(address _operator, bytes memory memo) public payable {
            emit Deposited(_operator, msg.sender, msg.value, memo);
    }

    // STATE UPDATE

    /* TODO: include a bond with this and every claim */
    function claim_state_update(bytes32 _new_state, uint _bond) public payable {
        make_claim(digest_claim(msg.sender, ClaimType.STATE_UPDATE, _new_state), _bond);
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
        address _operator, address _account,
        uint64 _ticket, uint _value, uint _bond, bytes32 _confirmed_state)
            private pure returns(bytes32) {
        return digest_claim(_operator, ClaimType.WITHDRAWAL_CLAIM, _confirmed_state);
    }

    // TODO: The cost of a legal argument in gas should be statically deduced
    // from the structure of the contract itself.
    int maximum_withdrawal_challenge_gas = 100*1000;

    function claim_withdrawal(address _operator, uint64 _ticket, uint _value, bytes32 _confirmed_state)
            public payable {
        require_bond(msg.value, maximum_withdrawal_challenge_gas);
	
        make_claim(withdrawal_claim(
            _operator, msg.sender, _ticket, _value, msg.value, _confirmed_state), msg.value);
    }

    event Withdrawal(address operator, uint64 ticket);

    function withdraw(address _operator, uint64 _ticket, uint _value, uint _bond, bytes32 _confirmed_state)
            public {
        // Consume a valid withdrawal claim.
        consume_claim(withdrawal_claim(
            _operator, msg.sender, _ticket, _value, _bond, _confirmed_state));

        // Log the withdrawal so future double-claim attempts can be duly rejected.
        emit Withdrawal(_operator, _ticket);

        // NB: Always transfer money LAST!
        msg.sender.transfer(_value + _bond);
    }


    // TODO: challenges and counter-challenges for withdrawal (and then for all other claims)

    /**
     * Challenge a withdrawal claim because its confirmed_state isn't accepted as valid.
     */
    function challenge_withdrawal__confirmed_state_not_accepted (
        address _operator, address _account,
        uint64 _ticket, uint _value, uint _bond, bytes32 _confirmed_state)
            public {
        require(!is_claim_status_accepted(claim_status[_confirmed_state]));
        reject_claim(withdrawal_claim(_operator, _account, _ticket, _value, _bond, _confirmed_state));
        // LAST, send the bond as reward to the sender.
        // TODO: should we send only half the bond, and burn the rest and/or donate it to a foundation?
        msg.sender.transfer(_bond);
    }

    /**
     * Challenge a withdrawal claim because its confirmed_state isn't actually a state update.
     *
     * Parameters from _operator to _confirmed_state describe the claim being disputed.
     * Parameters from _preimage_operator to _preimage_data describe a preimage to _confirmed_state,
     * that fail to match a state update.
     */
    function challenge_withdrawal__confirmed_state_not_state(
        address _operator, address _account,
        uint64 _ticket, uint _value, uint _bond, bytes32 _confirmed_state,
        address _preimage_operator, ClaimType _preimage_tag, bytes32 _preimage_data)
            public {
        require(_operator != _preimage_operator || _preimage_tag != ClaimType.STATE_UPDATE);
        require(_confirmed_state == digest_claim(_preimage_operator, _preimage_tag, _preimage_data));
        reject_claim(withdrawal_claim(
            _operator, _account, _ticket, _value, _bond, _confirmed_state));
        // LAST, send the bond as reward to the sender.
        msg.sender.transfer(_bond);
    }

    /**
     * Challenge a withdrawal claim because its confirmed_state doesn't contain that big a ticket number.
     */
    function challenge_withdrawal__ticket_number_too_large(
        address _operator,
        address _account,
        uint64 _ticket,
        uint _value,
        uint _bond,
        bytes32 _confirmed_state,
        bytes32 _previous_side_chain_state,
        uint64 _operator_revision)
            public {
        require(_operator_revision < _ticket);
        require(_confirmed_state ==
            digest_claim(_operator, ClaimType.STATE_UPDATE,
                    _previous_side_chain_state));
        reject_claim(withdrawal_claim(
            _operator, _account, _ticket, _value, _bond, _confirmed_state));
        msg.sender.transfer(_bond);
    }

    /**
     * Challenge a withdrawal claim because the ticket number doesn't correspond to a withdrawal.
     *
     * Parameters from _operator to _confirmed_state describe the claim being disputed.
     * Parameters afterwards exhibit the ticket at given number, which is of the wrong subtype.
     */
    function challenge_withdrawal__ticket_not_withdrawal(
        address _operator,
        address _account,
        uint64 _ticket,
        uint _value,
        uint _bond,
        bytes32 _confirmed_state,
        bytes32 state_bits)
            public {
        require(_confirmed_state ==
            digest_claim(_operator, ClaimType.STATE_UPDATE,state_bits));
        reject_claim(withdrawal_claim(
            _operator, _account, _ticket, _value, _bond, _confirmed_state));
        msg.sender.transfer(_bond);
    }

    /**
     * Challenge a withdrawal claim because the actual ticket doesn't match the claim
     *
     * Parameters from _operator to _confirmed_state describe the claim being disputed.
     * Parameters afterwards exhibit the ticket at given number, which fails to match the claim.
     */
    function challenge_withdrawal__ticket_mismatch(
        address _operator,
        address _account,
        uint64 _ticket,
        uint _value,
        uint _bond,
        bytes32 _confirmed_state,
        bytes32 state_bits)
            public {
        require(_confirmed_state ==
            digest_claim(_operator, ClaimType.STATE_UPDATE,state_bits));
        reject_claim(withdrawal_claim(
            _operator, _account, _ticket, _value, _bond, _confirmed_state));
        msg.sender.transfer(_bond);
    }

    /**
     * Challenge a withdrawal claim because the ticket was already withdrawn.
     *
     * For now, we don't need this function because the withdrawal_claim is consumed,
     * then currently remains in storage to prevent new withdrawal attempts.
     * As evidence (for now), check for the claim in permanent storage.
     *
     * TODO: Instead, as evidence, access the log entry for the Withdrawal
     *     https://ethereum.stackexchange.com/questions/49441/verifying-that-an-event-did-happen
     * a path to the log entry within a block
     * a path to the block from a known ethereum block
     *     (using https://github.com/amiller/ethereum-blockhashes if needs be)
     */
    /*
    function challenge_withdrawal__ticket_already_withdrawn(
        address _operator,
        address _account,
        uint64 _ticket,
        uint _value,
        uint _bond,
        bytes32 _confirmed_state)
            public {
        // TODO: complete this thing XXXXXX
        reject_claim(withdrawal_claim(
            _operator, _account, _ticket, _value, _bond, _confirmed_state));
        // LAST, send the bond as reward to the sender.
        msg.sender.transfer(_bond);
    }
    */
}

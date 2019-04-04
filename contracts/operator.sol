pragma solidity ^0.5.2;
//pragma experimental ABIEncoderV2;

import "claims.sol";
import "claimtypes.sol";
import "bonds.sol";

/**
 * Contract for a number of operator using the same court registry
 */
contract Operators is Claims, ClaimTypes, Bonds {

    // DEPOSITS
    //
    // See operator-fallback.sol for an alternate strategy for deposits, not currently implemented.
    // Question: should we allow the depositor to specify the recipient as well, for a few extra GAS?
    //
    // TODO: We need to add the "memo" entry back.
    // TODO: The balance needs to be removed eventually as it is for debugging purposes.
    event Deposited(address _operator, address _recipient, uint256 _value, uint256 _balance);
    function deposit(address _operator) external payable {
            emit Deposited(_operator, msg.sender, msg.value, address(this).balance);
    }

    // STATE UPDATE

    event StateUpdate(address _operator, bytes32 _confirmed_state, uint256 _balance);

    /* TODO: include a bond with this and every claim */
    function claim_state_update(bytes32 _new_state) external {
        make_claim(digest_claim(msg.sender, ClaimType.STATE_UPDATE, _new_state));
        emit StateUpdate(msg.sender, _new_state, address(this).balance);
    }

    function operator_state(
        bytes32 _previous_main_chain_state,
        bytes32 _previous_side_chain_state,
        uint64 _operator_revision,
        uint256 _spending_limit,
        uint256 _bond_posted,
        bytes32 _accounts,
        bytes32 _operations,
        bytes32 _main_chain_transactions_posted)
            private pure returns(bytes32) {
            return keccak256(abi.encodePacked(
                _previous_main_chain_state,
                _previous_side_chain_state,
                _operator_revision,
                _spending_limit,
                _bond_posted,
                _accounts,
                _operations,
                _main_chain_transactions_posted));
    }


    // WITHDRAWALS

    function withdrawal_claim_data(
        address _account, // account making the claim
        uint64 _ticket, // claimed ticket number (revision in the side chain)
        uint256 _value, // claimed value in the ticket
        uint256 _bond, // bond deposited with the claim
        bytes32 _confirmed_state) // digest of a confirmed state of the side-chain
            private pure returns(bytes32) {
        return keccak256(abi.encodePacked(_account, _ticket, _value, _bond, _confirmed_state));
    }

    function withdrawal_claim(
        address _operator, address _account,
        uint64 _ticket, uint256 _value, uint256 _bond, bytes32 _confirmed_state)
            private pure returns(bytes32) {
        return digest_claim(
                _operator, ClaimType.WITHDRAWAL_CLAIM,
                withdrawal_claim_data(_account, _ticket, _value, _bond, _confirmed_state));
    }
//                _operator, ClaimType.WITHDRAWAL_CLAIM, _confirmed_state);
//                withdrawal_claim_data(_account, _ticket, _value, _bond,
//                withdrawal_claim_data(_account, _ticket, _value, _bond, _confirmed_state));

    // TODO: The cost of a legal argument in gas should be statically deduced
    // from the structure of the contract itself.
    // TODO the balance needs to be removed eventually from the code because it is here for debugging
    uint256 maximum_withdrawal_challenge_gas = 100*1000;

    event ClaimWithdrawal(address _operator, uint64 _ticket, uint256 _value, bytes32 _confirmed_state, uint256 _bond, uint256 _balance);

    function claim_withdrawal(address _operator, uint64 _ticket, uint256 _value, bytes32 _confirmed_state)
            external payable {
        bool test=is_bond_ok(msg.value, maximum_withdrawal_challenge_gas);
        if (test) {
          make_claim(withdrawal_claim(
              _operator, msg.sender, _ticket, _value, msg.value, _confirmed_state));
          emit ClaimWithdrawal(_operator, _ticket, _value, _confirmed_state, msg.value, address(this).balance);
        }
    }

    event Withdrawal(address _operator, uint64 _ticket, uint256 _value, uint256 _bond, bytes32 _confirmed_state);

    // Logging a Withdrawal event allows validators to reject double-withdrawal
    // without keeping the withdrawal claim alive indefinitely.
    function withdrawal_confirmation(address _operator, uint64 _ticket)
            private pure returns(bytes32) {
        return digest_claim(_operator, ClaimType.WITHDRAWAL, bytes32(uint256(_ticket)));
    }



    function withdraw(address _operator, uint64 _ticket, uint256 _value, uint256 _bond, bytes32 _confirmed_state)
            external {
        bytes32 claim = withdrawal_claim(
            _operator, msg.sender, _ticket, _value, _bond, _confirmed_state);
        if (is_claim_status_accepted(claim)) {
          // Consume a valid withdrawal claim.
          set_claim_consumed(claim);

          // Log the withdrawal so future double-claim attempts can be duly rejected.
          emit Withdrawal(_operator, _ticket, _value, _bond, _confirmed_state);

          // NB: Should we always transfer money LAST! ?
          // I am not sure this is such a good idea
          // TODO: Should we allow a recipient different from the sender?
          msg.sender.transfer(_value + _bond);
        }
    }


    // TODO: challenges and counter-challenges for withdrawal (and then for all other claims)

    /**
     * Challenge a withdrawal claim because its confirmed_state isn't accepted as valid.
     */
     /*
    function challenge_withdrawal__confirmed_state_not_accepted (
        address _operator, address _account,
        uint64 _ticket, uint256 _value, uint256 _bond, bytes32 _confirmed_state)
        public {
        require(!is_claim_status_accepted(_confirmed_state));
        reject_claim(withdrawal_claim(_operator, _account, _ticket, _value, _bond, _confirmed_state));
        // LAST, send the bond as reward to the sender.
        // TODO: should we send only half the bond, and burn the rest and/or donate it to a foundation?
        msg.sender.transfer(_bond);
    }*/

    /**
     * Challenge a withdrawal claim because its confirmed_state isn't actually a state update.
     *
     * Parameters from _operator to _confirmed_state describe the claim being disputed.
     * Parameters from _preimage_operator to _preimage_data describe a preimage to _confirmed_state,
     * that fail to match a state update.
     */
     /*
    function challenge_withdrawal__confirmed_state_not_state(
        address _operator, address _account,
        uint64 _ticket, uint256 _value, uint256 _bond, bytes32 _confirmed_state,
        address _preimage_operator, ClaimType _preimage_tag, bytes32 _preimage_data)
            public {
        require(_operator != _preimage_operator || _preimage_tag != ClaimType.STATE_UPDATE);
        require(_confirmed_state == digest_claim(_preimage_operator, _preimage_tag, _preimage_data));
        reject_claim(withdrawal_claim(
            _operator, _account, _ticket, _value, _bond, _confirmed_state));
        // LAST, send the bond as reward to the sender.
        // TODO: should we send only half the bond, and burn the rest and/or donate it to a foundation?
        msg.sender.transfer(_bond);
    }*/

    /**
     * Challenge a withdrawal claim because its confirmed_state doesn't contain that big a ticket number.
     */
     /*
    function challenge_withdrawal__ticket_number_too_large(
        address _operator,
        address _account,
        uint64 _ticket,
        uint256 _value,
        uint256 _bond,
        bytes32 _confirmed_state,
        bytes32 _previous_main_chain_state,
        bytes32 _previous_side_chain_state,
        uint64 _operator_revision,
        uint256 _spending_limit,
        uint256 _bond_posted,
        bytes32 _accounts,
        bytes32 _operations,
        bytes32 _main_chain_transactions_posted)
            public {
        require(_operator_revision < _ticket);
        require(_confirmed_state ==
            digest_claim(_operator, ClaimType.STATE_UPDATE,
                operator_state(
                    _previous_main_chain_state,
                    _previous_side_chain_state,
                    _operator_revision,
                    _spending_limit,
                    _bond_posted,
                    _accounts,
                    _operations,
                    _main_chain_transactions_posted)));
        reject_claim(withdrawal_claim(
            _operator, _account, _ticket, _value, _bond, _confirmed_state));
        // LAST, send the bond as reward to the sender.
        msg.sender.transfer(_bond);
    }*/

    /*
    function state_bits_hash(bytes32[] memory state_bits) public pure returns (bytes32) {
      return keccak256(abi.encodePacked(state_bits));
    }*/

    /**
     * Challenge a withdrawal claim because the ticket number doesn't correspond to a withdrawal.
     *
     * Parameters from _operator to _confirmed_state describe the claim being disputed.
     * Parameters afterwards exhibit the ticket at given number, which is of the wrong subtype.
     */
     /*
    function challenge_withdrawal__ticket_not_withdrawal(
        address _operator,
        address _account,
        uint64 _ticket,
        uint256 _value,
        uint256 _bond,
        bytes32 _confirmed_state,
        bytes32[] memory state_bits,
        bytes32[] memory _merkle_path_in_operations
        // TODO: side-chain operation support
            )
            public {
        // NB: This is largely a stub. TODO: Actually implement the function.
        require(_confirmed_state ==
            digest_claim(_operator, ClaimType.STATE_UPDATE,
                         state_bits_hash(state_bits)));
        bytes32 operations = state_bits[6]; // TODO: make sure that's correct!
        // TODO: complete this thing XXXXX
        _merkle_path_in_operations; operations;
        reject_claim(withdrawal_claim(
            _operator, _account, _ticket, _value, _bond, _confirmed_state));
        // LAST, send the bond as reward to the sender.
        msg.sender.transfer(_bond);
    }*/

    /**
     * Challenge a withdrawal claim because the actual ticket doesn't match the claim
     *
     * Parameters from _operator to _confirmed_state describe the claim being disputed.
     * Parameters afterwards exhibit the ticket at given number, which fails to match the claim.
     */
     /*
    function challenge_withdrawal__ticket_mismatch(
        address _operator,
        address _account,
        uint64 _ticket,
        uint256 _value,
        uint256 _bond,
        bytes32 _confirmed_state,
        bytes32[] memory state_bits,
        bytes32[] memory _merkle_path_in_operations
        // TODO: side-chain operation support
            )
            public {
        // NB: This is largely a stub. TODO: Actually implement the function.
        require(_confirmed_state ==
            digest_claim(_operator, ClaimType.STATE_UPDATE,
                         state_bits_hash(state_bits)));
        bytes32 operations = state_bits[6]; // TODO: make sure that's correct!
        // TODO: complete this thing XXXXX
        _merkle_path_in_operations; operations;
        reject_claim(withdrawal_claim(
            _operator, _account, _ticket, _value, _bond, _confirmed_state));
        // LAST, send the bond as reward to the sender.
        msg.sender.transfer(_bond);
    }*/

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
        uint256 _value,
        uint256 _bond,
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

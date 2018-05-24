pragma solidity ^0.4.23;

/** Contract for a facilitator
   Contract initialization arguments:
  Operator: address of the side-chain operator, who alone may update the side_chain_state_digest
  */
contract Facilitator {

    address operator;

    // Generic support for claims. This ought to be moved to a library.

    // This mapping contains *contestable* active claims about this contract,
    // that are subject to challenge before they can be accepted as facts.
    // (TODO: It may also contain facts that can help resolve future transactions immediately.)
    // By contrast, incontestable facts can just be logged as events, which is much cheaper to write.
    // (It is much more expensive to read or consult a log entry, but that cost is paid for
    // by the wrongful party, not by rightful participants.)
    // To each ongoing claim is associated a non-zero validity date (in seconds since epoch)
    // after which it is considered fact, if left unchallenged or unsatisfactorily challenged.
    // If the validity date if not zero. If the date is in the future, the fact may be challenged
    // and removed from the set of accepted facts.
    // TODO: to handle third-party litigation, etc., we may have to store more than just a validity date.
    // Or can we do that entirely with parallel claims?
    mapping(bytes32 => uint) public active_claims;

    // Is this side-chain active? If not, there won't be updates or expiries anymore.
    // There can still be claims, though.
    // Particularly for involuntary exits within the liquidation window.
    bool active;

    /** Make a claim
        Usage Pattern: make_claim(keccak256(abi.encodePacked(x))).
        Unhappily, that pattern can't be made a function (bad types) and can't be made a macro (no macros).
     */
    function make_claim(bytes32 claim) private {
        require(active_claims[claim]==0);
        active_claims[claim] = now + challenge_period_in_seconds;
    }

    // Check that a claim is valid
    function check_claim(bytes32 claim) private view {
        uint valid_time = active_claims[claim];
        require(valid_time > 0 && valid_time <= now);
    }

    // Check that a claim is valid, then make it valid no more.
    function consume_claim(bytes32 claim) private {
        check_claim(claim);
        active_claims[claim] = 0;
    }

    // Counter a claim, pushing back its validity date.
    // TODO: can there be multiple periods before the timeout?
    // TODO: if all counter-claims are resolved early and the initial timeout is passed,
    // shouldn't the original claim then become valid immediately?
    // TODO: figure out how third-party litigation affects validity times and challenge timeouts.
    function counter_claim(bytes32 claim) private {
        uint valid_time = active_claims[claim];
        require(valid_time > now); // claim must still be active.
        uint challenge_timeout = now + challenge_period_in_seconds;
        if (valid_time < challenge_timeout) {
            active_claims[claim] = challenge_timeout;
        }
    }

    // Expiry delay, in seconds. Claims may disappear after this delay.
    // TODO: Make sure it's large enough.
    uint constant expiry_delay = 31 days;

    // Function called by the operator only, to forget about some old enough claim,
    // too old to be involved in an active lawsuit.
    // This allows the operator to get some gas refund as he makes a new update.
    function expire_old_claim(bytes32 _claim) private {
        require(active);
        require(msg.sender == operator);
        uint valid_time = active_claims[_claim];
        require(valid_time>0 && valid_time < now-expiry_delay);
        active_claims[_claim] = 0;
    }

    // One challenge period is 2h, about 423 blocks at the expected rate of 1 block/17 s.
    uint constant challenge_period_in_seconds = 2 hours;

    // The minimum bond for each operation should be low enough to be affordable,
    // yet large enough to cover the gas costs of an entire trial over the duration of
    // the challenge window.

    // The contract ought to comfortably and conservatively overestimate of the price of gas
    // for the duration of any upcoming legal argument.
    // TODO: Ideally, gas cost estimates should be dynamically computed from the environment.
    function get_gas_cost_estimate () pure private returns(uint) {
        // 100 shannon (= 100 gwei, .1 szabo),
        // a somewhat conservative value for May 2018 (when the median is about 10).
        // But NOT a future-proof value.
        // TODO: dynamic configuration? Migration before it gets too bad?
        return 100*1000*1000*1000 wei;
    }

    // TODO: The cost of a legal argument in gas should be statically deduced
    // from the structure of the contract itself.
    uint maximum_withdrawal_challenge_gas = 100*1000;

    // NB: We assume no overflow(!)
    // If the gas_cost_estimate becomes astronomically high, this is a vulnerability.
    function minimum_bond(uint maximum_gas) pure private returns(uint) {
        return maximum_gas*get_gas_cost_estimate();
    }


    // DEPOSITS

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

    // TODO: I tried to define a struct but solidity won't let me hash it:
    // "Error: This type cannot be encoded.",
    // so instead I fell back to doing my own poor man's struct.
    // StateUpdateClaim { bytes32 state; },

    uint8 constant STATE_UPDATE = 1;

    // Function called by the operator only, to update the new state.
    // This is a claim only, because users may dispute its validity.
    function claim_state_update(bytes32 _new_state) public {
        require(active);
        require(msg.sender == operator);
        make_claim(keccak256(abi.encodePacked(STATE_UPDATE, _new_state)));
    }


    // WITHDRAWALS

    // struct WithdrawalClaim {
    //     address _account; // account making the claim
    //     uint64 _ticket; // claimed ticket number (revision in the side chain)
    //     uint _value; // claimed value in the ticket
    //     uint _bond; // bond deposited with the claim
    //     bytes32 _confirmed_state; // digest of a confirmed state of the side-chain
    // }

    uint8 constant WITHDRAWAL_CLAIM = 2;

    // Logging a Withdrawal event allows validators to reject double-withdrawal
    // without keeping the withdrawal claim alive indefinitely.
    event Withdrawal(uint64 _ticket);

    function claim_withdrawal(uint64 _ticket, uint _value, bytes32 _confirmed_state)
            public payable {
        // Check that a sufficient bond was included in the message.
        require(msg.value >= minimum_bond(maximum_withdrawal_challenge_gas));

        // Make the claim, assuming it wasn't made yet.
        make_claim(keccak256(abi.encodePacked(
            WITHDRAWAL_CLAIM, msg.sender, _ticket, _value, msg.value, _confirmed_state)));
    }

    function withdraw(uint64 _ticket, uint _value, uint _bond, bytes32 _confirmed_state)
            public payable {
        // Consume a valid withdrawal claim.
        consume_claim(keccak256(abi.encodePacked(
            WITHDRAWAL_CLAIM, msg.sender, _ticket, _value, _bond, _confirmed_state)));

        // Log the withdrawal so future claim attempts can be duly rejected.
        emit Withdrawal(_ticket);

        // NB: Always transfer money LAST!
        // TODO: Should we allow a recipient different from the sender?
        msg.sender.transfer(_value + _bond);
   }


   // TODO: challenges and counter-challenges!
   function challenge_withdrawal (
       address _account, uint _ticket, uint _value, uint _bond, bytes32 _confirmed_state, bytes _proof)
       public payable {
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
       //    * as evidence, the log entry for the Withdrawal
       //    * a path to the log entry within a block
       //    * a path to the block from a known ethereum block
       //        (using https://github.com/amiller/ethereum-blockhashes if needs be)
       bytes32 claim = keccak256(abi.encodePacked(
           WITHDRAWAL_CLAIM, _account, _ticket, _value, _bond, _confirmed_state));
       // TODO: implement this method!
       require(false);
       _proof;
       counter_claim(claim);
   }
}

// Matrix contract that generates contracts and remembers its offsprings.
// NB: For involuntary transfer, beware to only use offsprings
// that have compatible court registries.
// Initialization argument: the signatures that make the court registry.
contract Matrix {

}

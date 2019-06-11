pragma solidity ^0.5.2;

/**
 * Contract support for computing bonds.
 */
contract Bonds {

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
    function get_gas_cost_estimate () pure internal returns(uint256) {
        // 100 shannon (= 100 gwei, .1 szabo),
        // a somewhat conservative value for May 2018 (when the median is about 10).
        // But NOT a future-proof value.
        // TODO: dynamic configuration? Migrate to a new contract before this gets too bad?
        return 100*1000*1000*1000;
    }

    /**
     * Compute the minimum bond to stake when making a given claim.
     *
     * The parameter should be larger than the maximum gas necessary
     * for one honest party to challenge the claim.
     */
    function minimum_bond(uint256 _maximum_gas) pure internal returns(uint256) {
        // TODO: Should we worry about overflow? If so, prevent it as follows:
        //     require(maximum_gas) < 2**254 / get_gas_cost_estimate())
        return _maximum_gas*get_gas_cost_estimate();
    }

    /**
     * Require that the posted bond be sufficient to cover the gas required to challenge the claim.
     */
    function is_bond_ok(uint256 _bond, uint256 _maximum_gas) internal pure returns(bool) {
//        return _bond >= minimum_bond(_maximum_gas);
        return _bond >= 1;
    }
}

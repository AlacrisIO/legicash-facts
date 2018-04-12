# Invariants

Things we'd like to prove about our code.

## Side-chain Well-formedness

We will specify what it means for a side-chain to be well-formed,
including its relationship to the main-chain:

  * Schema validation (ML types): the type of the side-chain state is correct by construction
    * all the transitive digests point to data of the correct type
    * each update contains a new current state and a list of transactions from the previous state
    * the state is patricia merkle trie of accounts mapped to account states
  * Fine structure validation (dependent types):
    * account balances are non-negative
    * account balances sum up to less than the total number of tokens (less than the total on the contract)
    * transaction numbers are consecutive
    * timestamps are increasing
    * transactions preserve total token amount in the accounts at stake
    * transactions and requests are properly signed by the proper participants
    * authorization only happens on active accounts
    * updates indeed lead from the previous state to the current state
  * Consistency:
    * No two pieces of conflicting history are ever found (i.e. no double-spending)
      (in the gossip network, on the main chain, etc.)
  * Relationship with main chain:
    * the state of the side-chain is regularly updated on the main chain
    * the revision of side-chain updates is non-decreasing
    * a recent state of the main-chain is referred to by the side-chain and its transactions
    * the referred states of the main-chain are ones where the suitable state of the side-chain was updated
    * the referred state of the main-chain is non-decreasing
    * account balances sum up to less than the total on the contract
    * all confirmed main-chain transactions are accounted for in the side-chain within a timeout
  * Relationship with user chain:
    * the state of the user-account is always making progress


## Facilitator Repudiation

We will specify the protocol for adversarial repudiation of a facilitator.
It will be partly accounted for as part of well-formedness above,
but it also works when the side-chain updates cease to be well-formed.

Voluntary repudiation: at any point, disgruntled users can either exit on the main chain,
or entrust another facilitator (using the exact same facilitation contract) with exit.
When the side-chain isn't well-formed anymore, or otherwise ceases to function,
an additional involuntary repudiation takes place.

A simple exit from a single user to the main chain is functionally equivalent
to a general mass exit with only a single, voluntary, participant.
A mass exit takes several

takes the form of
an interactive proof that the XXXXXX
If the user has pending transactions, he must first wait for the facilitator to post its last update,
or to fail in which case adversarial updates will be posted.
Only then will the user be able to do an exit.


## Code Extraction

We will extract OCaml code that facilitators and users use to maintain their respective chains
and post updates to the main chain.

We will define "good strategies" and "good contexts" for using the code.
We will prove that the code if used according to a good strategy in a good context,
then it maintains the invariants specified above.


## Contract Extraction

We will extract code in Michelson (Tezos contract VM) and/or EVM (Ethereum contract VM)
that serves as referee for verification of the invariants using Game Semantics.
Those who break their promises will be sanctioned.

Additional complication that we will take into account in our specification:
third parties may offer better arguments than a failing party.
A party may either time out or present a losing argument even though a winning one exists;
in these cases, the party is consider to be failing, and
third parties may declare themselves litigant and offer better arguments.
This way, dishonest or failing parties (notably facilitators) cannot let the bad guys win
and get away with other people's money.
When a party is found to have used a suboptimal argument, they are punished.
If they are facilitators, they "lose their license" at which point
the involuntary mass exit process starts.

Multiple third parties may be contesting the trial, and it might not be clear to the judge
until the very end which is the honest party, so all third party ligitants are accepted.
However, there is a strict ordering between the third party arguments
so only the first litigant wins the prize.
Those who failed to arrive first because of a race condition can promptly drop out of the race,
or will have to pay extra legal fees if they keep their losing side-trial open longer than necessary.


## Strategy Extraction

We will extract OCaml code that plays the verification game following an optimal strategy
for each party: user, facilitator, other facilitator, and independent verifier

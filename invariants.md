# Invariants

Things we'd like to prove about our code.

## Side-chain Well-formedness

We will specify what it means for a side-chain to be well-formed, including its
relationship to the main-chain:

  * Schema validation (ML types): the type of the side-chain state is correct by
    construction
    * all the transitive digests point to data of the correct type
    * each update contains a new current state and a list of transactions from
      the previous state
    * the state is patricia merkle trie of accounts mapped to account states
    * ideally, from a common DSL we can generate at the same time in-logic
      (Coq), in-language (ML), on-network and efficient on-disk representations,
      for all the blockchains that we may want to link together (including all
      the cryptocurrencies that matter: Bitcoin, Ethereum, Ripple, Bitcoin Cash,
      Litecoin, Cardano, Tezos, EOS, Stellar, Monero, Zcash, etc.)
  * Fine structure validation (dependent types for each blockchain in
    isolation):
    * account balances are non-negative
    * account balances sum up to less than the total number of tokens (less than
      the total on the contract)
    * transaction numbers are consecutive
    * timestamps are increasing
    * transactions preserve total token amount in the accounts at stake
    * transactions and requests are properly signed by the proper participants
    * authorization only happens on active accounts
    * updates indeed lead from the previous state to the current state
  * Consistency:
    * No two pieces of conflicting history are ever found (i.e. no
      double-spending) (in the gossip network, on the main chain, etc.)
  * Relationship with main chain:
    * the state of the side-chain is regularly updated on the main chain
    * the revision of side-chain updates is non-decreasing
    * a recent state of the main-chain is referred to by the side-chain and its
      transactions
    * the referred states of the main-chain are ones where the suitable state of
      the side-chain was updated
    * the referred state of the main-chain is non-decreasing
    * account balances sum up to less than the total on the contract
    * all confirmed main-chain transactions are accounted for in the side-chain
      within a timeout
  * Relationship with user chain:
    * the state of the user-account is always making progress
  * Relationship with other side chains:
    * Takes part honestly in bankrupcy proceedings for and mass exits from other
      facilitators (see section below).

Note that if the side-chain data structure includes suitable indexes with
redundant copies of all the intermediate results of the relevant reductions
(folds), then all proofs of dysfunction can be done in one step without further
interaction or timeout. Note that many correctness properties could be asserted
non-interactively by the side-chain maintainer with recursive zkSNARKs (or
STARKs), but that would be extremely slow and costly for the maintainer to
constantly recompute, for little added benefit since some properties like
double-spending still require a proof challenge. One minor benefit is that it
might save some time and space for verifiers (not at all on the server) by
summarizing away all the ancillary data required to assert correctness
(signatures, intermediate results, etc.).


## Crucial Protocol Workflows

### Payment Settlement

When a user wants to transfer money out of one account on the main chain or a
facilitator side-chain into another account on the same chain or a different one
(which can also be the main chain or a side-chain), we will provide and specify
a protocol for that — except in the case of payment from the main chain to the
main chain, where we will only specify the protocol that is already provided by
the main chain developers.

We suppose that the specification of these payments may use some variant of
computability logic to model the preservation of resources. Some linear
disjunction operators will express choices that are available to specific
parties regarding those resources. Which party, user, facilitator or miner,
etc., can itself be specified as part of a conjunction operator that includes a
signature of the suitable party in the product of items part of the choice.

Similarly, when modeling liveness properties whereby all requests are eventually
resolved in bounded time, we will presumably need to introduce some variant of
temporal logic quantifiers to model lower or upper limits to the delays
involved.

Payments within a single chain use the chain's regular mechanism: the consensus
of the main chain, or the authoritative facilitator of the side chain. Payments
across multiple chains require some variant of two-phase commit, with suitable
timeouts in case one party fails to cooperate until the end. Dealing with a
dishonest or otherwise failing party can lead to funds being locked until the
timeout, but not to the other parties wholly losing their stake. The uncertainty
between multiple states of the incomplete transaction can there again be modeled
with linear disjunction operators expressing choice between alternatives chosen
by one party through either positive action or passive timeout.

Note that the possibilities introduced by these payment protocols, as well as by
all the protocols described below, must be accounted for in the model used for
the well-formedness of the chain, as previously discussed.


### Facilitator Repudiation

At any point, disgruntled users can close their account and either take their
funds to their account on the main chain, or entrust their funds to any other
facilitator using the exact same facilitation contract.

One potential issue with taking funds out is that it is not obvious to anyone
but the facilitator what transactions may or may not be pending that will or
will not make it to the facilitator's chain. The user may have posted requests
that the facilitator will confirm, and he may have posted requests that the
facilitator refuses to confirm. A dishonest facilitator might even hold some
unconfirmed requests as "hostage" so the user himself might not know what his
final balance is, at which point he can never request the correct amount. We
solve this issue by splitting facilitator repudiation in three steps.

First, the account is closed on the side-chain, and the user waits for this
closure is confirmed on the main chain. This first step, closing the account on
the side-chain, is normally done by the user privately sending an account
closing request to the facilitator, who publicly confirms it and includes it on
his side-chain. But what if the facilitator refuses to confirm the request and
include it on his side-chain? Or what if the facilitator demands an exorbitant
fee to close the account? What if the facilitator and the user have a
disagreement on some other matter? Then the user can post the closing request
publicly in a message to the contract on the main chain, or in the side-chain of
another facilitator who can afterwards in one such message post the simultaneous
closing requests of a large number of users. Posting such an adversarial closing
request will have the cost a regular transaction on the main chain or on the
sister chain of the other facilitator, but no cost on the chain of the accout
being closed, whose repudiated facilitator receives no fee. Once the closing
request is thus made public, the repudiated facilitator*must* confirm and close
the account within a short deadline, or be declared invalid and go through
liquidation as described in the section below. In the case of a collective
closing request from another facilitator, the repudiated facilitator also has
the option of demonstrating the closing request as invalid (if it indeed is), in
which case the other facilitator is the one that is declared invalid and
liquidated.

Second, now that it is officially known to the user and all participants that
the account is closed, that there are no more floating transactions, and what
the final balance of the user is, the user claims the balance. This claim can
again be done either individually, or as part of a mass exit, by the claiming
recipient (user or new facilitator) publicly invoking the contract on the main
chain. To prevent fraudulent double exits, or exits from invalid side-chains,
the funds are locked for a challenge period during which third parties may show
a proof-of-fraud and invalidate the exit.

Third and last, once the challenge period successfully times out without any
valid proof-of-fraud being posted, the user (or his new facilitator, if part of
a mass exit) can once again invoke the contract to actually get money from the
contract.

The entire process requires several transactions on the main chain and may take
many hours due to the many delays involved. But the user is guaranteed to be
able to get his money out of the contract, even without cooperation from the
repudiated facilitator. If the facilitator is cooperative and offers fees the
user is ready to afford, then the user can of course withdraw money faster by
using the regular settlement mechanism from the previous section. He may or may
not care whether his account on said facilitator remains open afterwards. If he
does care, he may close the account, with or without the facilitator's
cooperation.

The invariants to prove with respect to this protocol are that indeed, users can
always close their account and get their money out in bounded time, assuming
that at least one honest main chain miner will publish the exit transactions for
him and/or his new facilitator, but that users can only take out their money
once, and only the correct amount.


### Facilitator Dissolution

When a facilitator is proven to be invalid, it may not confirm anymore requests
and all accounts are considered closed.

However, there may be valid transactions floating, and somehow a final update
must be posted that includes all these valid transactions. A challenge period is
open for claims to be made, whereby people may post partial updates with
previously confirmed but unpublished transactions, based on the gossip network
or private confirmations. People may also be making claims of voluntary and
involuntary account transfers.

After the period expires, the first-comer can propose a final update to the
state of the chain including a definitive list of the voluntary and involuntary
transfers. The update must include all the transactions previously posted,
except that in case any double spending is detected, it must stop processing the
accounts that are involved in double-spending from the point that this spending
is detected and mark their funds for destruction. There may also be conflicting
but non-fraudulent transactions, whereby e.g. the fraudulent facilitator
declined to confirm some of a user's transactions, but later confirmed others;
as long as the transaction history is consistent with the user not cheating,
then the longest non-cheating history will be preferred.

Now, the proposed final update may itself be fraudulent, therefore it is itself
subject to a challenge period during which others may submit a proof of fraud
and an alternate final update, at which point the challenge period is reset.

After some final update proposal survives its challenge period, the chain is
closed and the last state is now known. The poster of the final update earns a
modest reward for it, that will covers his costs. Then comes a period whereby
users individually and collectively take their funds out, based on the confirmed
final update.

Invariants for this protocol will include well-formedness of all the data
structures, proof that any honest floating transactions not involving any
double-spending account will be honored, that the process doesn't itself involve
any violation of resource conservation laws, etc.


## Code Extraction

### Client and Server Code Extraction

We will extract OCaml code that facilitators and users use to maintain their respective chains
and post updates to the main chain.

We will define "good strategies" and "good contexts" for using the code.
We will prove that the code if used according to a good strategy in a good context,
then it maintains the invariants specified above.


## On-Chain Contract Extraction

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


### Strategy Extraction

We will extract OCaml code that verifies that parties to a contract are keeping their promises.
The software interface will use this verification to prevent user actions
that would result in breaking his promises
(at least not without clear warning and manual override).
The verification software will also help punish offenders
by playing the verification game following an optimal strategy for each party:
user, facilitator, other facilitator, and independent verifier.


## Model Resistance to Attacks

### 50% attack against Court Registry

For state to be considered valid, it must come with signatures from a quorum of 50% of the court registry.
Why 50%? Because let's assume the quorum is q.
Then, if proportion q of registrars collude with Trent,
then Trent can withhold blocks, and the system crumbles.
Meanwhile, if proportion 1-q of registrars collude against Trent,
then they can prevent Trent from creating blocks, and the system also crumbles.
To minimize both q and 1-q, the quorum q must be 50%.
And then, we have a 50% attack on the court registry.
For the q attack, non-working registrars don't count as attacking.
For the 1-q attack, non-working registrars do count as attacking.
If some proportion of registrars stop being trusted (10%? 25%?),
then all users of all facilitators must issue *voluntarily* mass exits.
Involuntary mass exits cannot help laggards in the case of failing registry.

As a mitigation, if a majority of facilitators (by account volume) agree that the registry is failing,
then facilitators using that registry can declare that after some date D sufficiently in the future,
use of the registry will not be accepted, and
only slow non-scaling exits to the main chain will be accepted.


### Advice from Atalay

Do not encode in the type an invariant that an adversary can break.
Keep correctness properties separate.

Have a concrete model of the adversaries.

Use Coq as early as possible for the model.

For pipelining, use some concurrent combinator monad.
But: prove equivalence to a sequential program.

Instead of implementing module per module, make progress in parallel, and
iterate to ensure compatibility between logic encodings.

Use Gallina as much as possible to let Coq do the work,
but use an embedded logic where necessary for extraction to game semantics.

Abstract Data Types for Coq:
(see modules in https://github.com/mit-pdos/fscq/blob/master/src/Balloc.v).
Basically reconstitute the inductive type as abstract things that Coq won't expand via reduction
which sometimes simplifies evaluation, but makes the code explode.

Read Chlipala's FRAP (introductory) and CPDT.

Look F* and SMT solvers instead of Coq (?)


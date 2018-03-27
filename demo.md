# Plan for our demo

## Flows to get right

We identified nine flows in Legicash FaCTS. We can stub most of them.

* create facilitator (stub?)
* close facilitator (stub)
* update facilitator sidechain state + pay penalties (stub?)
* open account (stub?)
* close account (need to do the adversarial kind)
* send payment (need to do)
* settle external payment (to/from main/side chain) (need to do; can cover open account?)
* denounce facilitator (minor / major) (stub)
* mass transfer account (voluntary / involuntary) (stub)

In the end, the three flows we really need to implement non-trivially for the demo are:

* payment
* settlement of main-chain vs side-chain swap (including account creation / deletion?)
* adversarial exit (with the simplest invariants only, both success / failure cases)

### Payment Flow

Actors: Alice (sender), Bob (recipient), Trent (facilitator).

Preconditions:
* Alice has an account on Trent's side-chain. The account is voluntary, and has balance X.
* Bob has an account on Trent's side-chain. The account is voluntary, and has balance Y.

Alice wants to send amount Z to Bob.
Trent charges a fee F.
Trent has a current limit of L.
Z+F <= X.
Z+F <= L.

Alice will send to Bob a "certified check".

1- Alice write a message M1, signed by her, that says:
  "Trent, please pay Z to Bob and receivee your fee F.
  My previous state with you was P.
  This message is written at time T0 (identified by main chain state M, side-chain state S).
  This message is valid until date T1. -- signed by Alice"

2- Alice sends message M1 to Trent for underwriting.

3- Trents verifies that M1 is correct, then signs a message M2 certifying M1:
  "I, Trent, certify M1 as valid and promise to make good on it in my next update to the main chain.
  Latest side-chain update was PS. My current TXID is T. My new limit for this cycle is NL."

4- Trent commits M2 to his side-chain.

5- Trent sends M2 to Alice (and Bob?)

6- Alice sends M2 to Bob.

7- Bob sends M2 to the Gossip Network, waits to check there is no double-spend by Trent.

8- Bob accepts the payment, and starts servicing Alice.

9- Trent updates the main chain and the payment is fully cleared.


## Other questions to get right

### Predicting court fees as prelude to determining bond.

### Getting the logic model right

The programming / logic model has to deal with
public databases that atomically include records,
and relations between consecutive states of these databases.

(How) can the contract "see" things outside of the contract state?
Does the Ethereum and/or Tezos API help with that?
If not (and/or not cheaply, easily, reliably, in the long run, etc.),
can we identify what API entry points Tezos or Ethereum would need to add?

A signature by a manager is a promise to include some data in the next version of the database.
How do we deal with failing managers who withhold signature on an ongoing transaction?

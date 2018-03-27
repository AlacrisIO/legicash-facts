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

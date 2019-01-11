Persistence
===========

This document explains our strategy to persist data against failures and attacks.


Centralized Server with Redundant Backups
-----------------------------------------

A given operator is a network of redundant servers,
wherein only one at a given time actually processes requests, sequentially,
and the other ones are backup servers ready to pick up the workload in a fail over event
in case the master becomes non responsive.

Master election: in V0 we can have operators manually select which server is THE master (yuck),
but eventually we want some kind of Paxos election for the leader.
A chaos monkey should aperiodically disrupt the master server and/or
backup servers and force a new master election.


Key Value Store
---------------

The master pushes all data in a distributed key value store.
We will use LevelDB for that:
it's developed by Google, it will scale up, yet scales down.

Values stored will be from a Marshallable.t type,
that we code and decode with marshal and unmarshal functions.
Keys also will be such, but key types will also have metadata that
allows to distinguish which tables we use, what versions and types they use, etc.
There are two kinds of keys: content-addressing and intent-addressing.

A content-addressing key concatenates a table identifier and a digest;
the table identifier also identifies the object's type, so we know how to (un)marshal it.
The table identifier could itself be a digest of the type description and other metadata.

An intent-addressing key looks more like the usual primary key of a database.
It also has a table identifier, but instead of the digest it is indexed by other data,
such as the operator address and the revision number of a confirmation, etc.


Request Pipeline
----------------

A operator's master machine processes requests as follows:

1. Receive the Request.
   Accept encrypted user connections, read and decrypt the bits.
   This is completely parallel, with no contention.
2. Purely validate the Request.
   Check that the Request is well-formed and makes sense in itself,
   without accessing any contextual information from any database.
   This is completely parallel, with no contention.
   Malformed and invalid requests are rejected.
3. Optimistically validate the Request.
   Check that the Request makes sense in the context of a recent read-only snapshot of the database.
   This is largely parallel, with a little bit of contention to acquire the snapshot.
   Requests that fail to validate are rejected.
   Users can retry a bit later if it's due to the snapshot being out-of-date.
4. Process the Request in a local transaction.
   A master thread processes all requests sequentially,
   but groups them in a batch of say all requests initiated in a given slice of 100 ms.
   Consecutive batches go through a pipeline through steps 4 and 5,
   such that no batch is complete through a stage until the previous batch has completed the same stage,
   but a batch can be started in earlier stages before the previous batch has completed later stages.
   This process is sequential and somewhat slow (a good fraction of a second?) but batched and pipelined.
   If signing is cheap, sign every confirmation as part of step 4.
   If signing is expensive, the protocol may require only one signature per batch,
   which if deterministic can be even committed to persistent storage as an extra step 7.
   In the latter case, we will have to communicate merkle proofs to clients,
   whether relative to the batch or to the entire side-chain.
5. Commit the Request to the distributed database.
   Send the request to all the other nodes in the network, and wait for a majority of them to confirm.
   This process is sequential and slow (up to a second?) but batched and pipelined.
6. Send confirmation to the users.
   This is completely parallel, with no contention.
7. (Optional, if signing is extra slow yet deterministic) Commit the signature to distributed storage.
   This is sequential, but with loose synchronization need, since it can be recomputed in case of crash.


Content References
------------------

When retrieving an object from the database, we won't eagerly page in all the transitive objects
that it refers to in memory. That would eat all the process RAM, filling it with mostly unused garbage,
Instead we shall use lazy references that load the objects on demand,
and weak references to the contents such that unused objects can be reclaimed at the next GC,
and/or after they grow too old.

Beware: a simple use of weak reference might reclaim too much or not enough, killing performance.
A naive use of locking for LRU-based eviction would also over synchronization and kill performance.
For better performance, using some PPX to define low-level memory pointers and accessors directly
on the marshaled representation might be best.
With low-level memory pointers, we could even use memory mapping over a large address space,
so that the objects are always mapped in memory, and
we let the underlying OS handle the paging out of objects.
LevelDB might even work this way.


Transactions
------------

We need some mechanism to control the atomicity of our operations.
I'm not sure exactly what that looks like from a pure functional programming point of view
(considering that we may or may not prove code correct in Coq).
Using monads everywhere can be painful, especially if filling in objects from caches is seen as an effect.

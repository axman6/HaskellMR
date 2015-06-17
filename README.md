# HaskellMR
A Haskell Map Reduce project designed from the ground up for distributed computation.

Early ideas on how to make the system below.

---
## High level ideas
* MR is clearly a monad, can we make use of that? 
  * MR should not be limited to a single map and reduce stage
  * Do we need to enforce a strict pipeline though?
  * Can later computations depend on (stored) data from computations not immediately preceding - fully monadic?
  * Can we enforce that reasults are monoidal (or semi-groups) so that reduction is implicit?
* Distributed srtorage is important, but do we need to store all stages of the computation? Can probably make this optional if there's enough RAM to store intermediary results - computations should be able to restart if somrthing goes wrong.
* Initially only allow pure computations - if we do this then the same computations can be run on any node which has the same data stored locally in case the computation node for the data goes offline. KV pairs should be identical
* For distributed storage, can we do something fancy with a Map k (FilePath, Weak ByteString) so the GC can take care of removing ByteStrings of objects if they aren't in use? Or should we do cachine manually? (Good chance this would lead to a high miss rate)

### Package notes
#### [raft](https://github.com/kfish/raft) - implementation of the Raft protocol, may be useful for distributed storage implementation (a la HDFS)
  * Play with implementing an AcidState based store
  * Investigate cluster membership, adding new members etc.

#### [Acid-state](http://acid-state.seize.it)
  * May be useful for Raft log and data store

#### [distributed-process](https://hackage.haskell.org/package/distributed-process) AKA Cloud Haskell
  * Already handles a lot of what's needed to 

### Design ideas
* Use Raft to distribute data to all nodes, only those who have been designated to store it do, others store hash so they have something to have concensus on - slow/high bandwidth, but should work as a first step
* Use Acid-state as initial data store
* All computations nodes are part of storage system, so they know who to send updates to
  * Is it possible to save to local node first, and have the leader not send the data back, only send the Raft metadata once consensus is reached?
  * perhaps storage and raft messages should be distinct
  * 

## Architecture diagram

    +----------------------------------------------+
    |       MR Computation- Reductions only        |
    |       run on one replica of the data         |
    +----------------------------------------------+
    +----------------------------------------------+
    |      Raft consensus for data replication     |
    |      replication and MR round agreement      |
    +----------------------------------------------+
    +---------------------+ +----------------------+
    | Acid|State raft log | |      Data store?     |
    +---------------------+ +----------------------+

(Made with http://asciiflow.com)

High-Level CCSTM Design Document
==============================

Guarantees
----------

- **Atomicity** - a transaction will appear to outside observers to have
  either not yet committed or completely committed.  Outside observers
  are other transactions and the non-transactional context.  This means
  that if a transaction updates both X and Y, and another observer sees
  the new value of X, a subsequent read of Y will definitely see the
  new value of Y.

- **Isolation** - no observer can see the results of a transaction that
  has not yet committed.

- **Consistency** - there exists a point in time at which all of the
  reads of a transaction could have occurred while producing the same
  values as those actually observed, even for transactions that are only
  partially completed.  Note that many STM algorithms do not guarantee
  this property for transactions that have not yet completed.

- **Strong isolation and atomicity** - The atomicity and isolation
  guarantees apply regardless of whether or not non-transactional access
  is made to data that is also accessed in a transaction.  This is a
  superset of privatization and publication safety.

- **Linearizability** - committed transactions and non-transactional
  accesses may be considered to have occurred at a single point in time,
  and for each thread the resulting time order is consistent with the
  order in which transactions and accesses by that thread were performed.
  Note that this is a stronger property than that provided by snapshot
  isolation or MVCC, since it guarantees not only that all reads see
  a consistent world view, but that the world view is still current at
  commit time.


Design Choices
--------------

- **Rollback on exception** - user exceptions in an atomic block trigger
  rollback without automatic retry, and are then rethrown.  This allows
  atomic blocks to be used to provide failure atomicity, although it is
  not clear that this is the most theoretically pure choice.


Features
--------

- **Retry** and **OrElse** - as in CMT.  The user should not have to
  specify the conditions under which retry is appropriate.

- **Nested transactions?** - not sure about this one.  Explicit retry
  of a nested transaction is tricky.  Nested transactions may be useful
  for failure atomicity, though.


Performance Goals
-----------------

- **Low uncontended per-transaction overhead** - if there are *N*
  processors (out-of-order 2Ghz), then *N* threads should be able to
  each execute in 500ns a transaction that performs a single uncontended
  read and write.  If only a single thread is performing transactions,
  then those transactions should complete in 250ns.  If transactions
  take much longer than this, then they won't be useful for constructing
  custom atomic operations on concurrent collection classes.

- **Low overhead for non-transactional accesses** - non-transactional
  reads should impose an average overhead of at most 25ns.

- **Mostly obstruction free** - read-only transactions and
  non-transactional reads should not be obstructed by a transaction that
  is blocked.  This means that a transaction may never obstruct a reader
  and then attempt to acquire a lock.

- **Polite blocking behavior** - waiters should never use `Thread.sleep`,
  but should put themselves to sleep on a normal Java monitor if a quick
  spin loop fails to be sufficient.

- **Contention tolerance** - performance should plateau as threads are
  added, even when the number of CPU-bound threads exceeds the number
  of CPU cores available.

- **Intelligent retry** - the retry feature should only cause reexecution
  of the transaction if one of the read values has changed, at least
  probabalistically.


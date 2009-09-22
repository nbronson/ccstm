# Consistency

CCSTM provides linearizable opaque transactions.  Briefly stated, this
means that all transactions, even those that roll back, act as if all
of their interactions with the outside world occur at a single instant.

## Strong isolation and atomicity

CCSTM provides strong isolation/strong atomicity for non-transactional
accesses to atomic data types.  This means that each access to an atomic
data type that occurs outside a transaction can be considered to occur
in its own tiny transaction.

## Linearizability

* All successful transactions have a linearization point at some instant
  between their creation and their commit
* All reads and writes [^foot] by a successful transaction appear to
  occur instantly at its linearization point

[^foot]: Only reads and writes to explicitly atomic objects are managed
         by CCSTM transactions.

Linearizability is a stronger property than serializability.  For more
details see the Wikipedia page [Linearizability][lineariz].

## Opacity

Opacity means that live transactions (transactions that have not yet
committed) are always guaranteed to have observed a consistent state.
Essentially, this means that the linearizability property is maintained
at each point in every transactional execution.  Each time a read is
performed by a live transaction, CCSTM guarantees that there is an
instant at which all reads performed so far by the transaction could
have observed the values actually seen.  All transactions, even
those that will eventually roll back, will always see a consistent
view of the world.  For more on opacity see ["On the Correctness of
Transactional Memory"][opacity] by Guerraoui and Kapalka.

## A mental model using snapshots

Every time a transactional read is performed, CCSTM guarantees that if
all of the reads in a transaction were performed instantaneously at the
same time as that read, the values would be the same as the ones that
were previously returned.  If this is not the case then the transaction
is rolled back and retried.

Reads are validated by using virtual snapshots associated with a global
clock.  When a transaction starts it chooses a *read version*, which
is associated with a virtual snapshot.  Each time an atomic object is
read, the transaction checks if any changes have been made to the object
since the virtual snapshot was taken.  If a change has occurred, then an
attempt is made to advance the virtual snapshot to a time that includes
the changes, by checking all of the reads previously performed.  If the
virtual snapshot cannot be advanced then the transaction is retried.
For more details on how the global clock guarantees consistency for
partial transactions, see the paper ["Transactional Locking II"][tl2]
by Dice, Shalev, and Shavit.

## Comparison to MVCC

MVCC provides snapshot isolation, which means that the transaction
observes a consistent state of the world, but not necessarily the most
recent state.  MVCC only guarantees that values that are written (or
explicitly marked with something like `SELECT FOR UPDATE` or Clojure's
`(ensure ref)`) are current at commit.  This allows for more concurrency
than a linearizable transaction, but it requires the user to have a
deeper understanding of the application's semantics.  By default, reads
in CCSTM are guaranteed to reflect the current state at commit time.
CCSTM provides `Ref.unrecordedRead` for relaxed semantics, where required
to improve concurrency.

Unlike multi-version concurrency control, CCSTM does not keep more than
one committed version of data.  This means that if a write is committed,
a reading transaction must advance its virtual snapshot forward or
roll back.  Since CCSTM verifies at commit that reads reflect the most
recent state of the world, there is little use in allowing transactions
to continue execution using an old version.

## Happens-before

Transactions in CCSTM have acquire semantics (like a volatile load)
for atomic objects that are read, and release semantics (like a volatile
store) for atomic atomic objects that are written.  This means that a Java
memory model happens-before relationship is only created from transaction
*A* to transaction *B* if *A* writes to a location that is read by *B*.
This means that objects can be safely published, privatized, or handed
off using transactions, but transactions are *not* barriers.


[lineariz]: http://en.wikipedia.org/wiki/Linearizability

[tl2]: http://research.sun.com/scalable/pubs/DISC2006.pdf

[opacity]: http://lpd.epfl.ch/kapalka/files/opacity-ppopp08.pdf

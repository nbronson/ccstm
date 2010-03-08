# Transaction nesting

## Read set implementation

To validate a read, CCSTM records a pair containing the handle and
the version number.  The underlying representation is as a pair of
arrays, with no attempt to merge entries representing a duplicate read.
If an array capacity is exhausted a new array with double the length is
allocated and the existing elements are copied over.

This non-merged linear representation makes the checkpointing, nested
rollback, nested commit, and nested retry operations very simple.
A checkpoint must only record the existing number of reads.  To perform
a nested rollback, the current position in the read set is restored to
the checkpoint value and handle references are nulled (to allow GC).
To commit a nested transaction, the checkpoint is just discarded.
Nested retry is the same as nested rollback, but the portion of the read
set that is being discarded is saved for later use.

## Write buffer implementation

Nested transaction support in a hash table write buffer is substantially more
complicated, because writes to the same location must all reference the same
entry, and because the write buffer is a record of locks that must be released.

Consider the following code

    STM.atomic { implicit t1 =>
      x := 10
      STM.atomic { implicit t2 =>
        x := 20
        y := 30
      }
      STM.atomic { implicit t3 =>
        x := 40
      }
    }

How many times will `x` be present in the write buffer?

* Always 1: If entries are always merged, then the previous value of
  `x` must be stored in an undo log.  This undo log may be discarded
  during nested commit, but it must be retained during nested rollback
  to arrange for release of any required locks.  If the outer transaction
  cannot survive a nested rollback, then this problem only arises during
  nested retry.

* 1 or 2: Another possibility is to store a second entry, but to merge it
  during commit.  This could lead to bad behavior during deep nesting,
  because the write buffer would have to be traversed each nested commit.

* 3: A third possibility is to install a new entry each time an access
  is performed from a new nested context.  This would have serious
  problems if a loop was performed in `t1` that performed many calls to
  `t2`, though.  A variation of this would unlink the old versions,
  keeping the lookup chains short.  The linear allocation of buckets
  would cause problems, though.

It seems that the "Always 1" solution is the best.  The only case
where this leads to bad behavior is when there are a large number of
nested rollbacks or retries, because the entries that have been removed
from the write buffer but that still might be locked must be retained.
Unacceptable growth of this data structure could be handled by making
it a hash set that would merge duplicates.

## Txn instance identity

What happens if a violation is detected?  Revalidation can perform a
partial rollback.  Does this require a separate `Txn` instance for each
nesting level?  Also, if another transaction steals a lock from a lower
priority txn can a partial rollback be performed?  Partial rollback on
violation seems doable, but partial rollback during a steal seems to be
problematic, since txn slots are a quite limited commodity.

Duplicating the entire `Txn` object seems a bit expensive, since there
are quite a few fields.  One possibility is to store all of the fields
in a referenced object that is linked from a lightweight `Txn` instance,
but the extra level of indirection seems undesirable.  Perhaps we can have
a single instance for all nesting levels?  Should that still be called
`Txn`?  The name must be short, because that is the visible implicit
parameter type.


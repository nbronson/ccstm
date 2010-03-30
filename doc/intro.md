# CCSTM - An Introduction

CCSTM is a library-based software transactional memory (STM) for Scala.
It uses optimistic concurrency control to coordinate concurrent accesses
to shared mutable state, replacing the use of locks.  CCSTM only manages
memory locations encapsulated in instances of a class `Ref[A]`[^1],
allowing atomic blocks to be expressed directly in Scala with no bytecode
rewriting or language modifications.  Atomic blocks are speculatively
executed in parallel and automatically retried until successful.

[^1]: `Ref` instances may be transient, allowing for storage without an extra
level of indirection.  See `TArray` and `TxnFieldUpdater`.

## An example

As a simple example, consider a set of integers stored as a sorted
singly-linked list.  One way to encode this in Scala is

    class IntSet {
      private class Node(val e: Int, var next: Node)
    
      private val header = new Node(-1, null)

      def contains(e: Int) = {
        @tailrec
        def loop(cur: Node): Boolean = {
          cur != null && (cur.e == e || loop(cur.next))
        }
        loop(header.next)
      }

      def add(e: Int) {
        @tailrec
        def loop(prev: Node) {
          val cur = prev.next
          if (cur == null || cur.e > e)
            prev.next = new Node(e, cur)
          else if (cur.e != e)
            loop(cur)
        }
        loop(header)
      }
    }

In this data structure the set of primes less than 10 would be
represented as

               +------+    +-----+
    primes ==> |header ==> |e: -1|   +----+
               +------+    |next ==> |e: 2|    +----+
                           +-----+   |next ==> |e: 3|    +----+
                                     +----+    |next ==> |e: 5|    +----------+
                                               +----+    |next ==> |e: 7      |
                                                         +----+    |next: null|
                                                                   +----------+

It is not safe to use an instance of this version of `IntSet` from
multiple threads, unless none of the threads calls `add(e)` for an element
`e` that is not already in the set.  If thread *T* calls `add(0)` and
thread *U* calls `add(1)` on the set of primes less than 10, for example,
both *T* and *U* could decide to insert a new node after the header.
If that happens, both methods will return but only one of the elements
would actually have been inserted.

## Optimistic concurrency control

Many method calls on `IntSet` can safely run in parallel.
`primes.contains(4)` and `primes.add(6)` will not interfere with each
other.  `primes.add(5)` and `primes.add(5)` are also okay because 5 is
already in the set.  Taking advantage of this potential parallelism can
be tricky, though, because it is not easy to tell ahead of time whether
or not a pair of methods conflict.

Optimistic concurrency control (OCC) uses *speculation* to run methods in
parallel before it can be proved that this is safe.  When the optimism
pays off (`add(5)` and `add(5)`) then extra parallelism is obtained.
If the optimism was incorrect (`add(0)` and `add(1)`) then one or both
of the speculative attempts must be *rolled back* and retried.

CCSTM makes it easy to use optimistic concurrency control to manage
shared data in your program.  It takes care of tracking the reads and
writes performed in a speculative block, and it performs the bookkeeping
required to support rollback and retry.

### Replacing variables with CCSTM references

To use CCSTM, `IntSet` must store its mutable data inside `Ref`s.
`Ref`'s companion object provides an `apply[A](initialValue: A)` method
that constructs a concrete instance with an appropriate implementation.
Because `Node`'s element `e` is a value, not a variable, it does not need
to be managed by the STM.  `Node`'s next pointer, however, must be stored
in a `Ref[Node]`.  Note that the reference itself is a `val`, not a `var`:

    class Node(val e: Int, next0: Node) {
      val next = Ref(next0)
    }

### Syntax for an atomic block

To execute a block of code atomically using CCSTM, pass it to
`STM.atomic[A](block: Txn => A)`.  This method will create a new
transaction or join an existing one, then pass the transaction to
the block.  The `Txn` must be passed to `Ref`'s methods, but this is
accomplished using Scala's `implicit` parameters.  The two most important
methods of `Ref[A]` are `apply()(implicit t: Txn): A`, which performs
a transactional read,m and `:=(v: A)(implicit t: Txn)`, which performs
a transactional write.  (These are also available as `get` and `set`.)
The `Txn` parameter can be omitted from the code if an implicit `Txn`
is available in the current scope, which leads to the idiomatic way of
writing a CCSTM transaction:

    STM.atomic { implicit t =>
      // the body
    }

We can now write the complete transactional version of `IntSet`:

    import scala.annotation.tailrec
    import edu.stanford.ppl.ccstm._

    class IntSet {
      private class Node(val e: Int, next0: Node) {
        val next = Ref(next0)
      }

      private val header = new Node(-1, null)
      
      def contains(e: Int) = STM.atomic { implicit t =>
        @tailrec
        def loop(cur: Node): Boolean = {
          cur != null && (cur.e == e || loop(cur.next()))
        }
        loop(header.next())
      }

      def add(e: Int) {
        STM.atomic { implicit t =>
          @tailrec
          def loop(prev: Node) {
            val cur = prev.next()
            if (cur == null || cur.e > e)
              prev.next := new Node(e, cur)
            else if (cur.e != e)
              loop(cur)
          }
          loop(header)
        }
      }
    }

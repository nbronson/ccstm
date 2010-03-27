# CCSTM - An Introduction

CCSTM is a library-based software transactional memory (STM) for Scala.
It uses optimistic concurrency control to coordinate concurrent accesses
to shared mutable state, replacing the use of locks.  CCSTM only manages
memory locations encapsulated in instances of a class `Ref[A]`[^1],
allowing atomic blocks to be expressed directly in Scala with no bytecode
rewriting or language modifications.  Atomic blocks are speculatively
executed in parallel and automatically retried until successful.

[^1]: `Ref` instances may be transient, allowing for efficient storage.
See `TArray` and `TxnFieldUpdater`.

## An example

As a simple example, consider a set of integers stored as a sorted
singly-linked list.  We might encode this in Scala as

    class IntSet {
      class Node(val e: Int, var next: Node)
    
      val header = new Node(-1, null)

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

In this data structure the set of primes less than 10 would be represented as

               +-----+
    header ==> |e: -1|   +----+
               |next ==> |e: 2|    +----+
               +-----+   |next ==> |e: 3|    +----+
                         +----+    |next ==> |e: 5|    +----------+
                                   +----+    |next ==> |e: 7      |
                                             +----+    |next: null|
                                                       +----------+

Without some sort of concurrency control, this implementation won't be
safe if it is accessed simultaneously by multiple threads, and any of
those threads are modifying the `IntSet`.  We refer to a pair of accesses
that are not both reads as a *conflict*.

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
      class Node(val e: Int, next0: Node) {
        val next = Ref(next0)
      }

      val header = new Node(-1, null)
      
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

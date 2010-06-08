/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// atomic

package edu.stanford.ppl.ccstm

/** The `atomic` object manages atomic execution of code blocks using software
 *  transactional memory.  The transactions provide linearizability for all of
 *  the reads and writes to data stored in `Ref` instances.  Atomic blocks are
 *  functions from `Txn` to any type.  `Ref`'s methods require that the `Txn`
 *  be available as an implicit parameter.  Scala allows the implicit modifier
 *  to be included on the argument to an anonymous method, leading to the
 *  idiomatic CCSTM atomic block {{{
 *    atomic { implicit t =>
 *      // body
 *    }
 *  }}}
 *
 *  Atomic blocks nest, so when modularizing your code you can chose to either
 *  pass around the `Txn` instance or create a new atomic block whenever an
 *  implicit `Txn` is needed in the current scope.
 *
 *  The `ccstm` package object provides an implicit conversion that implements
 *  an `orAtomic` operation for composing transactions that use `retry`.  See
 *  `atomic.oneOf`, which provides the same functionality for dynamic
 *  collections of code blocks.
 *
 *  Some examples
 *  {{{
 *  import edu.stanford.ppl.ccstm._
 *
 *  class Account(bal0: BigDecimal) {
 *    private val bal = Ref(bal0)
 *
 *    def tryWithdrawal(m: BigDecimal) = atomic { implicit t =>
 *      if (bal() >= m) {
 *        bal() = bal() - m
 *        true
 *      } else {
 *        false
 *      }
 *    }
 *
 *    def deposit(m: BigDecimal)(implicit txn: Txn) {
 *      bal() = bal() + m
 *    }
 *  }
 *
 *  object Account {
 *    def tryTransfer(from: Account, to: Account, m: BigDecimal) = atomic { implicit t =>
 *      from.tryWithdrawal(m) && { to.deposit(m) ; true }
 *    }
 *  }
 *  }}}
 *
 *  @see edu.stanford.ppl.ccstm.Ref
 *
 */
object atomic {

  /** Repeatedly attempts to perform the work of `block` in a transaction,
   *  until an attempt is successfully committed or an exception is thrown by
   *  `block` or a callback registered during the block's execution.  On
   *  successful commit this method returns the result of the block.  If the
   *  block throws an exception, the transaction will be rolled back and the
   *  exception will be rethrown from this method without further retries.
   *
   *  In the current version, if a transaction is already active on the
   *  current thread the block will be run as part of the existing transaction.
   *  In STM terminology this is support for nested transactions using
   *  "flattening" or "subsumption".  A future version of CCSTM will implement
   *  partial rollback.
   */
  def apply[Z](block: Txn => Z) = STM.atomic(block)

  /** Atomically executes a transaction that is composed from `blocks` by
   *  joining with a left-biased `orAtomic` operator.  This is equivalent to
   *  {{{
   *    atomic { implicit t =>
   *      blocks(0)
   *    } orAtomic { implicit t =>
   *      blocks(1)
   *    } ...
   *  }}}
   *
   *  The first block will be attempted in an optimistic transaction until it
   *  either succeeds, fails with no retry possible (in which case the causing
   *  exception will be rethrown), or performs an explicit `retry`.  If a retry
   *  is performed, then the next block will be attempted in the same fashion.
   *  If all blocks are explicitly retried then execution resumes at the first
   *  block, but only after some other context has changed some value read by
   *  one of the attempts.
   *
   *  The left-biasing of the `orAtomic` composition guarantees that if the
   *  first block does not call `retry`, no other blocks will be executed.
   *
   *  CCSTM's `retry` and `orAtomic` operators are modelled after the
   *  `retry` and `orElse` operators introduced by T. Harris, S. Marlow, S. P.
   *  Jones, and M. Herlihy, "Composable Memory Transactions", in PPoPP '05.
   *  (We changed the name because `orElse` is already defined on many Scala
   *  types, preventing our use of implicit conversions to make `orAtomic`
   *  behave like a keyword.)
   */
  def oneOf[Z](blocks: (Txn => Z)*) = STM.atomicOrElse(blocks: _*)

  /** Captures an atomic block, allowing it to be composed using `orAtomic`. */
  class Delayed[A](lhs: => A) {
    import impl.ThreadContext

    /** @see edu.stanford.ppl.ccstm.atomic#oneOf */
    def orAtomic[B >: A](rhs: Txn => B)(implicit mt: MaybeTxn): B = {
      var ctx: ThreadContext = null
      val txn = mt match {
        case t: Txn => t
        case _ => { ctx = ThreadContext.get ; ctx.txn }
      }

      val rightMost = if (txn == null) {
        val a = ctx.alternatives
        ctx.alternatives = rhs :: a
        a.isEmpty
      }
      else {
        val a = txn.childAlternatives
        txn.childAlternatives = rhs :: a
        a.isEmpty
      }

      // We don't have the type B available when we type lhs, so we can't
      // pass rhs's return through lhs's.  We handle this by tunnelling the
      // alternative's return value out through an exception.  If orElse-s
      // are chained, however, an OrElseResult might not be ours, and so
      // might not actually be an instance of B.  We solve this by having
      // only the right-most orElse do the catch.
      if (!rightMost) {
        lhs
      } else {
        try {
          lhs
        } catch {
          case STM.AlternativeResult(z) => z.asInstanceOf[B]
        }
      }
    }
  }
}

/* CCSTM - (c) 2009 Stanford University - PPL */

// STM.scala

package edu.stanford.ppl.ccstm


import runtime.NonLocalReturnException

/** <code>STM</code> controls execution of atomic blocks with software
 *  transactional memory transactions.  A convenient style of programming is
 *  obtained by importing <code>edu.stanford.ppl.ccstm.STM._</code>; this makes
 *  <code>atomic</code> and <code>retry</code> accessible directly.
 *  <p>
 *  <code>STM.atomic</code> works best when the work of atomic blocks is
 *  performed by named functions, due to an inability to mark the block's
 *  transaction argument as <code>implicit</code>.  If you prefer to code
 *  atomic blocks inline, you may prefer the style provided by subclassing the
 *  <code>Atomic</code> class.
 *  <p>
 *  For example:<pre>
 *     import edu.stanford.ppl.ccstm._
 *     import edu.stanford.ppl.ccstm.STM._
 *
 *     val a = Ref(10)
 *     val b = Ref(0)
 *     atomic(performTransfer(a, b, 5)(_))
 *
 *     def performTransfer(from: Ref[Int], to: Ref[Int], amount: Int)(implicit txn: Txn) {
 *       from := !from - amount
 *       to := !to + amount
 *     }
 *  </pre>
 *  or
 *  <pre>
 *     import edu.stanford.ppl.ccstm._
 *
 *     val a = Ref(10)
 *     val b = Ref(0)
 *     new Atomic { def body {
 *       val amount = 5
 *       a := !a - amount
 *       b := !b + amount
 *     }}.run
 *  </pre>
 *
 *  @see edu.stanford.ppl.ccstm.Atomic
 *
 *  @author Nathan Bronson
 */
object STM {
  /** Repeatedly attempts to perform the work of <code>block</code> in a
   *  transaction, until an attempt is successfully committed or an exception is
   *  thrown by <code>block</code> or a callback registered during the block's
   *  execution.  On successful commit this method returns the result of the
   *  block.  If the block throws an exception, the transaction will be rolled
   *  back and the exception will be rethrown from this method without further
   *  retries.
   */
  def atomic[Z](block: Txn => Z): Z = {
    var hist: List[Txn.RollbackCause] = Nil
    var zt = attemptImpl(block, hist)
    while (zt._2.status != Txn.Committed) {
      val cause = zt._2.status.rollbackCause
      hist = cause :: hist
      cause match {
        case x: Txn.ExplicitRetryCause => {
          Txn.awaitRetry(x)
        }
        case _ => {}
      }
      // retry
      zt = attemptImpl(block, hist)
    }
    zt._1
  }

  /** Makes a single attempt to perform the work of <code>block</code> in a
   *  transaction, returning <code>Some(result)</code> if the transaction was
   *  successfully committed, <code>None</code> if it was rolled back.  If the
   *  block throws an exception the transaction will be rolled back and the
   *  exception rethrown from this method.
   *  <p>
   *  Repeated calls to <code>attemptAtomic</code> are <em>not</em> the same as
   *  a call to <code>atomic</code>, because the latter avoids livelock via an
   *  implementation-specific priority escalation, and the latter can avoid
   *  futile retries of a transaction that was explicitly retried by
   *  <code>Txn.retry</code>.
   *  @see #atomic
   */
  def attemptAtomic[Z](block: Txn => Z): Option[Z] = {
    val (z,txn) = attemptImpl(block, Nil)
    if (txn.status == Txn.Committed) Some(z) else None
  }

  private def attemptImpl[Z](block: Txn => Z, failureHistory: List[Txn.RollbackCause]): (Z,Txn) = {
    val txn = new Txn(failureHistory)
    var nonLocalReturn: NonLocalReturnException[_] = null
    var result: Z = null.asInstanceOf[Z]
    try {
      result = block(txn)
    }
    catch {
      case RollbackError => {}
      case x: NonLocalReturnException[_] => {
        // This has the opposite behavior from other exception types.  We don't
        // trigger rollback, and we rethrow only if the transaction _commits_.
        nonLocalReturn = x
      }
      case x => txn.forceRollback(Txn.UserExceptionCause(x))
    }
    txn.commitAndRethrow()
    if (nonLocalReturn != null && txn.status == Txn.Committed) throw nonLocalReturn
    (result,txn)
  }

  /** Rolls the transaction back, indicating that it should be retried after
   *  one or more of the values read during the transaction have changed.
   *  @throws IllegalStateException if the transaction is not active.
   *  @see edu.stanford.ppl.ccstm.Txn#retry
   */
  def retry()(implicit txn: Txn) { txn.retry() }

  object Debug {
    def assertQuiescent() {
      impl.STMImpl.slotManager.assertAllReleased()
    }
  }
}
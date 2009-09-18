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
 *     val a = TVar(10)
 *     val b = TVar(0)
 *     atomic(performTransfer(a, b, 5)(_))
 *     //...
 *     def performTransfer(from: TVar[Int], to: TVar[Int], amount: Int)(implicit txn: Txn) {
 *       from := from - amount
 *       to := to + amount
 *     }
 *  </pre>
 *  or
 *  <pre>
 *     import edu.stanford.ppl.ccstm._
 *
 *     val a = TVar(10)
 *     val b = TVar(0)
 *     new Atomic { def body {
 *       val amount = 5
 *       a := a - amount
 *       b := b + amount
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
   *  execution.  On successful commit this method returns.  If the block
   *  throws an exception, the transaction will be rolled back and the
   *  exception will be rethrown from this method without further retries.
   */
  def atomic(block: Txn => Unit) {
    var hist: List[Txn.RollbackCause] = Nil
    var txn = attemptImpl(block, hist)
    while (txn.status != Txn.Committed) {
      val cause = txn.status.rollbackCause
      hist = cause :: hist
      cause match {
        case x: Txn.ExplicitRetryCause => {
          Txn.awaitRetry(x)
        }
        case _ => {}
      }
      // retry
      txn = attemptImpl(block, hist)
    }
  }

  /** Makes a single attempt to perform the work of <code>block</code> in a
   *  transaction, returning true if the transaction was successfully
   *  committed, false if it was rolled back.  If the block throws an
   *  exception the transaction will be rolled back and the exception rethrown
   *  from this method.
   *  <p>
   *  Repeated calls to <code>attemptAtomic</code> are <em>not</em> the same as
   *  a call to <code>atomic</code>, because the latter avoids livelock via an
   *  implementation-specific priority escalation, and the latter can avoid
   *  futile retries of a transaction that was explicitly retried by
   *  <code>Txn.retry</code>.
   *  @see #atomic
   */
  def attemptAtomic(block: Txn => Unit): Boolean = {
    attemptImpl(block, Nil).status == Txn.Committed
  }

  private def attemptImpl(block: Txn => Unit, failureHistory: List[Txn.RollbackCause]): Txn = {
    val txn = new Txn(failureHistory)
    var nonLocalReturn: NonLocalReturnException[_] = null
    try {
      block(txn)
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
    txn
  }

  /** Rolls the transaction back, indicating that it should be retried after
   *  one or more of the values read during the transaction have changed.
   *  @throws IllegalStateException if the transaction is not active.
   *  @see edu.stanford.ppl.ccstm.Txn#retry
   */
  def retry()(implicit txn: Txn) { txn.retry() }

  /** Allows access to a <code>Ref[T]</code> in a transaction without using the
   *  <code>elem</code> or <code>!</code> methods, if used in a context that
   *  can accept a <code>T</code> but cannot accept a <code>Ref</code>.
   *  <p>
   *  TODO: Reevaluate if this is a good idea.
   *  <p>
   *  Pros: less clutter when implicit conversion is applicable.  Cons: caller
   *  must consider whether or not the implicit conversion does the right
   *  thing.  This can be considered a tradeoff between syntactic and semantic
   *  complexity.  Pay special attempt to <code>constant == tvar</code>.
   */
  implicit def implicitRead[T](v: Ref[T])(implicit txn: Txn): T = v.elem
}
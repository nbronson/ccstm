/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// STM.scala

package edu.stanford.ppl.ccstm


import impl.ThreadContext
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
   *
   *  If a transaction is already active on the current thread the block will
   *  be run as part of the existing transaction.  In STM terminology this is
   *  support for nested transactions using "flattening" or "subsumption".   
   */
  def atomic[Z](block: Txn => Z)(implicit mt: MaybeTxn): Z = {
    var ctx: ThreadContext = null
    var txn = (if (mt.isInstanceOf[Txn]) {
      // statically known to be nested
      mt.asInstanceOf[Txn]
    } else {
      // statically unknown, but maybe still dynamically nested
      ctx = ThreadContext.get
      ctx.txn
    })
    if (txn != null) {
      // subsumption
      block(txn)
    }
    else {
      // new transaction
      var hist: List[Txn.RollbackCause] = Nil
      txn = new Txn(hist, ctx)
      var z = attemptImpl(txn, block)
      while (txn.status ne Txn.Committed) {
        val cause = txn.status.rollbackCause
        cause match {
          case x: Txn.ExplicitRetryCause => Txn.awaitRetryAndDestroy(x)
          case _ => {}
        }
        // retry
        hist = cause :: hist
        txn = new Txn(hist, ctx)
        z = attemptImpl(txn, block)
      }
      z
    }
  }

  /** Atomically executes a transaction that is composed from
   *  <code>blocks</code> by joining with a left-biased <em>orElse</em>
   *  operator.  The first block will be attempted in an optimistic transaction
   *  until it either succeeds, fails with no retry possible (in which case the
   *  causing exception will be rethrown), or performs an explicit
   *  <code>retry</code>.  If a retry is performed, then the next block will
   *  be attempted in the same fashion.  If all blocks are explicitly retried
   *  then execution resumes at the first block, after a change has been made
   *  to one of the values read by any transaction.
   *  <p>
   *  The left-biasing of the <em>orElse</em> composition guarantees that if
   *  the first block does not call <code>Txn.retry</code>, no other blocks
   *  will be executed.
   *  <p>
   *  The <em>retry</em> and <em>orElse</em> operators are modelled after those
   *  introduced by T. Harris, S. Marlow, S. P. Jones, and M. Herlihy,
   *  "Composable Memory Transactions", in PPoPP '05.
   *  @see edu.stanford.ppl.ccstm.Atomic#orElse
   *  @see edu.stanford.ppl.ccstm.AtomicFunc#orElse
   */
  def atomicOrElse[Z](blocks: (Txn => Z)*): Z = {
    if (null != Txn.currentOrNull) throw new UnsupportedOperationException("nested orElse is not currently supported")

    val hists = new Array[List[Txn.RollbackCause]](blocks.length)
    while (true) {
      // give all of the blocks a chance
      for (i <- 0 until blocks.length) {
        if (null == hists(i)) hists(i) = Nil
        attemptUntilRetry(blocks(i), hists(i)) match {
          case Left(z) => return z
          case Right(h) => hists(i) = h
          // exception => pass through
        }
      }

      // go to sleep
      val ers = hists.map(_.head.asInstanceOf[Txn.ExplicitRetryCause])
      Txn.awaitRetryAndDestroy(ers:_*)
    }
    throw new Error("unreachable")
  }

  private def attemptUntilRetry[Z](block: Txn => Z,
                                   hist0: List[Txn.RollbackCause]): Either[Z, List[Txn.RollbackCause]] = {
    var hist = hist0
    var txn = new Txn(hist)
    var z = attemptImpl(txn, block)
    while (txn.status ne Txn.Committed) {
      val cause = txn.status.rollbackCause
      hist = cause :: hist
      if (cause.isInstanceOf[Txn.ExplicitRetryCause]) return Right(hist)
      // retry
      txn = new Txn(hist)
      z = attemptImpl(txn, block)
    }
    Left(z)
  }

  private def attemptImpl[Z](txn: Txn, block: Txn => Z): Z = {
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
    if (null != nonLocalReturn && txn.status == Txn.Committed) throw nonLocalReturn
    result
  }

  /** Rolls the transaction back, indicating that it should be retried after
   *  one or more of the values read during the transaction have changed.
   *  @throws IllegalStateException if the transaction is not active.
   *  @see edu.stanford.ppl.ccstm.Txn#retry
   */
  def retry()(implicit txn: Txn) { txn.retry() }

  /** Generalized atomic read/modify/write of two references.  Equivalent to
   *  executing the following transaction, but probably much more efficient:
   *  <pre>
   *    new AtomicFunc[Z]{ def body = {
   *      val (a,b,z) = f(!refA, !refB)
   *      refA := a
   *      refB := b
   *      z
   *    }}.run()
   *  </pre>
   *  Because this method is only presented as an optimization, it is assumed
   *  that the evaluation of <code>f</code> will be quick.
   */
  def transform2[A,B,Z](refA: Ref[A], refB: Ref[B], f: (A,B) => (A,B,Z)): Z = {
    impl.NonTxn.transform2(refA.nonTxnHandle, refB.nonTxnHandle, f)
  }

  // TODO: better names?

  /** Establishes a happens-before relationship between transactions that
   *  previously wrote to <code>ref</code> and a subsequent call to
   *  <code>resurrect(identity, _)</code>.  Embalming a reference does not
   *  prevent its continued use.
   */
  def embalm(identity: Int, ref: Ref[_]) {
    impl.STMImpl.embalm(identity, ref.nonTxnHandle)
  }

  /** Establishes a happens-before relationship between subsequent accesses to
   *  <code>ref</code> and previous calls to <code>embalm(identity, _)</code>.
   *  A reference may only be resurrected into an old identity before any other
   *  operations (except construction) have been performed on it.
   */
  def resurrect(identity: Int, ref: Ref[_]) {
    impl.STMImpl.resurrect(identity, ref.nonTxnHandle)
  }

  object Debug {
    def assertQuiescent() {
      impl.STMImpl.slotManager.assertAllReleased()
    }
  }
}

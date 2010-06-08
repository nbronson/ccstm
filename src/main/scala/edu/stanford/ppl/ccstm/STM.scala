/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// STM.scala

package edu.stanford.ppl.ccstm


import impl.ThreadContext
import annotation.tailrec
import util.control.ControlThrowable

/** The `STM` object encapsulated miscellaneous CCSTM functionality, and
 *  provides a means to run transactions with fewer imports than the idiomatic
 *  form.  Most code will use the `apply` method of `object atomic` to execute
 *  atomic blocks. 
 *
 *  @author Nathan Bronson
 */
object STM {

  private[ccstm] case class AlternativeResult(value: Any) extends Exception

  /** @see edu.stanford.ppl.ccstm.atomic#apply */
  def atomic[Z](block: Txn => Z)(implicit mt: MaybeTxn): Z = {
    // TODO: without partial rollback we can't properly implement failure atomicity (see issue #4)

    mt match {
      case txn: Txn => nestedAtomic(block, txn) // static resolution of enclosing block
      case _ => {
        // dynamic scoping for nesting
        val ctx = ThreadContext.get
        if (null != ctx.txn) {
          nestedAtomic(block, ctx.txn)
        } else {
          topLevelAtomic(block, ctx, Nil)
        }
      }
    }
  }

  private def nestedAtomic[Z](block: Txn => Z, txn: Txn): Z = {
    if (!txn.childAlternatives.isEmpty) {
      throw new UnsupportedOperationException("nested retry/orElse not yet supported (issue #4)")
    }
    block(txn)
  }

  @tailrec
  private def topLevelAtomic[Z](block: Txn => Z, ctx: ThreadContext, hist: List[Txn.RollbackCause]): Z = {
    if (!ctx.alternatives.isEmpty) {
      // There was one or more orElse clauses.  Be careful, because their
      // return type might not be Z.
      val b = ctx.alternatives
      ctx.alternatives = Nil
      val z = atomicOrElse((block :: b): _*)
      throw AlternativeResult(z)
    }

    // new transaction
    val txn = new Txn(hist, ctx)
    val z = attemptImpl(txn, block)
    if (txn.status eq Txn.Committed) {
      z
    } else {
      val cause = txn.status.rollbackCause
      cause match {
        case x: Txn.ExplicitRetryCause => Txn.awaitRetryAndDestroy(x)
        case _ => {}
      }
      // retry
      topLevelAtomic(block, ctx, cause :: hist)
    }
  }

  /** @see edu.stanford.ppl.ccstm.atomic#oneOf */
  def atomicOrElse[Z](blocks: (Txn => Z)*)(implicit mt: MaybeTxn): Z = {
    if (null != Txn.currentOrNull) throw new UnsupportedOperationException("nested orAtomic is not currently supported")

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
    val ctx = ThreadContext.get
    var hist = hist0
    var txn = new Txn(hist, ctx)
    var z = attemptImpl(txn, block)
    while (txn.status ne Txn.Committed) {
      val cause = txn.status.rollbackCause
      hist = cause :: hist
      if (cause.isInstanceOf[Txn.ExplicitRetryCause]) return Right(hist)
      // retry
      txn = new Txn(hist, ctx)
      z = attemptImpl(txn, block)
    }
    Left(z)
  }

  private val controlThrowableClass: Class[_] = {
    try {
      Class.forName("scala.util.control.ControlThrowable")
    } catch {
      case x: ClassNotFoundException =>
          Class.forName("scala.util.control.ControlException")
    }
  }

  private def attemptImpl[Z](txn: Txn, block: Txn => Z): Z = {
    var nonLocalReturn: ControlThrowable = null
    var result: Z = null.asInstanceOf[Z]
    try {
      result = block(txn)
    }
    catch {
      case RollbackError => {}
      case x: ControlThrowable => nonLocalReturn = x
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
  def retry(implicit txn: Txn): Nothing = txn.retry()

  /** Generalized atomic read/modify/write of two references.  Equivalent to
   *  executing the following transaction, but may be more efficient:
   *  {{{
   *    atomic { implicit t =>
   *      val (a,b,z) = f(refA(), refB())
   *      refA() = a
   *      refB() = b
   *      z
   *    }
   *  }}}
   *  Because this method is only presented as an optimization, it is assumed
   *  that the evaluation of `f` will be quick.
   */
  def transform2[A,B,Z](refA: Ref[A], refB: Ref[B], f: (A,B) => (A,B,Z)): Z = {
    if (refA.isInstanceOf[impl.RefOps[_]] && refB.isInstanceOf[impl.RefOps[_]]) {
      impl.NonTxn.transform2(
          refA.asInstanceOf[impl.RefOps[A]].handle,
          refB.asInstanceOf[impl.RefOps[B]].handle,
          f)
    } else {
      atomic { implicit t =>
        val (a,b,z) = f(refA(), refB())
        refA() = a
        refB() = b
        z
      }
    }
  }

  /** Establishes a happens-before relationship between transactions that
   *  previously wrote to `ref` and a subsequent call to
   *  `resurrect(identity, _)`.  Embalming a reference does not
   *  prevent its continued use.
   */
  def embalm(identity: Int, ref: Ref[_]) {
    ref.embalm(identity)
  }

  /** Establishes a happens-before relationship between subsequent accesses to
   *  `ref` and previous calls to `embalm(identity, _)`.
   *  A reference may only be resurrected into an old identity before any other
   *  operations (except construction) have been performed on it.
   */
  def resurrect(identity: Int, ref: Ref[_]) {
    ref.resurrect(identity)
  }

  object Debug {
    /** Performs checks that are only guaranteed to succeed if no transactions
     *  are active.
     */
    def assertQuiescent() {
      impl.STMImpl.slotManager.assertAllReleased()
    }
  }
}

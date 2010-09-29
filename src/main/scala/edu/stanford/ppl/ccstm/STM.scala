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
    val ctx = getContext

    val bb = ctx.alternatives
    if (bb.isEmpty) {
      atomic(block, ctx)
    } else {
      ctx.alternatives = Nil

      // There was one or more orAtomic clauses.  ctx.alternatives has type
      // List[Txn => Any], because the return types of the other blocks might
      // not be compatible with Z.  This means that we can't return the result
      // from this method directly.  The two possibilities are:
      //    store it in ctx and return a bogus value of Z
      //    wrap the value in an exception and throw it
      // We choose the latter because the resulting code is much less likely
      // to fail silently.
      val z = atomicOrElse(block :: bb, ctx)
      throw AlternativeResult(z)
    }
  }

  private def getContext(implicit mt: MaybeTxn): ThreadContext = mt match {
    case txn: Txn => txn.ctx
    case _ => ThreadContext.get
  }

  private def atomic[Z](block: Txn => Z, ctx: ThreadContext): Z = {
    val txn = ctx.txn
    if (txn == null)
      topLevelAtomic(block, Nil, ctx)
    else
      nestedAtomic(block, txn)
  }

  @tailrec
  private def topLevelAtomic[Z](block: Txn => Z, hist: List[Txn.RollbackCause], ctx: ThreadContext): Z = {
    // new transaction
    val txn = new Txn(hist, ctx)
    var nonLocalReturn: ControlThrowable = null
    var result: Z = null.asInstanceOf[Z]
    try {
      result = block(txn)
    } catch {
      case RollbackError => {}
      case x: ControlThrowable => nonLocalReturn = x
      case x => txn.forceRollback(0, Txn.UserExceptionCause(x))
    }
    val s = txn.topLevelComplete()
    if (s eq Txn.Committed) {
      // SUCCESS, return value is either the result or a control transfer
      if (nonLocalReturn != null)
        throw nonLocalReturn
      result
    } else {
      // FAILURE, throw an exception if no retry should be performed
      val h = s.rollbackCause
      h match {
        case Txn.UserExceptionCause(x) => throw x
        case Txn.CallbackExceptionCause(_, x) => throw x
        case Txn.ExplicitRetryCause => Txn.awaitRetry(txn.takeRetrySet())
        case _ =>
      }
      topLevelAtomic(block, h :: hist, ctx)
    }
  }

  private def topLevelAttempt[Z](block: Txn => Z, hist: List[Txn.RollbackCause], ctx: ThreadContext): Either[Z, Txn.RollbackCause] = {
    // new transaction
    val txn = new Txn(hist, ctx)
    var nonLocalReturn: ControlThrowable = null
    var result: Z = null.asInstanceOf[Z]
    try {
      result = block(txn)
    } catch {
      case RollbackError => {}
      case x: ControlThrowable => nonLocalReturn = x
      case x => txn.forceRollback(0, Txn.UserExceptionCause(x))
    }
    val s = txn.topLevelComplete()
    if (s eq Txn.Committed) {
      // success, return value is either the result or a control transfer
      if (nonLocalReturn != null)
        throw nonLocalReturn
      Left(result)
    } else {
      // failure, throw an exception if no retry should be performed
      s.rollbackCause match {
        case Txn.UserExceptionCause(x) => throw x
        case Txn.CallbackExceptionCause(_, x) => throw x
        case h => Right(h)
      }
    }
  }

  @tailrec
  private def nestedAtomic[Z](block: Txn => Z, txn: Txn): Z = {
    nestedAttempt(block, txn) match {
      case Left(z) => z
      case Right(h) => {
        h match {
          case Txn.ExplicitRetryCause => txn.retry(x.readSet)
          case x: Txn.OptimisticFailureCause => {
            // We can either retry immediately or roll back the parent.  There
            // is not much benefit to performing extra partial rollbacks,
            // because if one of the parents would take a different path next
            // time it would have been detected during the validation that
            // failed here.  Only if we go back to the very beginning can we
            // turn on barging and/or increase our priority.
          }
        }
        topLevelAtomic(block, h :: hist, ctx)
      }
    }
  }
  private def nestedAttempt[Z](block: Txn => Z, txn: Txn): Either[Z, Txn.RollbackCause] = {
    txn.nestedBegin()
    var nonLocalReturn: ControlThrowable = null
    var result: Z = null.asInstanceOf[Z]
    try {
      result = block(txn)
    } catch {
      case RollbackError => {}
      case x: ControlThrowable => nonLocalReturn = x
      case x => txn.forceRollback(txn.nestingLevel, Txn.UserExceptionCause(x))
    }
    val rc = txn.nestedCommit()
    if (rc == null) {
      // success, return value is either the result or a control transfer
      if (nonLocalReturn != null)
        throw nonLocalReturn
      Left(result)
    } else {
      // failure, throw an exception if no retry should be performed
      rc match {
        case Txn.UserExceptionCause(x) => throw x
        case Txn.CallbackExceptionCause(_, x) => throw x
        case h => Right(h)
      }
    }
  }

  private def nestedAtomic[Z](block: Txn => Z, txn: Txn): Z = {
    txn.nestedBegin()
    try {
      block(txn)
    } catch {
    }

    if (!ctx.alternatives.isEmpty) {
      // There was one or more orElse clauses.  Be careful, because their
      // return type might not be Z.
      val b = ctx.alternatives
      ctx.alternatives = Nil
      val z = atomicOrElse((block :: b): _*)
      throw AlternativeResult(z)

    }
    val b = txn.childAlternatives
    txn.childAlternatives = Nil

    txn.nestedBegin()
    try {

    }
    if (!txn.childAlternatives.isEmpty) {
      throw new UnsupportedOperationException("nested retry/orElse not yet supported (issue #4)")
    }
    block(txn)
  }


  /** @see edu.stanford.ppl.ccstm.atomic#oneOf */
  def atomicOrElse[Z](blocks: (Txn => Z)*)(implicit mt: MaybeTxn): Z = atomicOrElse(getContext, blocks)

  private def atomicOrElse[Z](ctx: ThreadContext, blocks: Seq[Txn => Z]): Z = {
    val txn = ctx.txn
    if (txn == null)
      topLevelAtomicOrElse(ctx, blocks :: bb)
    else
      nestedAtomicOrElse(txn, blocks)
  }

  private def topLevelAtomicOrElse[Z](ctx: ThreadContext, blocks: Seq[Txn => Z]): Z = {
    val hists = new Array[List[Txn.RollbackCause]](blocks.length)
    (while (true) {
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
      Txn.awaitRetryAndDestroy(ers: _*)
    }).asInstanceOf[Nothing]
  }

  @tailrec
  private def attemptUntilRetry[Z](block: Txn => Z,
                                   hist: List[Txn.RollbackCause],
                                   ctx: ThreadContext): Either[Z, List[Txn.RollbackCause]] = {
    topLevelAttempt(block, hist, ctx) match {
      case z @ Left(_) => z
      case z @ Right(Txn.ExplicitRetryCause(_) :: _) => z
      case Right(hh) => attemptUntilRetry(block, hh, ctx)
    }
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

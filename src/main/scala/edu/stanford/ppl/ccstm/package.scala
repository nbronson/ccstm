/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// atomic

package edu.stanford.ppl

package object ccstm {

  import impl.ThreadContext

  object atomic {

    def apply[Z](block: Txn => Z) = STM.atomic(block)

    def oneOf[Z](blocks: (Txn => Z)*) = STM.atomicOrElse(blocks: _*)

    class Delayed[A](lhs: => A) {
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

  def retry(implicit txn: edu.stanford.ppl.ccstm.Txn): Nothing = edu.stanford.ppl.ccstm.STM.retry

  implicit def delayAtomic[A](lhs: => A) = new edu.stanford.ppl.ccstm.atomic.Delayed(lhs)
}

/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// atomic

package edu.stanford.ppl.ccstm


object atomic {
  
  def apply[Z](block: Txn => Z) = STM.atomic(block)

  def oneOf[Z](blocks: (Txn => Z)*) = STM.atomicOrElse(blocks: _*)

//  class DelayedAtomic[A](lhs: => A) {
//    def orElse[B >: A](rhs: Txn => B): B = {
//      STM.pushAlternative(rhs)
//      try {
//        lhs
//      } catch {
//        // We don't have the type B available when we type lhs, so we can't
//        // pass rhs's return through lhs's.  We handle this by tunnelling the
//        // alternative's return value out through an exception.  If orElse-s
//        // are chained, however, an OrElseResult might not be ours, and so
//        // might not actually be an instance of B.
//        case STM.AlternativeResult(r, x) if (r eq rhs) => x.asInstanceOf[B]
//      }
//    }
//  }
//
//  implicit def delayAtomic[A](lhs: => A) = new DelayedAtomic(lhs)
}
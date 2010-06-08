/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// package

package edu.stanford.ppl

package object ccstm {

  /** Equivalent to `implicitly[Txn].retry()`. */
  def retry(implicit txn: edu.stanford.ppl.ccstm.Txn): Nothing = txn.retry()

  /** This is the first half of the machinery for implementing `orAtomic`. */
  implicit def delayAtomic[A](lhs: => A) = new edu.stanford.ppl.ccstm.atomic.Delayed(lhs)
}

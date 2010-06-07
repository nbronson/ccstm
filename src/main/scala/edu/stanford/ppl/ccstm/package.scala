/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// package

package edu.stanford.ppl

package object ccstm {

  def retry(implicit txn: edu.stanford.ppl.ccstm.Txn): Nothing = edu.stanford.ppl.ccstm.STM.retry

  implicit def delayAtomic[A](lhs: => A) = new edu.stanford.ppl.ccstm.atomic.Delayed(lhs)
}

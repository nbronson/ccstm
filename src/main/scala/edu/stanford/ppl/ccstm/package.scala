/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// atomic

package edu.stanford.ppl


package object ccstm {

  def retry(implicit txn: edu.stanford.ppl.ccstm.Txn): Nothing = edu.stanford.ppl.ccstm.STM.retry

  implicit def delayAtomic[@specialized(Int) A](lhs: => A) = new edu.stanford.ppl.ccstm.atomic.Delayed(lhs)
}

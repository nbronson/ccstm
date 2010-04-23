/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// atomic

package edu.stanford.ppl


package object ccstm {
  
  def retry(implicit txn: Txn) = STM.retry
}
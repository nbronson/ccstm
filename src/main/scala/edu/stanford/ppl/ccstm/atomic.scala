/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// atomic

package edu.stanford.ppl.ccstm


object atomic {
  
  def apply[Z](block: Txn => Z) = STM.atomic(block)

  def oneOf[Z](blocks: (Txn => Z)*) = STM.atomicOrElse(blocks: _*)
}
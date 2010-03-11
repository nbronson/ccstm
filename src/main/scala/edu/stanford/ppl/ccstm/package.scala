/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// ccstm

package edu.stanford.ppl

import ccstm.MaybeTxn

package object ccstm {
  /** An object that represents the absence of a statically-known `Txn`
   *  context.  Available implicitly.
   */
  implicit object TxnUnknown extends MaybeTxn
}

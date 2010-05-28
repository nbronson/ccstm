/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// MaybeTxn

package edu.stanford.ppl.ccstm


object MaybeTxn {
  implicit val unknown = TxnUnknown  
}

/** Holds either a `Txn`, or a `TxnUnknown`.  This is called `MaybeTxn` instead
 *  of OptTxn because the absence of a statically known transaction does not
 *  imply that there is not a dynamically-bound one.  When `TxnUnknown` is
 *  found the caller should call `Txn.current` to check the dynamic context.
 *
 *  @author Nathan Bronson
 */
trait MaybeTxn

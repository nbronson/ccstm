/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TxnUnknown

package edu.stanford.ppl.ccstm


/** An object that represents the absence of a statically known transaction
 *  context.  This is available implicitly via `MaybeTxn.unknown`.  When used
 *  to satisfy a `MaybeTxn` implicit, this object causes a dynamic transaction
 *  lookup to occur (as for `Txn.current`).
 *
 *  @author Nathan Bronson
 */
object TxnUnknown extends MaybeTxn
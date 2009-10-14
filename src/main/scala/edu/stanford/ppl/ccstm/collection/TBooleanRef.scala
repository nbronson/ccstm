/* CCSTM - (c) 2009 Stanford University - PPL */

// TBooleanRef

package edu.stanford.ppl.ccstm.collection

import impl.STMImpl._


/** A concrete implementation of <code>Ref[Boolean]</code>.  More efficient
 *  than a <code>TAnyRef[Boolean]</code>.
 *  <p>
 *  This class is not sealed, so it may be opportunistically subclassed to
 *  reduce a level of indirection and the associated storage overheads.
 *
 *  @author Nathan Bronson
 */

class TBooleanRef(initialValue: Boolean) extends impl.MetaHolder(withUserBit(0L, initialValue)) with Ref[Boolean] with impl.Handle[Boolean] {

  protected def handle: impl.Handle[Boolean] = this

  private[ccstm] def ref: AnyRef = this
  private[ccstm] def offset: Int = 0

  private[ccstm] def data: Boolean = userBit(meta)
  private[ccstm] def data_=(v: Boolean) {
    var done = false
    while (!done) {
      val m = meta
      done = userBit(m) == v || metaCAS(m, withUserBit(m, v))
    }
  }

  // TODO: evaluate whether the advantage of avoiding the spin loop in get is worth the polymorphism

  //override def get(implicit txn: Txn): Boolean = txn.getBooleanInUserData(handle)

  //override def bind(implicit txn: Txn): Ref.Bound[Boolean] = new Ref.TxnBound[Boolean](this, txn) {
  //  override def get: Boolean = txn.getBooleanInUserData(handle)
  //}

  override def nonTxn: Ref.Bound[Boolean] = new Ref.NonTxnBound[Boolean](this, handle) {
    //override def get: Boolean = impl.NonTxn.getUserBit(handle)
    override def set(v: Boolean) { impl.NonTxn.setUserBit(handle, v) }
  }
}
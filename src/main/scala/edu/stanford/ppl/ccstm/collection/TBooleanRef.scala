/* CCSTM - (c) 2009 Stanford University - PPL */

// TBooleanRef

package edu.stanford.ppl.ccstm.collection

import impl.STMImpl._


/** A concrete implementation of <code>Ref[Boolean]</code>.
 *  <p>
 *  This class is not sealed, so it may be opportunistically subclassed to
 *  reduce a level of indirection and the associated storage overheads.
 *
 *  @author Nathan Bronson
 */

class TBooleanRef(initialValue: Boolean) extends impl.MetaHolder(withUserBit(0L, initialValue)) with Ref[Boolean] with impl.Handle[Boolean] {

  protected def handle = this

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

  override def toString = {
    "TBooleanRef@" + Integer.toHexString(hashCode)
  }
}
/* CCSTM - (c) 2009 Stanford University - PPL */

// TLongRef

package edu.stanford.ppl.ccstm.collection


/** A concrete implementation of <code>Ref[Long]</code>.
 *  <p>
 *  This class is not sealed, so it may be opportunistically subclassed to
 *  reduce a level of indirection and the associated storage overheads.
 *
 *  @author Nathan Bronson
 */

class TLongRef(initialValue: Long) extends impl.MetaHolder with Ref[Long] with impl.Handle[Long] {

  protected def handle = this

  private[ccstm] def ref: AnyRef = this
  private[ccstm] def offset: Int = 0
  @volatile private[ccstm] var data = initialValue

  override def toString = {
    "TLongRef@" + Integer.toHexString(hashCode)
  }
}
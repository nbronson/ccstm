/* CCSTM - (c) 2009 Stanford University - PPL */

// TDoubleRef

package edu.stanford.ppl.ccstm.collection


/** A concrete implementation of <code>Ref[Double]</code>.
 *  <p>
 *  This class is not sealed, so it may be opportunistically subclassed to
 *  reduce a level of indirection and the associated storage overheads.
 *
 *  @author Nathan Bronson
 */

class TDoubleRef(initialValue: Double) extends impl.MetaHolder with Ref[Double] with impl.Handle[Double] {

  protected def handle = this

  private[ccstm] def ref: AnyRef = this
  private[ccstm] def offset: Int = 0
  @volatile private[ccstm] var data = initialValue

  override def toString = {
    "TDoubleRef@" + Integer.toHexString(hashCode)
  }
}
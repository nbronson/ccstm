/* CCSTM - (c) 2009 Stanford University - PPL */

// TFloatRef

package edu.stanford.ppl.ccstm.collection


/** A concrete implementation of <code>Ref[Float]</code>.
 *  <p>
 *  This class is not sealed, so it may be opportunistically subclassed to
 *  reduce a level of indirection and the associated storage overheads.
 *
 *  @author Nathan Bronson
 */

class TFloatRef(initialValue: Float) extends impl.MetaHolder with Ref[Float] with impl.Handle[Float] {

  protected def handle = this

  private[ccstm] def ref: AnyRef = this
  private[ccstm] def offset: Int = 0
  @volatile private[ccstm] var data = initialValue
}
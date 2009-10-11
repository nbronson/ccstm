/* CCSTM - (c) 2009 Stanford University - PPL */

// TAnyRef

package edu.stanford.ppl.ccstm.collection


/** A concrete implementation of <code>Ref</code>.
 *  <p>
 *  This class is not sealed, so it may be opportunistically subclassed to
 *  reduce a level of indirection and the associated storage overheads.
 *
 *  @author Nathan Bronson
 */
class TAnyRef[T](initialValue: T) extends impl.MetaHolder with Ref[T] with impl.Handle[T] {

  protected def handle = this

  private[ccstm] def ref: AnyRef = this
  private[ccstm] def offset: Int = 0
  @volatile private[ccstm] var data = initialValue
}

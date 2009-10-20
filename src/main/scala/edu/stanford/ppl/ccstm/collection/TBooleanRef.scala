/* CCSTM - (c) 2009 Stanford University - PPL */

// TBooleanRef

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._


/** A concrete implementation of <code>Ref[Boolean]</code>.  More efficient
 *  than a <code>TAnyRef[Boolean]</code>.
 *  <p>
 *  This class is not sealed, so it may be opportunistically subclassed to
 *  reduce a level of indirection and the associated storage overheads.
 *
 *  @author Nathan Bronson
 */
class TBooleanRef(initialValue: Boolean) extends impl.MetaHolder with Ref[Boolean] with impl.Handle[Boolean] {

  protected def handle: impl.Handle[Boolean] = this

  private[ccstm] def ref: AnyRef = this
  private[ccstm] def offset: Int = 0
  @volatile private[ccstm] var data = initialValue
}
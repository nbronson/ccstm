/* CCSTM - (c) 2009 Stanford University - PPL */

// TIntRef

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._


/** A concrete implementation of <code>Ref[Int]</code>.
 *  <p>
 *  This class is not sealed, so it may be opportunistically subclassed to
 *  reduce a level of indirection and the associated storage overheads.
 *
 *  @author Nathan Bronson
 */

class TIntRef(initialValue: Int) extends impl.MetaHolder with Ref[Int] with impl.Handle[Int] {

  protected def handle: impl.Handle[Int] = this

  private[ccstm] def ref: AnyRef = this
  private[ccstm] def offset: Int = 0
  @volatile private[ccstm] var data = initialValue
}
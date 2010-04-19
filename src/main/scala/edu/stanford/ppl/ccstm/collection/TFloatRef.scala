/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TFloatRef

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._
import java.util.concurrent.atomic.AtomicLongFieldUpdater


private object TFloatRef {
  val metaUpdater = (new TFloatRef(0 : Float)).newMetaUpdater
}

/** A concrete implementation of <code>Ref[Float]</code>.
 *  <p>
 *  This class is not sealed, so it may be opportunistically subclassed to
 *  reduce a level of indirection and the associated storage overheads.
 *
 *  @author Nathan Bronson
 */
class TFloatRef(initialValue: Float) extends impl.Handle[Float] with Ref[Float] {

  private[ccstm] def handle: impl.Handle[Float] = this

  @volatile private[ccstm] var meta: Long = 0L
  private[ccstm] def metaCAS(before: Long, after: Long) = {
    TFloatRef.metaUpdater.compareAndSet(this, before, after)
  }
  private[TFloatRef] def newMetaUpdater = {
    AtomicLongFieldUpdater.newUpdater(classOf[TFloatRef], "meta")
  }

  private[ccstm] def ref: AnyRef = this
  private[ccstm] def offset: Int = 0
  private[ccstm] def metaOffset: Int = 0
  @volatile private[ccstm] var data = initialValue
}

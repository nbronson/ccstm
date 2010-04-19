/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TLongRef

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._
import java.util.concurrent.atomic.AtomicLongFieldUpdater


private object TLongRef {
  val metaUpdater = (new TLongRef(0 : Long)).newMetaUpdater
}

/** A concrete implementation of <code>Ref[Long]</code>.
 *  <p>
 *  This class is not sealed, so it may be opportunistically subclassed to
 *  reduce a level of indirection and the associated storage overheads.
 *
 *  @author Nathan Bronson
 */
class TLongRef(initialValue: Long) extends impl.Handle[Long] with Ref[Long] {

  private[ccstm] def handle: impl.Handle[Long] = this

  @volatile private[ccstm] var meta: Long = 0L
  private[ccstm] def metaCAS(before: Long, after: Long) = {
    TLongRef.metaUpdater.compareAndSet(this, before, after)
  }
  private[TLongRef] def newMetaUpdater = {
    AtomicLongFieldUpdater.newUpdater(classOf[TLongRef], "meta")
  }

  private[ccstm] def ref: AnyRef = this
  private[ccstm] def offset: Int = 0
  private[ccstm] def metaOffset: Int = 0
  @volatile private[ccstm] var data = initialValue
}

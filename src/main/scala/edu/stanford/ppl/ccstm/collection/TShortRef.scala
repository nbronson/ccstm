/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TShortRef

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._
import java.util.concurrent.atomic.AtomicLongFieldUpdater


private object TShortRef {
  val metaUpdater = (new TShortRef(0 : Short)).newMetaUpdater
}

/** A concrete implementation of <code>Ref[Short]</code>.
 *  <p>
 *  This class is not sealed, so it may be opportunistically subclassed to
 *  reduce a level of indirection and the associated storage overheads.
 *
 *  @author Nathan Bronson
 */
private[ccstm] class TShortRef(initialValue: Short) extends impl.Handle[Short] with Ref[Short] {

  private[ccstm] def handle: impl.Handle[Short] = this

  @volatile private[ccstm] var meta: Long = 0L
  private[ccstm] def metaCAS(before: Long, after: Long) = {
    TShortRef.metaUpdater.compareAndSet(this, before, after)
  }
  private[TShortRef] def newMetaUpdater = {
    AtomicLongFieldUpdater.newUpdater(classOf[TShortRef], "meta")
  }

  private[ccstm] def ref: AnyRef = this
  private[ccstm] def offset: Int = 0
  private[ccstm] def metaOffset: Int = 0
  @volatile private[ccstm] var data = initialValue
}

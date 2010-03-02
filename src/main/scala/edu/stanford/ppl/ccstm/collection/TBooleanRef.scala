/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TBooleanRef

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._
import java.util.concurrent.atomic.AtomicLongFieldUpdater


private object TBooleanRef {
  val metaUpdater = (new TBooleanRef(false)).newMetaUpdater
}

/** A concrete implementation of <code>Ref[Int]</code>.
 *  <p>
 *  This class is not sealed, so it may be opportunistically subclassed to
 *  reduce a level of indirection and the associated storage overheads.
 *
 *  @author Nathan Bronson
 */
class TBooleanRef(initialValue: Boolean) extends Ref[Boolean] with impl.Handle[Boolean] {

  protected def handle: impl.Handle[Boolean] = this

  @volatile private[ccstm] var meta: Long = 0L
  private[ccstm] def metaCAS(before: Long, after: Long) = {
    TBooleanRef.metaUpdater.compareAndSet(this, before, after)
  }
  private[TBooleanRef] def newMetaUpdater = {
    AtomicLongFieldUpdater.newUpdater(classOf[TBooleanRef], "meta")
  }

  private[ccstm] def ref: AnyRef = this
  private[ccstm] def offset: Int = 0
  private[ccstm] def metaOffset: Int = 0
  @volatile private[ccstm] var data = initialValue
}

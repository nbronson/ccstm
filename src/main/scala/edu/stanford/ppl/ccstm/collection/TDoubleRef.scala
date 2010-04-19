/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TDoubleRef

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._
import java.util.concurrent.atomic.AtomicLongFieldUpdater


private object TDoubleRef {
  val metaUpdater = (new TDoubleRef(0 : Double)).newMetaUpdater
}

/** A concrete implementation of <code>Ref[Double]</code>.
 *  <p>
 *  This class is not sealed, so it may be opportunistically subclassed to
 *  reduce a level of indirection and the associated storage overheads.
 *
 *  @author Nathan Bronson
 */
class TDoubleRef(initialValue: Double) extends impl.Handle[Double] with Ref[Double] {

  private[ccstm] def handle: impl.Handle[Double] = this

  @volatile private[ccstm] var meta: Long = 0L
  private[ccstm] def metaCAS(before: Long, after: Long) = {
    TDoubleRef.metaUpdater.compareAndSet(this, before, after)
  }
  private[TDoubleRef] def newMetaUpdater = {
    AtomicLongFieldUpdater.newUpdater(classOf[TDoubleRef], "meta")
  }

  private[ccstm] def ref: AnyRef = this
  private[ccstm] def offset: Int = 0
  private[ccstm] def metaOffset: Int = 0
  @volatile private[ccstm] var data = initialValue
}

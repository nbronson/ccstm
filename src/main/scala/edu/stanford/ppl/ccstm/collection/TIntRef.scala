/* CCSTM - (c) 2009 Stanford University - PPL */

// TIntRef

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._
import java.util.concurrent.atomic.AtomicLongFieldUpdater


private object TIntRef {
  val metaUpdater = (new TIntRef(0)).newMetaUpdater
}

/** A concrete implementation of <code>Ref[Int]</code>.
 *  <p>
 *  This class is not sealed, so it may be opportunistically subclassed to
 *  reduce a level of indirection and the associated storage overheads.
 *
 *  @author Nathan Bronson
 */
class TIntRef(initialValue: Int) extends Ref[Int] with impl.Handle[Int] {

  protected def handle: impl.Handle[Int] = this

  @volatile private[ccstm] var meta: Long = 0L
  private[ccstm] def metaCAS(before: Long, after: Long) = {
    TIntRef.metaUpdater.compareAndSet(this, before, after)
  }
  private[TIntRef] def newMetaUpdater = {
    AtomicLongFieldUpdater.newUpdater(classOf[TIntRef], "meta")
  }

  private[ccstm] def ref: AnyRef = this
  private[ccstm] def offset: Int = 0
  private[ccstm] def metaOffset: Int = 0
  @volatile private[ccstm] var data = initialValue
}
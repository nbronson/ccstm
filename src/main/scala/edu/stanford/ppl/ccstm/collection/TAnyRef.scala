/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TAnyRef

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._
import java.util.concurrent.atomic.AtomicLongFieldUpdater


private object TAnyRef {
  val metaUpdater = (new TAnyRef(null)).newMetaUpdater
}

/** A concrete implementation of <code>Ref[Int]</code>.
 *  <p>
 *  This class is not sealed, so it may be opportunistically subclassed to
 *  reduce a level of indirection and the associated storage overheads.
 *
 *  @author Nathan Bronson
 */
class TAnyRef[T](initialValue: T) extends impl.Handle[T] with Ref[T] {

  protected def handle: impl.Handle[T] = this

  @volatile private[ccstm] var meta: Long = 0L
  private[ccstm] def metaCAS(before: Long, after: Long) = {
    TAnyRef.metaUpdater.compareAndSet(this, before, after)
  }
  private[TAnyRef] def newMetaUpdater = {
    AtomicLongFieldUpdater.newUpdater(classOf[TAnyRef[_]], "meta")
  }

  private[ccstm] def ref: AnyRef = this
  private[ccstm] def offset: Int = 0
  private[ccstm] def metaOffset: Int = 0
  @volatile private[ccstm] var data = initialValue
}

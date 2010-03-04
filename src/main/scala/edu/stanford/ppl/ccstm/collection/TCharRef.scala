/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TCharRef

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._
import java.util.concurrent.atomic.AtomicLongFieldUpdater


private object TCharRef {
  val metaUpdater = (new TCharRef(0 : Char)).newMetaUpdater
}

/** A concrete implementation of <code>Ref[Char]</code>.
 *  <p>
 *  This class is not sealed, so it may be opportunistically subclassed to
 *  reduce a level of indirection and the associated storage overheads.
 *
 *  @author Nathan Bronson
 */
class TCharRef(initialValue: Char) extends Ref[Char] with impl.Handle[Char] {

  protected def handle: impl.Handle[Char] = this

  @volatile private[ccstm] var meta: Long = 0L
  private[ccstm] def metaCAS(before: Long, after: Long) = {
    TCharRef.metaUpdater.compareAndSet(this, before, after)
  }
  private[TCharRef] def newMetaUpdater = {
    AtomicLongFieldUpdater.newUpdater(classOf[TCharRef], "meta")
  }

  private[ccstm] def ref: AnyRef = this
  private[ccstm] def offset: Int = 0
  private[ccstm] def metaOffset: Int = 0
  @volatile private[ccstm] var data = initialValue
}

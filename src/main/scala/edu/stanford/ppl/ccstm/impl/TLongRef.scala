/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TLongRef

package edu.stanford.ppl.ccstm
package impl

import java.util.concurrent.atomic.AtomicLongFieldUpdater


private object TLongRef {
  val metaUpdater = (new TLongRef(0 : Long)).newMetaUpdater
}

/** A concrete implementation of `Ref[Long]`.
 *
 *  @author Nathan Bronson
 */
private[ccstm] class TLongRef(initialValue: Long) extends Handle[Long] with RefOps[Long] {

  private[ccstm] def handle: Handle[Long] = this

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

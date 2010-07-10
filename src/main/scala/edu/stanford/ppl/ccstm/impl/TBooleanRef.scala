/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TBooleanRef

package edu.stanford.ppl.ccstm
package impl

import java.util.concurrent.atomic.AtomicLongFieldUpdater


private object TBooleanRef {
  val metaUpdater = (new TBooleanRef(false)).newMetaUpdater
}

/** A concrete implementation of `Ref[Boolean]`.
 *
 *  @author Nathan Bronson
 */
private[ccstm] class TBooleanRef(initialValue: Boolean) extends Handle[Boolean] with RefOps[Boolean] {

  private[ccstm] def handle: Handle[Boolean] = this

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

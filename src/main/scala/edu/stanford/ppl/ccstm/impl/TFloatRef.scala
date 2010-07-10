/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TFloatRef

package edu.stanford.ppl.ccstm
package impl

import java.util.concurrent.atomic.AtomicLongFieldUpdater


private object TFloatRef {
  val metaUpdater = (new TFloatRef(0 : Float)).newMetaUpdater
}

/** A concrete implementation of `Ref[Float]`.
 *
 *  @author Nathan Bronson
 */
private[ccstm] class TFloatRef(initialValue: Float) extends Handle[Float] with RefOps[Float] {

  private[ccstm] def handle: Handle[Float] = this

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

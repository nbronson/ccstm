/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TByteRef

package edu.stanford.ppl.ccstm
package impl

import java.util.concurrent.atomic.AtomicLongFieldUpdater


private object TByteRef {
  val metaUpdater = (new TByteRef(0 : Byte)).newMetaUpdater
}

/** A concrete implementation of `Ref[Byte]`.
 *
 *  @author Nathan Bronson
 */
private[ccstm] class TByteRef(initialValue: Byte) extends Handle[Byte] with RefOps[Byte] { 

  private[ccstm] def handle: Handle[Byte] = this

  @volatile private[ccstm] var meta: Long = 0L
  private[ccstm] def metaCAS(before: Long, after: Long) = {
    TByteRef.metaUpdater.compareAndSet(this, before, after)
  }
  private[TByteRef] def newMetaUpdater = {
    AtomicLongFieldUpdater.newUpdater(classOf[TByteRef], "meta")
  }

  private[ccstm] def ref: AnyRef = this
  private[ccstm] def offset: Int = 0
  private[ccstm] def metaOffset: Int = 0
  @volatile private[ccstm] var data = initialValue
}

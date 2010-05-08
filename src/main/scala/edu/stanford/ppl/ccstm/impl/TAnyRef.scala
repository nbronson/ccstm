/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TAnyRef

package edu.stanford.ppl.ccstm
package impl

import java.util.concurrent.atomic.AtomicLongFieldUpdater


private object TAnyRef {
  val metaUpdater = (new TAnyRef(null)).newMetaUpdater
}

/** A concrete implementation of `Ref[T]` for any type `T`.
 *
 *  @author Nathan Bronson
 */
private[ccstm] class TAnyRef[@specialized(Int) T](initialValue: T) extends Handle[T] with RefOps[T] {

  private[ccstm] def handle: Handle[T] = this

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

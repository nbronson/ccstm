/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TDoubleRef

package edu.stanford.ppl.ccstm
package impl

import java.util.concurrent.atomic.AtomicLongFieldUpdater


private object TDoubleRef {
  val metaUpdater = (new TDoubleRef(0 : Double)).newMetaUpdater
}

/** A concrete implementation of `Ref[Double]`.
 *
 *  @author Nathan Bronson
 */
private[ccstm] class TDoubleRef(initialValue: Double) extends Handle[Double] with RefOps[Double] {

  private[ccstm] def handle: Handle[Double] = this

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

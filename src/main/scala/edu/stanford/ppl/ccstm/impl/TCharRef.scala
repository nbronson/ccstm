/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TCharRef

package edu.stanford.ppl.ccstm
package impl

import java.util.concurrent.atomic.AtomicLongFieldUpdater


private object TCharRef {
  val metaUpdater = (new TCharRef(0 : Char)).newMetaUpdater
}

/** A concrete implementation of `Ref[Char]`.
 *
 *  @author Nathan Bronson
 */
private[ccstm] class TCharRef(initialValue: Char) extends Handle[Char] with RefOps[Char] {

  private[ccstm] def handle: Handle[Char] = this

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

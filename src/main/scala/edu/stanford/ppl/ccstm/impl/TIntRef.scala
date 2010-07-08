/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TIntRef

package edu.stanford.ppl.ccstm
package impl

import java.util.concurrent.atomic.AtomicLongFieldUpdater
import math.Numeric


private object TIntRef {
  val metaUpdater = (new TIntRef(0)).newMetaUpdater
}

/** A concrete implementation of `Ref[Int]`.
 *
 *  @author Nathan Bronson
 */
private[ccstm] class TIntRef(initialValue: Int) extends Handle[Int] with RefOps[Int] {

  private[ccstm] def handle: Handle[Int] = this

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

  override def single: Ref.View[Int] = new SingleView[Int](this, handle) {
    override def += (rhs: Int)(implicit num: Numeric[Int]) {
      if (rhs != 0) {
        Txn.dynCurrentOrNull match {
          case null => NonTxn.getAndAdd(handle, rhs)
          case txn => txn.getAndTransform(handle, { (v: Int) => v + rhs })
        }
      }
    }

    override def -= (rhs: Int)(implicit num: Numeric[Int]) { this += (-rhs) }
  }

  override def escaped: Ref.View[Int] = new EscapedView[Int](this, handle) {
    override def += (rhs: Int)(implicit num: Numeric[Int]) {
      if (rhs != 0) NonTxn.getAndAdd(handle, rhs)
    }

    override def -= (rhs: Int)(implicit num: Numeric[Int]) { this += (-rhs) }
  }

  override def +=(rhs: Int)(implicit txn: Txn, num: Numeric[Int]) {
    txn.getAndTransform(handle, { (v: Int) => v + rhs })
  }

  override def -=(rhs: Int)(implicit txn: Txn, num: Numeric[Int]) { this += (-rhs) }
}

/* CCSTM - (c) 2009 Stanford University - PPL */

// TAnyRef

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._
import java.util.concurrent.atomic.AtomicLongFieldUpdater


private object TPairRef {
  val metaUpdater = (new TPairRef(("",""))).newMetaUpdater
}

/** A <code>Ref</code> implementation that holds only non-null
 *  <code>Option</code> instances.  When compared to
 *  <code>TAnyRef[Option[T]]</code> instances, instances of
 *  <code>TOptionRef</code> have lower storage overhead (the wrapping
 *  <code>Option</code> objects are discarded and recreated as needed), but a
 *  slightly higher runtime cost when accessing.
 *
 *  @author Nathan Bronson
 */
class TPairRef[A,B](initialValue: (A,B)) extends Ref[(A,B)] with impl.Handle[(A,B)] {
  import TPairRef._

  protected def handle: impl.Handle[(A,B)] = this

  @volatile private[ccstm] var meta: Long = 0L
  private[ccstm] def metaCAS(before: Long, after: Long) = {
    metaUpdater.compareAndSet(this, before, after)
  }
  private[TPairRef] def newMetaUpdater = {
    AtomicLongFieldUpdater.newUpdater(classOf[TPairRef[_,_]], "meta")
  }

  private[ccstm] def ref: AnyRef = this
  private[ccstm] def offset: Int = 0
  private[ccstm] def metaOffset: Int = 0

  @volatile private var _first = initialValue._1
  @volatile private var _second = initialValue._2
  private[ccstm] def data = (_first, _second)
  private[ccstm] def data_=(v: (A,B)) { _first = v._1 ; _second = v._2 }
}
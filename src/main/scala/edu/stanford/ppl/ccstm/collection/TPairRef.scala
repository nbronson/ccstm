/* CCSTM - (c) 2009 Stanford University - PPL */

// TAnyRef

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._
import java.util.concurrent.atomic.AtomicLongFieldUpdater


private object TPairRef {
  val metaUpdater = (new TPairRef(("",""))).newMetaUpdater
}

/** A <code>Ref</code> implementation that holds only non-null
 *  <code>Tuple2</code> instances.  When compared to
 *  <code>TAnyRef[Tuple2[A,B]]</code> instances, instances of
 *  <code>TPairRef</code> have lower storage overhead (the wrapping
 *  <code>Tuple2</code> objects are discarded and recreated as needed), but a
 *  slightly higher runtime cost when accessing.
 *
 *  @author Nathan Bronson
 */
class TPairRef[A,B](initialA: A, initialB: B) extends Ref[(A,B)] with impl.Handle[(A,B)] {

  def this(initialPair: (A,B)) = this(initialPair._1, initialPair._2)

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

  @volatile private var _first = initialA
  @volatile private var _second = initialB
  private[ccstm] def data = (_first, _second)
  private[ccstm] def data_=(v: (A,B)) { _first = v._1 ; _second = v._2 }
}
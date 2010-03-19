/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TAnyRef

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._
import java.util.concurrent.atomic.AtomicLongFieldUpdater


private object TIdentityPairRef {
  val metaUpdater = (new TIdentityPairRef(IdentityPair("",""))).newMetaUpdater
}

/** A <code>Ref</code> implementation that holds either null or a
 *  <code>IdentityPair</code> instance with a non-null element.  When compared
 *  to <code>TAnyRef[IdentityPair[A,B]]</code> instances, instances of
 *  <code>TIdentityPairRef</code> have lower storage overhead (the wrapping
 *  <code>IdentityPair</code> objects are discarded and recreated as needed),
 *  but a slightly higher runtime cost when accessing.
 *  <p>
 *  When storing, <code>IdentityPair(null,null)</code> will be silently
 *  converted to <code>null</code>.
 *
 *  @author Nathan Bronson
 */
class TIdentityPairRef[A,B](initialA: A, initialB: B
        ) extends impl.Handle[IdentityPair[A,B]] with Ref[IdentityPair[A,B]] {

  def this(initialPair: IdentityPair[A,B]) = this(
      (if (null == initialPair) null.asInstanceOf[A] else initialPair._1),
      (if (null == initialPair) null.asInstanceOf[B] else initialPair._2))

  import TIdentityPairRef._

  protected def handle: impl.Handle[IdentityPair[A,B]] = this

  @volatile private[ccstm] var meta: Long = 0L
  private[ccstm] def metaCAS(before: Long, after: Long) = {
    metaUpdater.compareAndSet(this, before, after)
  }
  private[TIdentityPairRef] def newMetaUpdater = {
    AtomicLongFieldUpdater.newUpdater(classOf[TIdentityPairRef[_,_]], "meta")
  }

  private[ccstm] def ref: AnyRef = this
  private[ccstm] def offset: Int = 0
  private[ccstm] def metaOffset: Int = 0

  @volatile private var _first = initialA
  @volatile private var _second = initialB
  private[ccstm] def data = {
    if (null == _first && null == _second) {
      null
    } else {
      IdentityPair(_first, _second)
    }
  }
  private[ccstm] def data_=(v: IdentityPair[A,B]) {
    if (null == v) {
      _first = null.asInstanceOf[A]
      _second = null.asInstanceOf[B]
    } else {
      _first = v._1
      _second = v._2
    }
  }
}

/* CCSTM - (c) 2009 Stanford University - PPL */

// TAnyRef

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._
import java.util.concurrent.atomic.AtomicLongFieldUpdater


private object TIdentityPairRef {
  val metaUpdater = (new TIdentityPairRef(IdentityPair("",""))).newMetaUpdater
}

/** A <code>Ref</code> implementation that holds only non-null
 *  <code>IdentityPair</code> instances.  When compared to
 *  <code>TAnyRef[IdentityPair[A,B]]</code> instances, instances of
 *  <code>TIdentityPairRef</code> have lower storage overhead (the wrapping
 *  <code>IdentityPair</code> objects are discarded and recreated as needed), but a
 *  slightly higher runtime cost when accessing.
 *
 *  @author Nathan Bronson
 */
class TIdentityPairRef[A,B](initialA: A, initialB: B
        ) extends Ref[IdentityPair[A,B]] with impl.Handle[IdentityPair[A,B]] {

  def this(initialPair: IdentityPair[A,B]) = this(initialPair._1, initialPair._2)

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
  private[ccstm] def data = IdentityPair(_first, _second)
  private[ccstm] def data_=(v: IdentityPair[A,B]) { _first = v._1 ; _second = v._2 }
}
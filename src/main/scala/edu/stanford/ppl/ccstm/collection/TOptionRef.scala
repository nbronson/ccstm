/* CCSTM - (c) 2009 Stanford University - PPL */

// TAnyRef

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._
import java.util.concurrent.atomic.AtomicLongFieldUpdater


private object TOptionRef {
  val SOME_NULL = new AnyRef

  val metaUpdater = (new TOptionRef(None)).newMetaUpdater
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
class TOptionRef[T](initialValue: Option[T]) extends Ref[Option[T]] with impl.Handle[Option[T]] {
  import TOptionRef._

  protected def handle: impl.Handle[Option[T]] = this

  @volatile private[ccstm] var meta: Long = 0L
  private[ccstm] def metaCAS(before: Long, after: Long) = {
    metaUpdater.compareAndSet(this, before, after)
  }
  private[TOptionRef] def newMetaUpdater = {
    AtomicLongFieldUpdater.newUpdater(classOf[TOptionRef[_]], "meta")
  }

  private[ccstm] def ref: AnyRef = this
  private[ccstm] def offset: Int = 0
  private[ccstm] def metaOffset: Int = 0

  @volatile private var _packed = pack(initialValue)
  private[ccstm] def data = unpack(_packed)
  private[ccstm] def data_=(v: Option[T]) { _packed = pack(v) }

  private def unpack(v: AnyRef): Option[T] = {
    v match {
      case null => None
      case SOME_NULL => Some(null.asInstanceOf[T])
      case _ => Some(v.asInstanceOf[T])
    }
  }

  private def pack(o: Option[T]): AnyRef = {
    o match {
      case null => throw new NullPointerException("TOptionRef does not allow null Option references")
      case None => null
      case Some(v) => if (null == v) SOME_NULL else v.asInstanceOf[AnyRef]
    }
  }
}
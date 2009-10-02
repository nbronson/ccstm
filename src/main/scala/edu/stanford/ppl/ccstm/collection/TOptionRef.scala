/* CCSTM - (c) 2009 Stanford University - PPL */

// TRef

package edu.stanford.ppl.ccstm.collection

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater


private object TOptionRef {
  private[TOptionRef] val SOME_NULL = new AnyRef
}

/** A <code>Ref</code> implementation that holds only non-null
 *  <code>Option</code> instances.  When compared to
 *  <code>TRef[Option[T]]</code> instances, instances of
 *  <code>TOptionRef</code> have lower storage overhead (the wrapping
 *  <code>Option</code> objects are discarded and recreated as needed), but a
 *  slightly higher runtime cost when accessing.
 *
 *  @author Nathan Bronson
 */
class TOptionRef[T](initialValue: Option[T]) extends impl.MetaHolder with Ref[Option[T]] with impl.Handle[Option[T]] {
  import TOptionRef._

  protected def handle = this

  private[ccstm] def ref: AnyRef = this
  private[ccstm] def offset: Int = 0

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
      case Some(v) => if (v == null) SOME_NULL else v.asInstanceOf[AnyRef]
    }
  }

  override def toString = {
    "TOptionRef@" + Integer.toHexString(hashCode)
  }
}
/* CCSTM - (c) 2009 Stanford University - PPL */

// TRef

package edu.stanford.ppl.ccstm.collection

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater


private object TOptionRef {
  private[TOptionRef] val SOME_NULL = new AnyRef
  private[TOptionRef] val dataUpdater = (new TOptionRef[Any](None)).newDataUpdater
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
class TOptionRef[T](initialValue: Option[T]) extends Ref[Option[T]] {
  import TOptionRef._

  private trait Accessor {
    def unbind: Ref[AnyRef] = null
    def instance: TOptionRef[_]
    def fieldIndex = 0
    def data: STMImpl.Data[AnyRef] = instance._data
    def data_=(v: STMImpl.Data[AnyRef]) { instance._data = v }
    def dataCAS(before: STMImpl.Data[AnyRef], after: STMImpl.Data[AnyRef]) = {
      TOptionRef.dataUpdater.compareAndSet(instance, before, after)
    }
  }

  //////////////// implementation

  // null means None
  // SOME_NULL means Some(null)

  // The choice to disallow null Option-s is not intrinsic to the
  // packing/unpacking mechanism, it was made based on the typical usage of
  // Option-s.

  @volatile private[ccstm] var _data: STMImpl.Data[AnyRef] = STMImpl.initialData(pack(initialValue))

  private[TOptionRef] def newDataUpdater: AtomicReferenceFieldUpdater[TOptionRef[_],STMImpl.Data[_]] = {
    AtomicReferenceFieldUpdater.newUpdater(classOf[TOptionRef[_]], classOf[STMImpl.Data[_]], "_data")
  }

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

  private def packF[Z](f: Option[T] => Z) = {
    (v: AnyRef) => f(unpack(v))
  }

  private def packTransform(f: Option[T] => Option[T]) = {
    (v: AnyRef) => pack(f(unpack(v)))
  }
  
  private def packPartialTransform(pf: PartialFunction[Option[T],Option[T]]) = {
    new PartialFunction[AnyRef,AnyRef] {
      def isDefinedAt(v: AnyRef) = pf.isDefinedAt(unpack(v))
      def apply(v: AnyRef) = pack(pf(unpack(v)))
    }
  }

  def bind(implicit txn0: Txn): Ref.Bound[Option[T]] = new Wrapped(new STMImpl.TxnAccessor[AnyRef] with Accessor {
    def instance = TOptionRef.this
    def txn = txn0
  })

  def nonTxn: Ref.Bound[Option[T]] = new Wrapped(new STMImpl.NonTxnAccessor[AnyRef] with Accessor {
    def instance = TOptionRef.this
  })

  private class Wrapped(impl: Ref.Bound[AnyRef]) extends Ref.Bound[Option[T]] {

    def unbind = TOptionRef.this
    def context = impl.context

    // BoundSink

    def get = unpack(impl.get)
    def map[Z](f: Option[T] => Z) = impl.map(packF(f))
    def await(pred: Option[T] => Boolean) { impl.await(packF(pred)) }
    def unrecordedRead = new UnrecordedRead[Option[T]] {
      private val uImpl = impl.unrecordedRead

      def context: Option[Txn] = uImpl.context
      def value: Option[T] = unpack(uImpl.value)
      def stillValid: Boolean = uImpl.stillValid
      def recorded: Boolean = uImpl.recorded
    }

    // BoundSource

    def set(v: Option[T]) { impl.set(pack(v)) }
    def tryWrite(v: Option[T]) = impl.tryWrite(pack(v))
    def freeze() { impl.freeze() }

    // Bound

    def readForWrite = unpack(impl.readForWrite)
    def compareAndSet(before: Option[T], after: Option[T]) = impl.compareAndSet(pack(before), pack(after))
    def compareAndSetIdentity[A <: Option[T]](before: A, after: Option[T]): Boolean = {
      throw new UnsupportedOperationException("identity comparisons not useful for ephemeral Options")
    }
    def weakCompareAndSet(before: Option[T], after: Option[T]) = impl.weakCompareAndSet(pack(before), pack(after))
    def weakCompareAndSetIdentity[A <: Option[T]](before: A, after: Option[T]): Boolean = {
      throw new UnsupportedOperationException("identity comparisons not useful for ephemeral Options")
    }
    def transform(f: Option[T] => Option[T]) { impl.transform(packTransform(f)) }
    def transformIfDefined(pf: PartialFunction[Option[T], Option[T]]) = impl.transformIfDefined(packPartialTransform(pf))
  }


  override def toString = {
    "TOptionRef@" + Integer.toHexString(hashCode)
  }
}
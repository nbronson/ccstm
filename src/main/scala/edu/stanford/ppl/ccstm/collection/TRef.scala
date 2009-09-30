/* CCSTM - (c) 2009 Stanford University - PPL */

// TRef

package edu.stanford.ppl.ccstm.collection

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater


private object TRef {
  private[TRef] val dataUpdater = (new TRef[Any](null)).newDataUpdater
}

/** A concrete implementation of <code>Ref</code>.
 *  <p>
 *  This class is not sealed, so it may be opportunistically subclassed to
 *  reduce a level of indirection and the associated storage overheads.
 *
 *  @author Nathan Bronson
 */
class TRef[T](initialValue: T) extends STMImpl.MetadataHolder with Ref[T] {

  private trait Accessor[T] {
    def unbind: TRef[T]
    def data: STMImpl.Data[T] = unbind._data
    def data_=(v: STMImpl.Data[T]) { unbind._data = v }
    def dataCAS(before: STMImpl.Data[T], after: STMImpl.Data[T]) = {
      TRef.dataUpdater.compareAndSet(unbind, before, after)
    }
  }

  //////////////// implementation

  @volatile private[ccstm] var _data: STMImpl.Data[T] = STMImpl.initialData(initialValue)

  private[TRef] def newDataUpdater: AtomicReferenceFieldUpdater[TRef[_],STMImpl.Data[_]] = {
    // this method must be a member of Ref, because all Scala variables are
    // private under the covers
    AtomicReferenceFieldUpdater.newUpdater(classOf[TRef[_]], classOf[STMImpl.Data[_]], "_data")
  }

  def bind(implicit txn0: Txn): Ref.Bound[T] = new STMImpl.TxnAccessor[T] with Accessor[T] {
    def unbind = TRef.this
    def txn = txn0
  }

  def nonTxn: Ref.Bound[T] = new STMImpl.NonTxnAccessor[T] with Accessor[T] {
    def unbind = TRef.this
  }

  override def toString = {
    "TRef@" + Integer.toHexString(hashCode)
  }
}

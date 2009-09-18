/* CCSTM - (c) 2009 Stanford University - PPL */

// TCell

package edu.stanford.ppl.ccstm.collection

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater


private object TCell {
  private[TCell] val dataUpdater = (new TCell[Any](null)).newDataUpdater
}

/** A concrete implementation of <code>Ref</code>.
 *  <p>
 *  This class is not sealed, so it may be opportunistically subclassed to
 *  reduce a level of indirection and the associated storage overheads.
 */
class TCell[T](initialValue: T) extends STMImpl.MetadataHolder with Ref[T] {

  private trait Accessor[T] {
    def instance: TCell[T]
    def fieldIndex = 0
    def metadata = instance._metadata
    def metadata_=(v: STMImpl.Metadata) { instance._metadata = v }
    def metadataCAS(before: STMImpl.Metadata, after: STMImpl.Metadata) = instance._metadataCAS(before, after)
    def data: STMImpl.Data[T] = instance._data
    def data_=(v: STMImpl.Data[T]) { instance._data = v }
    def dataCAS(before: STMImpl.Data[T], after: STMImpl.Data[T]) = {
      TCell.dataUpdater.compareAndSet(instance, before, after)
    }
  }

  //////////////// implementation

  @volatile private[ccstm] var _data: STMImpl.Data[T] = STMImpl.initialData(initialValue)

  private[TCell] def newDataUpdater: AtomicReferenceFieldUpdater[TCell[_],STMImpl.Data[_]] = {
    // this method must be a member of Ref, because all Scala variables are
    // private under the covers
    AtomicReferenceFieldUpdater.newUpdater(classOf[TCell[_]], classOf[STMImpl.Data[_]], "_data")
  }

  def bind(implicit txn0: Txn): Ref.Bound[T] = new STMImpl.TxnAccessor[T] with Accessor[T] {
    def instance = TCell.this
    def txn = txn0
  }

  def nonTxn: Ref.Bound[T] = new STMImpl.NonTxnAccessor[T] with Accessor[T] {
    def instance = TCell.this
  }

  override def toString = {
    "TCell@" + Integer.toHexString(hashCode)
  }
}

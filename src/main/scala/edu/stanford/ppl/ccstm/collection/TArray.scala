/* CCSTM - (c) 2009 Stanford University - PPL */

// TArray

package edu.stanford.ppl.ccstm.collection

import java.util.concurrent.atomic.AtomicReferenceArray

// TODO: fix null initial value for primitive types

object TArray {
  trait Bound[T] extends RandomAccessSeq.Mutable[T] {
    def unbind: TArray[T]
    def context: Option[Txn]
    def length: Int = unbind.length
    def apply(index: Int): T
    def update(index: Int, v: T)
    def refs: RandomAccessSeq[Ref.Bound[T]]
  }
}

class TArray[T](length0: Int) {
  import TArray._

  def length = length0

  def apply(index: Int)(implicit txn: Txn) = getBoundRef(index, txn).get
  def update(index: Int, v: T)(implicit txn: Txn) = getBoundRef(index, txn).set(v)

  def bind(implicit txn: Txn): Bound[T] = new Bound[T] {
    def unbind = TArray.this
    def context = Some(txn)
    def apply(index: Int): T = getBoundRef(index, txn).get
    def update(index: Int, v: T) = getBoundRef(index, txn).set(v)
    def refs: RandomAccessSeq[Ref.Bound[T]] = new RandomAccessSeq[Ref.Bound[T]] {
      def length = length0
      def apply(index: Int) = getBoundRef(index, txn)
    }
  }

  def nonTxn: Bound[T] = new Bound[T] {
    def unbind = TArray.this
    def context = None
    def apply(index: Int): T = getNonTxnRef(index).get
    def update(index: Int, v: T) = getNonTxnRef(index).set(v)
    def refs: RandomAccessSeq[Ref.Bound[T]] = new RandomAccessSeq[Ref.Bound[T]] {
      def length = length0
      def apply(index: Int) = getNonTxnRef(index)
    }
  }

  def refs: RandomAccessSeq[Ref[T]] = new RandomAccessSeq[Ref[T]] {
    def length: Int = length0
    def apply(index0: Int): Ref[T] = getRef(index0)
  }

  /////////////// Internal implementation

  private val _data = {
    val d = new AtomicReferenceArray[STMImpl.Data[T]](length0)
    var i = 0
    if (STMImpl.isInitialDataReusable) {
      val init = STMImpl.initialData(null.asInstanceOf[T])
      while (i < length0) {
        d.set(i, init)
        i += 1
      }
    } else {
      while (i < length0) {
        d.set(i, STMImpl.initialData(null.asInstanceOf[T]))
        i += 1
      }
    }
    d
  }

  private trait Accessor[T] {
    def instance: TArray[T]
    def index: Int
    def unbind: Ref[T] = instance.getRef(index)

    def data: STMImpl.Data[T] = instance._data.get(index)
    def data_=(v: STMImpl.Data[T]) { instance._data.set(index, v) }
    def dataCAS(before: STMImpl.Data[T], after: STMImpl.Data[T]) = {
      instance._data.compareAndSet(index, before, after)
    }
  }

  private trait IndexedRef[T] extends Ref[T] {
    def instance: TArray[T]
    def index: Int
  }

  private def getRef(index0: Int): Ref[T] = new IndexedRef[T] {
    def instance = TArray.this
    def index = index0

    def bind(implicit txn0: Txn) = getBoundRef(index0, txn0)
    def nonTxn = getNonTxnRef(index0)

    override def equals(rhs: Any) = rhs match {
      case x: IndexedRef[_] => (index0 == x.index) && (instance eq x.instance)
      case _ => false
    }
    override def hashCode = System.identityHashCode(instance) * 31 ^ index0
    override def toString = {
      "TArray@" + Integer.toHexString(System.identityHashCode(instance)) + "(" + index0 + ")"
    }
  }

  private def getBoundRef(index0: Int, txn0: Txn): Ref.Bound[T] = new STMImpl.TxnAccessor[T] with Accessor[T] {
    def instance = TArray.this
    def txn = txn0
    def index = index0
  }

  private def getNonTxnRef(index0: Int): Ref.Bound[T] = new STMImpl.NonTxnAccessor[T] with Accessor[T] {
    def instance = TArray.this
    def index = index0
  }
}


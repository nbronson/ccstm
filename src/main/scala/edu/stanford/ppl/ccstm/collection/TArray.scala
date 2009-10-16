/* CCSTM - (c) 2009 Stanford University - PPL */

// TArray

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._
import edu.stanford.ppl.ccstm.impl.{DefaultValue, Handle}
import java.util.concurrent.atomic.{AtomicLongArray, AtomicReferenceArray}

// TODO: select backing store based on manifest

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

class TArray[T](length0: Int)(implicit manifest: scala.reflect.Manifest[T]) {
  import TArray._

  def length = length0

  def apply(index: Int)(implicit txn: Txn) = getRef(index).get
  def update(index: Int, v: T)(implicit txn: Txn) = getRef(index).set(v)

  def bind(implicit txn: Txn): Bound[T] = new Bound[T] {
    def unbind = TArray.this
    def context = Some(txn)
    def apply(index: Int): T = getRef(index).get
    def update(index: Int, v: T) = getRef(index).set(v)
    def refs: RandomAccessSeq[Ref.Bound[T]] = new RandomAccessSeq[Ref.Bound[T]] {
      def length = length0
      def apply(index: Int) = getRef(index).bind
    }
  }

  def nonTxn: Bound[T] = new Bound[T] {
    def unbind = TArray.this
    def context = None
    def apply(index: Int): T = getRef(index).nonTxn.get
    def update(index: Int, v: T) = getRef(index).nonTxn.set(v)
    def refs: RandomAccessSeq[Ref.Bound[T]] = new RandomAccessSeq[Ref.Bound[T]] {
      def length = length0
      def apply(index: Int) = getRef(index).nonTxn
    }
  }

  def refs: RandomAccessSeq[Ref[T]] = new RandomAccessSeq[Ref[T]] {
    def length: Int = length0
    def apply(index0: Int): Ref[T] = getRef(index0)
  }

  /////////////// Internal implementation

  private val _meta = new AtomicLongArray(length0)
  private val _data = ({
    val dv = DefaultValue[T]
    if (null == dv) {
      new AtomicReferenceArray[T](length0)
    } else {
      val a = new Array[AnyRef](length0)
      java.util.Arrays.fill(a, dv.asInstanceOf[AnyRef])
      (new AtomicReferenceArray(a)).asInstanceOf[AtomicReferenceArray[T]]
    }
  })

  private def getRef(index: Int): Ref[T] = new Ref[T] with Handle[T] {

    protected def handle: Handle[T] = this

    private[ccstm] def meta = _meta.get(index)
    private[ccstm] def meta_=(v: Long) { _meta.set(index, v) }
    private[ccstm] def metaCAS(before: Long, after: Long) = _meta.compareAndSet(index, before, after)
    private[ccstm] def ref = TArray.this
    private[ccstm] def offset = index
    private[ccstm] def data = _data.get(index)
    private[ccstm] def data_=(v: T) { _data.set(index, v) } 

    override def toString = {
      "TArray@" + Integer.toHexString(System.identityHashCode(TArray.this)) + "(" + index + ")"
    }
  }
}


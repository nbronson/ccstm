/* CCSTM - (c) 2009 Stanford University - PPL */

// TArray

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._
import edu.stanford.ppl.ccstm.impl.{DefaultValue, Handle}
import java.util.concurrent.atomic.{AtomicLongArray, AtomicReferenceArray}
import scala.collection._

// TODO: select backing store based on manifest

object TArray {
  trait Bound[T] extends mutable.IndexedSeq[T] {
    def unbind: TArray[T]
    def context: Option[Txn]
    def length: Int = unbind.length
    def apply(index: Int): T
    def update(index: Int, v: T)
    def refs: immutable.IndexedSeq[Ref.Bound[T]]
  }

  /** <code>MetaMapping</code> defines the mapping from elements of the array
   *  to the metadata entries used by the STM to protect those elements.
   *  Elements of the array that share a metadata entry may cause conflicts or
   *  contention if accessed by concurrent transactions, at least one of which
   *  is writing.  Minimal contention is realized by having a separate metadata
   *  entry for each array element, but this also maximizes the storage
   *  overhead introduced by <code>TArray</code>.  Minimal storage overhead is
   *  realized by having only a single metadata element, but this prevents
   *  concurrent writes to any element of the array.
   *  <p>
   *  A <code>MetaMapping</code> is defined by three parameters,
   *  <code>dataPerMeta</code>, <code>maxMeta</code>, and
   *  <code>neighboringDataPerMeta</code>.  The first two are used to determine
   *  the number of metadata entries that will be allocated, the last is used
   *  to map indices in the data array to indices in the metadata array.
   */
  sealed case class MetaMapping(dataPerMeta: Int, maxMeta: Int, neighboringDataPerMeta: Int)

  val MaximizeParallelism = MetaMapping(1, Int.MaxValue, 1)
  val MinimizeSpace = MetaMapping(1, 1, 1)
  def Striped(stripeCount: Int) = MetaMapping(1, stripeCount, 1)
  def Chunked(chunkSize: Int) = MetaMapping(chunkSize, Int.MaxValue, chunkSize)
  val DefaultMetaMapping = Striped(16)
}

class TArray[T](length0: Int, metaMapping: TArray.MetaMapping)(implicit manifest: scala.reflect.Manifest[T]) {
  import TArray._

  def this(length0: Int)(implicit manifest: scala.reflect.Manifest[T]) = this(length0, TArray.DefaultMetaMapping)
  def this(data0: Array[T], metaMapping: TArray.MetaMapping)(implicit manifest: scala.reflect.Manifest[T]) = {
    this(data0.length, metaMapping)
    var i = 0
    while (i < data0.length) {
      _data.set(i, data0(i))
      i += 1
    }
  }
  def this(data0: Array[T])(implicit manifest: scala.reflect.Manifest[T]) = this(data0, TArray.DefaultMetaMapping)

  def length = length0

  def apply(index: Int)(implicit txn: Txn) = getRef(index).get
  def update(index: Int, v: T)(implicit txn: Txn) = getRef(index).set(v)

  def bind(implicit txn: Txn): Bound[T] = new Bound[T] {
    def unbind = TArray.this
    def context = Some(txn)
    def apply(index: Int): T = getRef(index).get
    def update(index: Int, v: T) = getRef(index).set(v)
    def refs: immutable.IndexedSeq[Ref.Bound[T]] = new immutable.IndexedSeq[Ref.Bound[T]] {
      def length = length0
      def apply(index: Int) = getRef(index).bind
    }
  }

  def nonTxn: Bound[T] = new Bound[T] {
    def unbind = TArray.this
    def context = None
    def apply(index: Int): T = getRef(index).nonTxn.get
    def update(index: Int, v: T) = getRef(index).nonTxn.set(v)
    def refs: immutable.IndexedSeq[Ref.Bound[T]] = new immutable.IndexedSeq[Ref.Bound[T]] {
      def length = length0
      def apply(index: Int) = getRef(index).nonTxn
    }
  }

  def refs: immutable.IndexedSeq[Ref[T]] = new immutable.IndexedSeq[Ref[T]] {
    def length: Int = length0
    def apply(index0: Int): Ref[T] = getRef(index0)
  }

  /////////////// Internal implementation

  private val _metaIndexShift = { var i = 0 ; while ((1L << i) < metaMapping.neighboringDataPerMeta) i += 1 ; i }
  private val _metaIndexMask = {
    val n = Math.min(length0 / metaMapping.dataPerMeta, metaMapping.maxMeta)
    var m = 1 ; while (m < n) m = (m << 1) + 1
    assert ((m & (m + 1)) == 0)
    m
  }

  private val _meta = new AtomicLongArray(Math.min(_metaIndexMask + 1, length0))
  private def _metaIndex(i: Int) = (i >> _metaIndexShift) & _metaIndexMask
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

    private[ccstm] def metaOffset = _metaIndex(index)

    private[ccstm] def meta = _meta.get(metaOffset)
    private[ccstm] def meta_=(v: Long) { _meta.set(metaOffset, v) }
    private[ccstm] def metaCAS(before: Long, after: Long) = _meta.compareAndSet(metaOffset, before, after)
    private[ccstm] def ref = TArray.this
    private[ccstm] def offset = index
    private[ccstm] def data = _data.get(index)
    private[ccstm] def data_=(v: T) { _data.set(index, v) } 

    override def toString = {
      "TArray@" + Integer.toHexString(System.identityHashCode(TArray.this)) + "(" + index + ")"
    }
  }
}


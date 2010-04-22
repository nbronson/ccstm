/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TArray

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._
import impl.{RefOps, Handle}
import java.util.concurrent.atomic.AtomicLongArray
import scala.reflect.ClassManifest
import scala.collection._

object TArray {
  def apply[T](length: Int)(implicit m: ClassManifest[T]) = new TArray[T](length)
  def apply[T](length: Int, metaMapping: TArray.MetaMapping)(implicit m: ClassManifest[T]) = new TArray[T](length, metaMapping)

  // these are the cases we can do more efficiently
  def apply(data: Array[Int]) = new TArray[Int](AtomicArray(data), TArray.DefaultMetaMapping)
  def apply(data: Array[Int], metaMapping: TArray.MetaMapping) = new TArray[Int](AtomicArray(data), metaMapping)
  def apply(data: Array[Long]) = new TArray[Long](AtomicArray(data), TArray.DefaultMetaMapping)
  def apply(data: Array[Long], metaMapping: TArray.MetaMapping) = new TArray[Long](AtomicArray(data), metaMapping)
  def apply[T <: AnyRef](data: Array[T]) = new TArray[T](AtomicArray(data), TArray.DefaultMetaMapping)
  def apply[T <: AnyRef](data: Array[T], metaMapping: TArray.MetaMapping) = new TArray[T](AtomicArray(data), metaMapping)

  // this is the general case
  def apply[T](data: Traversable[T])(implicit m: ClassManifest[T]) = new TArray[T](data)
  def apply[T](data: Traversable[T], metaMapping: TArray.MetaMapping)(implicit m: ClassManifest[T]) = new TArray[T](data, metaMapping)

  
  trait View[T] extends mutable.IndexedSeq[T] {
    def unbind: TArray[T]
    def mode: AccessMode
    def length: Int = unbind.length
    def apply(index: Int): T
    def update(index: Int, v: T)
    def refs: immutable.IndexedSeq[Ref.View[T]]
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

class TArray[T](private val _data: AtomicArray[T], metaMapping: TArray.MetaMapping) {
  import TArray._

  def this(length0: Int, metaMapping: TArray.MetaMapping)(implicit m: ClassManifest[T]) =
    this(AtomicArray[T](length0), metaMapping)
  def this(length0: Int)(implicit m: ClassManifest[T]) = this(length0, TArray.DefaultMetaMapping)

  def this(data0: Traversable[T], metaMapping: TArray.MetaMapping)(implicit m: ClassManifest[T]) =
    this(AtomicArray[T](data0), metaMapping)
  def this(data0: Traversable[T])(implicit m: ClassManifest[T]) = this(data0, TArray.DefaultMetaMapping)

  def length = _data.length

  def apply(index: Int)(implicit txn: Txn) = getRef(index).get
  def update(index: Int, v: T)(implicit txn: Txn) = getRef(index).set(v)

  def bind(implicit txn: Txn): View[T] = new View[T] {
    def unbind = TArray.this
    def mode: AccessMode = txn
    def apply(index: Int): T = getRef(index).get
    def update(index: Int, v: T) = getRef(index).set(v)
    def refs: immutable.IndexedSeq[Ref.View[T]] = new immutable.IndexedSeq[Ref.View[T]] {
      def length = unbind.length
      def apply(index: Int) = getRef(index).bind
    }
  }

  def single: View[T] = new View[T] {
    def unbind = TArray.this
    def mode: AccessMode = Single
    def apply(index: Int): T = getRef(index).single.get
    def update(index: Int, v: T) = getRef(index).single.set(v)
    def refs: immutable.IndexedSeq[Ref.View[T]] = new immutable.IndexedSeq[Ref.View[T]] {
      def length = unbind.length
      def apply(index: Int) = getRef(index).single
    }
  }

  def escaped: View[T] = new View[T] {
    def unbind = TArray.this
    def mode: AccessMode = Escaped
    def apply(index: Int): T = getRef(index).escaped.get
    def update(index: Int, v: T) = getRef(index).escaped.set(v)
    def refs: immutable.IndexedSeq[Ref.View[T]] = new immutable.IndexedSeq[Ref.View[T]] {
      def length = unbind.length
      def apply(index: Int) = getRef(index).escaped
    }
  }

  @deprecated("consider replacing with TArray.single")
  def nonTxn = escaped

  def refs: immutable.IndexedSeq[Ref[T]] = new immutable.IndexedSeq[Ref[T]] {
    def length: Int = TArray.this.length
    def apply(index0: Int): Ref[T] = getRef(index0)
  }

  /////////////// Internal implementation

  private val _metaIndexShift = { var i = 0 ; while ((1L << i) < metaMapping.neighboringDataPerMeta) i += 1 ; i }
  private val _metaIndexMask = {
    val n = Math.min(length / metaMapping.dataPerMeta, metaMapping.maxMeta)
    var m = 1 ; while (m < n) m = (m << 1) + 1
    assert ((m & (m + 1)) == 0)
    m
  }

  private val _meta = new AtomicLongArray(Math.min(_metaIndexMask + 1, length))
  private def _metaIndex(i: Int) = (i >> _metaIndexShift) & _metaIndexMask

  private def getRef(index: Int): Ref[T] = new Handle[T] with RefOps[T] {

    private[ccstm] def handle: Handle[T] = this

    private[ccstm] def metaOffset = _metaIndex(index)

    private[ccstm] def meta = _meta.get(metaOffset)
    private[ccstm] def meta_=(v: Long) { _meta.set(metaOffset, v) }
    private[ccstm] def metaCAS(before: Long, after: Long) = _meta.compareAndSet(metaOffset, before, after)
    private[ccstm] def ref = TArray.this
    private[ccstm] def offset = index
    private[ccstm] def data = _data(index)
    private[ccstm] def data_=(v: T) { _data(index) = v } 

    override def toString = {
      "TArray@" + Integer.toHexString(System.identityHashCode(TArray.this)) + "(" + index + ")"
    }
  }
}


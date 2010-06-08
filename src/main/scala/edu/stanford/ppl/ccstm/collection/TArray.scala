/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TArray

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._
import impl.{RefOps, Handle}
import java.util.concurrent.atomic.AtomicLongArray
import scala.reflect.ClassManifest
import scala.collection._

/** An object that provides factory methods for `TArray` instances.
 *
 *  @author Nathan bronson
 */
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


  /** A view that supports accesses to a `TArray` instance outside the static
   *  scope of a `Txn`.
   *  @see edu.stanford.ppl.ccstm.Ref.View
   */
  trait View[T] extends mutable.IndexedSeq[T] {
    /** The `TArray` from which this view was created. */
    def unbind: TArray[T]

    def mode: AccessMode
    def length: Int = unbind.length

    /** Reads the `index`th element of `unbind` in the bound context. */
    def apply(index: Int): T

    /** Writes the `index`th element of `unbind` in the bound context. */ 
    def update(index: Int, v: T)

    /** Generates `Ref.View` instances on demand for elements of `unbind`.  All
     *  operations on the returned element views are supported.
     */
    def refs: immutable.IndexedSeq[Ref.View[T]]
  }

  /** `MetaMapping` defines the mapping from elements of the array
   *  to the metadata entries used by the STM to protect those elements.
   *  Elements of the array that share a metadata entry may cause conflicts or
   *  contention if accessed by concurrent transactions, at least one of which
   *  is writing.  Minimal contention is realized by having a separate metadata
   *  entry for each array element, but this also maximizes the storage
   *  overhead introduced by `TArray`.  Minimal storage overhead is
   *  realized by having only a single metadata element, but this prevents
   *  concurrent writes to any element of the array.
   *
   *  A `MetaMapping` is defined by three parameters, `dataPerMeta`, `maxMeta`,
   *  and `neighboringDataPerMeta`.  The first two are used to determine
   *  the number of metadata entries that will be allocated, the last is used
   *  to map indices in the data array to indices in the metadata array.
   *
   *  Several predefined `MetaMapping` instances are present in the `TArray`
   *  object.
   */
  sealed case class MetaMapping(dataPerMeta: Int, maxMeta: Int, neighboringDataPerMeta: Int)

  /** The `MetaMapping` that maximizes parallelism at the expense of space. */
  val MaximizeParallelism = MetaMapping(1, Int.MaxValue, 1)

  /** The `MetaMapping` that absolutely minimizes space, at the expense of
   *  serializing writes to separate locations by concurrent transactions.
   */
  val MinimizeSpace = MetaMapping(1, 1, 1)

  /** A `MetaMapping` that allows neighboring entries to be written in parallel
   *  by concurrent transactions, but that fixes the total metadata independent
   *  of the data size.  This policy is vulnerable to the birthday paradox, but
   *  works well when contention is not too high.
   */
  def Striped(stripeCount: Int) = MetaMapping(1, stripeCount, 1)

  /** A `MetaMapping` that evenly divides the index space into a fixed number
   *  of chunks, and allows parallelism only for concurrent writes to separate
   *  chunks. This policy is vulnerable to the birthday paradox.
   */
  def Chunked(chunkSize: Int) = MetaMapping(chunkSize, Int.MaxValue, chunkSize)

  /** `Striped(16)`. */
  val DefaultMetaMapping = Striped(16)
}

/** Bulk transactional storage, roughly equivalent to `Array[Ref[T]]` but much
 *  more efficient.  Elements are stored internally without boxing, and the
 *  mapping from data to metadata can be configured to maximize the potential
 *  parallelism or to minimize space overheads.  The length cannot be changed.
 *
 *  Elements can be read and written directly, or transient `Ref` instances can
 *  be obtained (`array.refs(index)`) for more sophisticated operations.
 */
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

  /** Returns a sequence that will produce transient `Ref` instances that are
   *  backed by elements of this `TArray`.  This allows use of all of `Ref`'s
   *  functionality for reading, writing, and transforming elements.
   */
  def refs: immutable.IndexedSeq[Ref[T]] = new immutable.IndexedSeq[Ref[T]] {
    def length: Int = TArray.this.length
    def apply(index0: Int): Ref[T] = getRef(index0)
  }

  /////////////// Internal implementation

  private val _metaIndexShift = { var i = 0 ; while ((1L << i) < metaMapping.neighboringDataPerMeta) i += 1 ; i }
  private val _metaIndexMask = {
    val n = math.min(length / metaMapping.dataPerMeta, metaMapping.maxMeta)
    var m = 1 ; while (m < n) m = (m << 1) + 1
    assert ((m & (m + 1)) == 0)
    m
  }

  private val _meta = new AtomicLongArray(math.min(_metaIndexMask + 1, length))
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


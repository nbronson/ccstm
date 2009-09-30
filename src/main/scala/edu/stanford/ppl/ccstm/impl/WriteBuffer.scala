/* CCSTM - (c) 2009 Stanford University - PPL */

// WriteBuffer

package edu.stanford.ppl.ccstm.impl


private [impl] object WriteBuffer {
  trait Visitor {
    def visit(ref: AnyRef, offset: Int, specValue: Any): Boolean
  }
}

/** A map keyed on a reference and integer pair, where the reference portion of
 *  the key is used for identity only (System.identityHashCode and eq).  Only
 *  get, put, and visitation are supported.
 *  <p>
 *  The underlying implementation uses Hopscotch Hashing (Herlihy, Shavit, and
 *  Tzafrir).  To keep object allocation low, entries are striped across
 *  conceptual arrays, and then those arrays are interleaved to further reduce
 *  object overheads and increase cache locality.  Memory consumption on 32-bit
 *  (or 64-bit with compressed OOPs) is 80+16*buckets bytes, on 64-bit is
 *  128+24*buckets bytes.  The maximum load factor is 2/3, so 8 buckets leads
 *  to a capacity before resize of 5 and a memory usage of 208 or 320 bytes.
 *  16 initial buckets gives an initial capacity of 10 with 336 or 512 bytes.
 *  32 initial buckets gives an initial capacity of 20 with 592 or 896 bytes.
 */
private [impl] class WriteBuffer(initialBuckets: Int) {
  /** Constructs a write buffer with 16 buckets, and an initial capacity of 10. */
  def this() = this(16)

  private var _size = 0
  def size = _size

  /** Reference key i is at 2*i, speculative value i is at 2*i+1. */
  private var _objs = new Array[AnyRef](2 * initialBuckets)

  /** Hop i is at 2*i, offset key i is at 2*i+1. */
  private var _ints = new Array[Int](2 * initialBuckets)

  private def capacity = _ints.length / 2

  private def hopI(i: Int) = 2 * i
  private def refI(i: Int) = 2 * i
  private def offsetI(i: Int) = 2 * i + 1
  private def specValueI(i: Int) = 2 * i + 1 

  private def hash(ref: AnyRef, offset: Int) = {
    // Hopscotch will fail if there are more than H entries that end up in the
    // same bucket.  This can lead to a pathological case if a single instance
    // is used with offsets that are strided by a power-of-two, because if we
    // use a simple hashCode(ref) op M*offset, we will fail to get any unique
    // bits from the M*offset portion.  Our solution is to use the bit-mixing
    // function from java.util.HashMap after merging with the system identity
    // hash code.
    if (ref == null) throw new NullPointerException
    var h = System.identityHashCode(ref) ^ (0x40108097 * offset)
    h ^= (h >>> 20) ^ (h >>> 12)
    h ^= (h >>> 7) ^ (h >>> 4)
    h
  }

  /** Returns the index on a hit, -1 on failure. */
  private def find(ref: AnyRef, offset: Int): Int = {
    val n = capacity
    var i = hash(ref, offset) & (n - 1)
    var mask = _ints(hopI(i))
    while (mask != 0) {
      if ((mask & 1) != 0) {
        if ((_objs(refI(i)) eq ref) && (_ints(offsetI(i)) == offset)) return i
      }
      mask >>>= 1
      i = (i + 1) & (n - 1)
    }
    return -1
  }

  private def findOrAdd(ref: AnyRef, offset: Int): Int = {
    // max load factor is 2/3
    if (_size * 3 >= capacity * 2) {
      grow()
    }

    val n = capacity
    val i0 = hash(ref, offset) & (n - 1)

    // search for the entry
    var mask = _ints(hopI(i0))
    var i = i0
    while (mask != 0) {
      if ((mask & 1) != 0) {
        if ((_objs(refI(i)) eq ref) && (_ints(offsetI(i)) == offset)) return i
      }
      mask >>>= 1
      i = (i + 1) & (n - 1)
    }

    // keep going until we find an empty entry
    while (_objs(refI(i)) ne null) {
      i = (i + 1) & (n - 1)
    }

    do {
      val dist = (i - i0) & (n - 1)
      if (dist < 32) {
        // within range
        _size += 1
        _ints(offsetI(i)) = offset
        _ints(hopI(i0)) |= (1 << dist)
        _objs(refI(i)) = ref
        return i
      }

      // find an entry within the 31 previous that can be moved to i
      i = hopscotch(i, n)
    } while (i != -1)

    // hopscotch failed
    grow()
    return findOrAdd(ref, offset)
  }

  private def hopscotch(empty: Int, n: Int): Int = {
    var i = (empty - 32 + 1) & (n - 1)
    do {
      val i0 = hash(_objs(refI(i)), _ints(offsetI(i))) & (n - 1)
      val dist = (empty - i0) & (n - 1)
      if (dist < 32) {
        // entry at i hashed to i0, and i0 is in range of empty
        _objs(specValueI(empty)) = _objs(specValueI(i))
        _objs(refI(empty)) = _objs(refI(i))
        _objs(refI(i)) = null
        _ints(offsetI(empty)) = _ints(offsetI(i))
        _ints(hopI(i0)) = (_ints(hopI(i0)) | (1 << dist)) & ~(1 << ((i - i0) & (n - 1)))
        return i
      }
      i = (i + 1) & (n - 1)
    } while (i != empty)
    return -1
  }


  def get[T](ref: AnyRef, offset: Int, defaultValue: T): T = {
    val i = find(ref, offset)
    if (i == -1) defaultValue else _objs(specValueI(i)).asInstanceOf[T]
  }

  def put[T](ref: AnyRef, offset: Int, value: T) {
    val i = findOrAdd(ref, offset)
    _objs(specValueI(i)) = value.asInstanceOf[AnyRef]
  }

  def visit(visitor: WriteBuffer.Visitor): Boolean = {
    val n = capacity
    var i = 0
    while (i < n) {
      val ref = _objs(refI(i))
      if (ref ne null) {
        if (!visitor.visit(ref, _ints(offsetI(i)), _objs(specValueI(i)))) return false
      }
      i += 1
    }
    return true
  }


  private def grow() {
    val oldInts = _ints
    val oldObjs = _objs
    val n = capacity
    _size = 0
    _ints = new Array[Int](n * 4)
    _objs = new Array[AnyRef](n * 4)
    var i = 0
    while (i < n) {
      if (oldObjs(refI(i)) ne null) {
        put(oldObjs(refI(i)), oldInts(offsetI(i)), oldObjs(specValueI(i)))
      }
      i += 1
    }
  }

  override def toString = {
    val buf = new StringBuilder
    buf.append("WriteBuffer(size=").append(_size).append("\n")
    buf.append("%8s | %16s | %7s | %s\n".format("hops", "refs", "offsets", "specValues"))
    for (i <- 0 until capacity) {
      buf.append("%08x | %16s | %7d | %s\n".format(_ints(hopI(i)), _objs(refI(i)), _ints(offsetI(i)), _objs(specValueI(i))))
    }
    buf.append(")")
    buf.toString
  }
}
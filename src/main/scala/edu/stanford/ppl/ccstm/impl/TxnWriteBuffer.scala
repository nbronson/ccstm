/* CCSTM - (c) 2009 Stanford University - PPL */

// WriteBuffer

package edu.stanford.ppl.ccstm.impl


private [impl] object TxnWriteBuffer {
  trait Visitor {
    def visit(specValue: Any, handle: Handle[_]): Boolean
  }
}

/** A map keyed on a reference and integer pair, with a pair of value
 *  associated with each key.  The reference portion of the key is used for
 *  identity only (System.identityHashCode and eq).  Only get, put, and
 *  visitation are supported.  The second value is available only during
 *  visitation.
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
 *  <p>
 *  This is designed to be mixed in to the actual Txn implementation, to remove
 *  a layer of indirection, so the non-private methods are named to make it
 *  clear that they involve the write buffer.
 */
private [impl] trait TxnWriteBuffer {
  import STMImpl.hash

  protected def writeBufferInitialCapacity = 16

  private var _size = 0
  private[impl] def writeBufferSize = _size

  /** Reference key i is at 3*i, specValue is at 3*i+1, handle is at 3*i+2. */
  private var _objs: Array[AnyRef] = null

  /** Hop i is at 2*i, offset key i is at 2*i+1. */
  private var _ints: Array[Int] = null

  private def capacity = _ints.length / 2

  private def hopI(i: Int) = 2 * i
  private def offsetI(i: Int) = 2 * i + 1
  private def refI(i: Int) = 3 * i
  private def specValueI(i: Int) = 3 * i + 1
  private def handleI(i: Int) = 3 * i + 2

  /** Returns the index on a hit, -1 on failure. */
  private def find(ref: AnyRef, offset: Int): Int = {
    if (_size == 0) return -1

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
    if (_size == 0) {
      if (_objs == null) {
        // lazy initialization
        val n = writeBufferInitialCapacity
        _objs = new Array[AnyRef](3 * n)
        _ints = new Array[Int](2 * n)
      }
    } else if (_size * 3 >= capacity * 2) {
      // max load factor is 2/3
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
        _objs(handleI(empty)) = _objs(handleI(i))
        _objs(refI(empty)) = _objs(refI(i))
        _objs(refI(i)) = null
        _objs(handleI(i)) = null
        _ints(offsetI(empty)) = _ints(offsetI(i))
        _ints(hopI(i0)) = (_ints(hopI(i0)) | (1 << dist)) & ~(1 << ((i - i0) & (n - 1)))
        return i
      }
      i = (i + 1) & (n - 1)
    } while (i != empty)
    return -1
  }

  /** Returns the specValue associated with (ref,offset). */
  private[impl] def writeBufferGet[T](handle: Handle[T]): T = {
    val i = find(handle.ref, handle.offset)
    if (i == -1) handle.data else _objs(specValueI(i)).asInstanceOf[T]
  }

  /** Adds or updates a write buffer entry. */
  private[impl] def writeBufferPut[T](handle: Handle[T], specValue: T) {
    val i = findOrAdd(handle.ref, handle.offset)
    _objs(specValueI(i)) = specValue.asInstanceOf[AnyRef]
    _objs(handleI(i)) = handle
  }

  /** Creates a write buffer entry if one does not already exist, initializing
   *  it from <code>handle.data</code> if creating.  Returns the resulting
   *  <code>specValue</code>.
   */
  private[impl] def writeBufferGetForPut[T](handle: Handle[T]): T = {
    val i = findOrAdd(handle.ref, handle.offset)
    if (_objs(handleI(i)) == null) {
      // inserting
      val z = handle.data
      _objs(specValueI(i)) = z.asInstanceOf[AnyRef]
      _objs(handleI(i)) = handle
      z
    } else {
      // already present
      _objs(specValueI(i)).asInstanceOf[T]
    }
  }

  private[impl] def writeBufferVisit(visitor: TxnWriteBuffer.Visitor): Boolean = {
    if (_size == 0) return true
    
    val n = capacity
    var i = 0
    while (i < n) {
      val handle = _objs(handleI(i))
      if (handle ne null) {
        if (!visitor.visit(_objs(specValueI(i)), handle.asInstanceOf[Handle[_]])) return false
      }
      i += 1
    }
    return true
  }

  private[impl] def writeBufferDestroy() {
    _ints = null
    _objs = null
  }


  private def grow() {
    val oldSize = _size
    val oldInts = _ints
    val oldObjs = _objs
    val n = capacity
    _size = 0
    _ints = new Array[Int](n * 4)
    _objs = new Array[AnyRef](n * 6)
    var i = 0
    while (i < n) {
      val ref = oldObjs(refI(i))
      if (ref ne null) {
        val j = findOrAdd(ref, oldInts(offsetI(i)))
        _objs(specValueI(j)) = oldObjs(specValueI(i))
        _objs(handleI(j)) = oldObjs(handleI(i))
      }
      i += 1
    }
    assert(_size == oldSize)
  }

  private[impl] def writeBufferStr = {
    val buf = new StringBuilder
    buf.append("TxnWriteBuffer(size=").append(_size).append("\n")
    buf.append("%8s | %16s | %7s | %16s | %s\n".format("hops", "refs", "offsets", "specValue", "handle"))
    for (i <- 0 until capacity) {
      buf.append("%08x | %16s | %7d | %16s | %s\n".format(
        _ints(hopI(i)), _objs(refI(i)), _ints(offsetI(i)), _objs(specValueI(i)), _objs(handleI(i))))
    }
    buf.append(")")
    buf.toString
  }
}
/* CCSTM - (c) 2009 Stanford University - PPL */

// HopscotchLIHashTable

package edu.stanford.ppl.ccstm.impls


/** A non-concurrent hopscotch hash table implementation that has a compound
 *  key (a reference compared using eq and an integer offset).  There are no
 *  entry objects, rather rather the hash table functions return indexes into
 *  a subclass-managed array (or set of arrays), along with hooks to perform
 *  resizing.  Deletion is not currently supported.  The initial capacity must
 *  be a power of two.
 *  <p>
 *  While this hash table uses slightly more memory than a linear probing one
 *  for a given load factor, it can tolerate higher load factors, so it doesn't
 *  actually use more memory in practice. 
 */
private[impls] abstract class HopscotchLIHashTable(initCap: Int) {
  /*protected*/ var _size = 0
  /*protected*/ var _hops = new Array[Int](initCap)
  /*protected*/ var _refs = new Array[AnyRef](initCap)
  /*protected*/ var _offsets = new Array[Int](initCap)

  /** Grow must be implemented by the subclass.  It should zero the size,
   *  allocate new (fresh, not copied) _hops, _refs, and _offsets arrays, and
   *  then re-add all of the entries.
   */
  protected def grow()

  /** Relocation only needs to handle user-managed arrays (unlike grow). */
  protected def relocate(src: Int, dst: Int)


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
  protected def find(ref: AnyRef, offset: Int): Int = {
    val n = _hops.length
    var i = hash(ref, offset) & (n - 1)
    var mask = _hops(i)
    while (mask != 0) {
      if ((mask & 1) != 0) {
        if ((_refs(i) eq ref) && (_offsets(i) == offset)) return i
      }
      mask >>>= 1
      i = (i + 1) & (n - 1)
    }
    return -1
  }

  /** Returns the index if already present, index - _refs.length if added. */
  protected def findOrAdd(ref: AnyRef, offset: Int): Int = {
    // max load factor is 2/3
    if (_size * 3 >= _hops.length * 2) {
      grow()
    }

    val n = _hops.length
    val i0 = hash(ref, offset) & (n - 1)

    // search for the entry
    var mask = _hops(i0)
    var i = i0
    while (mask != 0) {
      if ((mask & 1) != 0) {
        if ((_refs(i) eq ref) && (_offsets(i) == offset)) return i
      }
      mask >>>= 1
      i = (i + 1) & (n - 1)
    }

    // keep going until we find an empty entry
    while (_refs(i) ne null) {
      i = (i + 1) & (n - 1)
    }

    do {
      val dist = (i - i0) & (n - 1)
      if (dist < 32) {
        // within range
        _size += 1
        _hops(i0) |= (1 << dist)
        _refs(i) = ref
        _offsets(i) = offset
        return i - n
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
      val i0 = hash(_refs(i), _offsets(i)) & (n - 1)
      val dist = (empty - i0) & (n - 1)
      if (dist < 32) {
        // entry at i hashed to i0, and i0 is in range of empty
        relocate(i, empty)
        _refs(empty) = _refs(i)
        _refs(i) = null
        _offsets(empty) = _offsets(i)
        _hops(i0) = (_hops(i0) | (1 << dist)) & ~(1 << ((i - i0) & (n - 1)))
        return i
      }
      i = (i + 1) & (n - 1)
    } while (i != empty)
    return -1
  }
}
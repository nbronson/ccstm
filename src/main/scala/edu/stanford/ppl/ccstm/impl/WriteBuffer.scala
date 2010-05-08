/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// WriteBuffer

package edu.stanford.ppl.ccstm.impl

import annotation.tailrec


private[impl] object WriteBuffer {
  trait Visitor {
    def visit(handle: Handle[_], specValue: Any): Boolean
  }
}

/** Maps Handle[T] to specValue.  Handles are compared using the ref and
 *  offset.
 */
private[impl] class WriteBuffer {
  private def InitialCap = 16
  private def InitialRealCap = 512
  private def MaxInitialRealCap = 8 * InitialRealCap

  // This write buffer implementation uses chaining, but instead of storing the
  // buckets in objects, they are packed into the arrays bucketAnys and
  // bucketInts.  Pointers to a bucket are represented as a 1-based int, with 0
  // representing nil.  The dispatch array holds the entry index for the
  // beginning of a bucket chain.

  private var _size = 0
  private var _cap = InitialCap
  private var _bucketAnys = new Array[AnyRef](bucketAnysLen(InitialRealCap))
  private var _bucketInts = new Array[Int](bucketIntsLen(InitialRealCap))
  private var _dispatch = new Array[Int](InitialRealCap)

  private def refI(i: Int) = 3 * (i - 1)
  private def specValueI(i: Int) = 3 * (i - 1) + 1
  private def handleI(i: Int) = 3 * (i - 1) + 2
  private def offsetI(i: Int) = 2 * (i - 1)
  private def nextI(i: Int) = 2 * (i - 1) + 1

  private def bucketAnysLen(c: Int) = 3 * (maxSizeForCap(c) + 1)
  private def bucketIntsLen(c: Int) = 2 * (maxSizeForCap(c) + 1)

  private def maxSizeForCap(c: Int) = c - c / 4
  private def shouldGrow(s: Int, c: Int): Boolean = s > maxSizeForCap(c)
  private def shouldGrow: Boolean = shouldGrow(_size, _cap)


  def isEmpty = _size == 0
  def size = _size

  def get[@specialized(Int) T](handle: Handle[T]): T = {
    val i = find(handle)
    if (i != 0) {
      // hit
      _bucketAnys(specValueI(i)).asInstanceOf[T]
    } else {
      // miss
      handle.data
    }
  }

  private def find(handle: Handle[_]): Int = {
    val ref = handle.ref
    val offset = handle.offset
    find(ref, offset, _dispatch(STMImpl.hash(ref, offset) & (_cap - 1)))
  }

  @tailrec
  private def find(ref: AnyRef, offset: Int, i: Int): Int = {
    if (i == 0 || ((ref eq _bucketAnys(refI(i))) && offset == _bucketInts(offsetI(i)))) {
      i
    } else {
      find(ref, offset, _bucketInts(nextI(i)))
    }
  }


  def put[@specialized(Int) T](handle: Handle[T], value: T): Unit = {
    val ref = handle.ref
    val offset = handle.offset
    val slot = STMImpl.hash(ref, offset) & (_cap - 1)
    val i = find(ref, offset, _dispatch(slot))
    if (i != 0) {
      // hit, update an existing entry
      _bucketAnys(specValueI(i)) = value.asInstanceOf[AnyRef]
    } else {
      // miss, create a new entry
      append(ref, offset, handle, value, slot)
    }
  }

  def allocatingGet[@specialized(Int) T](handle: Handle[T]): T = {
    return _bucketAnys(specValueI(findOrAllocate(handle))).asInstanceOf[T]
  }

  def getAndTransform[@specialized(Int) T](handle: Handle[T], func: T => T): T = {
    val i = findOrAllocate(handle)
    val before = _bucketAnys(specValueI(i)).asInstanceOf[T]
    _bucketAnys(specValueI(i)) = func(before).asInstanceOf[AnyRef]
    return before
  }

  private def findOrAllocate(handle: Handle[_]): Int = {
    val ref = handle.ref
    val offset = handle.offset
    val slot = STMImpl.hash(ref, offset) & (_cap - 1)
    val i = find(ref, offset, _dispatch(slot))
    if (i != 0) {
      // hit
      return i
    } else {
      // miss, create a new entry using the existing data value
      return append(ref, offset, handle, handle.data, slot)
    }
  }

  private def append(ref: AnyRef, offset: Int, handle: Handle[_], value: Any, slot: Int): Int = {
    val s = _size + 1
    _bucketAnys(refI(s)) = ref
    _bucketAnys(specValueI(s)) = value.asInstanceOf[AnyRef]
    _bucketAnys(handleI(s)) = handle
    _bucketInts(offsetI(s)) = offset
    _bucketInts(nextI(s)) = _dispatch(slot)
    _dispatch(slot) = s
    _size = s

    if (shouldGrow) grow()

    // grow() relinks the buckets but doesn't move them, so s is still valid
    return s
  }

  private def grow(): Unit = {
    // adjust capacity
    _cap *= 2
    if (_cap > _dispatch.length) {
      // we actually need to reallocate
      _bucketAnys = java.util.Arrays.copyOf(_bucketAnys, bucketAnysLen(_cap))
      _bucketInts = java.util.Arrays.copyOf(_bucketInts, bucketIntsLen(_cap))
      _dispatch = new Array[Int](_cap)
    } else {
      java.util.Arrays.fill(_dispatch, 0, _cap, 0)
    }

    // relink the dispatch array
    var i = 1
    while (i <= _size) {
      val slot = STMImpl.hash(_bucketAnys(refI(i)), _bucketInts(offsetI(i))) & (_cap - 1)
      _bucketInts(nextI(i)) = _dispatch(slot)
      _dispatch(slot) = i
      i += 1
    }
  }

  def clear(): Unit = {
    if (_cap <= MaxInitialRealCap) {
      // null out the existing arrays
      var i = _size
      while (i > 0) {
        _bucketAnys(refI(i)) = null
        _bucketAnys(specValueI(i)) = null
        _bucketAnys(handleI(i)) = null
        i -= 1
      }

      // zero the initial portion of the dispatch array
      java.util.Arrays.fill(_dispatch, 0, InitialCap, 0)
    } else {
      // discard the existing big ones, to prevent bloat
      _bucketAnys = new Array[AnyRef](bucketAnysLen(InitialRealCap))
      _bucketInts = new Array[Int](bucketIntsLen(InitialRealCap))
      _dispatch = new Array[Int](InitialRealCap)
    }

    _size = 0
    _cap = InitialCap
  }

  def visitHandle(index: Int) = _bucketAnys(handleI(index)).asInstanceOf[Handle[_]]
  def visitSpecValue(index: Int) = _bucketAnys(specValueI(index))

  def visit(visitor: WriteBuffer.Visitor): Boolean = {
    var i = _size
    while (i != 0) {
      if (!visitor.visit(_bucketAnys(handleI(i)).asInstanceOf[Handle[_]], _bucketAnys(specValueI(i)))) {
        return false
      }
      i -= 1
    }
    return true
  }
}
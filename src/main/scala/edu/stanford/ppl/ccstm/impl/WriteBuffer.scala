/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// WriteBuffer2

package edu.stanford.ppl.ccstm.impl

import annotation.tailrec

private[impl] object WriteBuffer {
  trait Visitor {
    def visit(handle: Handle[_], specValue: Any): Boolean
  }
}

private[impl] class WriteBuffer {
  private def InitialCap = 16
  private def InitialRealCap = 512
  private def MaxInitialRealCap = 8 * InitialRealCap

  // This write buffer implementation uses chaining, but instead of storing the
  // buckets in objects, they are packed into the arrays bucketAnys and
  // bucketInts.  Pointers to a bucket are represented as a 1-based int, with 0
  // representing nil.  The dispatch array holds the entry index for the
  // beginning of a bucket chain.
  //
  // When a nested context is created with push(), the current number of
  // allocated buckets is recorded in undoThreshold.  Any changes to the
  // speculative value for a bucket with an index less than this threshold are
  // logged to allow partial rollback.  Buckets with an index greater than the
  // undoThreshold can be discarded during rollback.  This means that despite
  // nesting, each <ref,offset> key is stored at most once in the write buffer.

  /** The maximum index for which undo entries are required. */
  private var _undoThreshold = 0
  private var _undoLog: WBUndoLog = null

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
  
  def get[T](handle: Handle[T]): T = {
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
  

  def put[T](handle: Handle[T], value: T): Unit = {
    val ref = handle.ref
    val offset = handle.offset
    val slot = STMImpl.hash(ref, offset) & (_cap - 1)
    val head = _dispatch(slot)
    val i = find(ref, offset, head)
    if (i != 0) {
      // hit, update an existing entry, optionally with undo
      if (i <= _undoThreshold) _undoLog.record(i, _bucketAnys(specValueI(i)))
      _bucketAnys(specValueI(i)) = value.asInstanceOf[AnyRef]
    } else {
      // miss, create a new entry
      append(ref, offset, handle, value, slot, head)
    }
  }

  def allocatingGet[T](handle: Handle[T]): T = {
    return _bucketAnys(specValueI(findOrAllocate(handle))).asInstanceOf[T]
  }

  def getAndTransform[T](handle: Handle[T], func: T => T): T = {
    val i = findOrAllocate(handle)
    val before = _bucketAnys(specValueI(i)).asInstanceOf[T]
    _bucketAnys(specValueI(i)) = func(before).asInstanceOf[AnyRef]
    return before
  }

  private def findOrAllocate(handle: Handle[_]): Int = {
    val ref = handle.ref
    val offset = handle.offset
    val slot = STMImpl.hash(ref, offset) & (_cap - 1)
    val head = _dispatch(slot)
    val i = find(ref, offset, head)
    if (i != 0) {
      // hit, undo log entry is required to capture the potential reads that
      // won't be recorded in this nested txn's read set
      if (i <= _undoThreshold) _undoLog.record(i, _bucketAnys(specValueI(i)))
      return i
    } else {
      // miss, create a new entry using the existing data value
      return append(ref, offset, handle, handle.data, slot, head)
    }
  }

  private def append(ref: AnyRef, offset: Int, handle: Handle[_], value: Any, slot: Int, head: Int): Int = {
    val s = _size + 1
    _bucketAnys(refI(s)) = ref
    _bucketAnys(specValueI(s)) = value.asInstanceOf[AnyRef]
    _bucketAnys(handleI(s)) = handle
    _bucketInts(offsetI(s)) = offset
    _bucketInts(nextI(s)) = head
    if (slot >= 0) _dispatch(slot) = s
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

  def visitBegin: Int = _size
  def visitHandle(pos: Int) = _bucketAnys(handleI(pos)).asInstanceOf[Handle[_]]
  def visitSpecValue(pos: Int) = _bucketAnys(specValueI(pos))
  def visitNext(pos: Int): Int = pos - 1

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


  def push(): Unit = {
    _undoLog = new WBUndoLog(_undoLog, _undoThreshold)
    _undoThreshold = _size
  }

  def popWithCommit(): Unit = {
    _undoThreshold = _undoLog.prevThreshold
    _undoLog = _undoLog.prevLog
  }

  def popWithRollback(): Unit = {
    // apply the undo log in reverse order
    var i = _undoLog.size - 1
    while (i >= 0) {
      _bucketAnys(specValueI(_undoLog.index(i))) = _undoLog.prevValue(i)
      i -= 1
    }

    // null out and discard new bucket references
    i = _size
    while (i > _undoThreshold) {
      _bucketAnys(refI(i)) = null
      _bucketAnys(specValueI(i)) = null
      _bucketAnys(handleI(i)) = null
      i -= 1
    }
    _size = _undoThreshold

    // revert to previous context
    popWithCommit()
  }
}

private class WBUndoLog(val prevLog: WBUndoLog, val prevThreshold: Int) {
  var size = 0
  var index = new Array[Int](16)
  var prevValue = new Array[AnyRef](16)

  def record(i: Int, v: AnyRef) {
    if (size == index.length) {
      grow()
    }
    index(size) = i
    prevValue(size) = v
    size += 1
  }

  private def grow() {
    index = java.util.Arrays.copyOf(index, index.length * 2)
    prevValue = java.util.Arrays.copyOf(prevValue, prevValue.length * 2)
  }
}

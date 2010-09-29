/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// WriteBuffer2

package edu.stanford.ppl.ccstm.impl

private[impl] final class WriteBuffer {
  private def InitialCap = 8
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

  @inline private def refI(i: Int) = 3 * (i - 1)
  @inline private def specValueI(i: Int) = 3 * (i - 1) + 1
  @inline private def handleI(i: Int) = 3 * (i - 1) + 2
  @inline private def offsetI(i: Int) = 2 * (i - 1)
  @inline private def nextI(i: Int) = 2 * (i - 1) + 1 // bits 31..1 are the next, bit 0 is set iff freshOwner

  private def bucketAnysLen(c: Int) = 3 * (maxSizeForCap(c) + 1)
  private def bucketIntsLen(c: Int) = 2 * (maxSizeForCap(c) + 1)

  private def maxSizeForCap(c: Int) = c - c / 4
  private def shouldGrow(s: Int, c: Int): Boolean = s > maxSizeForCap(c)
  private def shouldGrow: Boolean = shouldGrow(_size, _cap)

  //////// accessors

  @inline private def getRef(i: Int) = _bucketAnys(refI(i))
  @inline def getHandle(i: Int) = _bucketAnys(handleI(i)).asInstanceOf[Handle[_]]
  @inline def getSpecValue[T](i: Int) = _bucketAnys(specValueI(i)).asInstanceOf[T]
  @inline private def getOffset(i: Int) = _bucketInts(offsetI(i))
  @inline private def getNext(i: Int): Int = _bucketInts(nextI(i)) >>> 1
  @inline def wasFreshOwner(i: Int): Boolean = (_bucketInts(nextI(i)) & 1) != 0

  @inline private def setRef(i: Int, r: AnyRef) { _bucketAnys(refI(i)) = r }
  @inline private def setHandle(i: Int, h: Handle[_]) { _bucketAnys(handleI(i)) = h }
  @inline private def setSpecValue[T](i: Int, v: T) { _bucketAnys(specValueI(i)) = v.asInstanceOf[AnyRef] }
  @inline private def setOffset(i: Int, o: Int) { _bucketInts(offsetI(i)) = o }
  @inline private def setNextAndFreshOwner(i: Int, n: Int, freshOwner: Boolean) { _bucketInts(nextI(i)) = (n << 1) | (if (freshOwner) 1 else 0) }
  @inline private def setNext(i: Int, n: Int) { _bucketInts(nextI(i)) = (n << 1) | (_bucketInts(nextI(i)) & 1) }

  //////// bulk access

  def isEmpty = _size == 0
  def size = _size

  //////// reads
  
  def get[T](handle: Handle[T]): T = {
    val i = find(handle)
    if (i != 0) {
      // hit
      getSpecValue[T](i)
    } else {
      // miss
      handle.data
    }
  }

  private def find(handle: Handle[_]): Int = {
    val ref = handle.ref
    val offset = handle.offset
    find(ref, offset, computeSlot(ref, offset))
  }

  private def computeSlot(ref: AnyRef, offset: Int): Int = {
    if (_cap == InitialCap) 0 else STMImpl.hash(ref, offset) & (_cap - 1)
  }

  private def find(ref: AnyRef, offset: Int, slot: Int): Int = {
    var i = _dispatch(slot)
    while (i > 0 && ((ref ne getRef(i)) || offset != getOffset(i)))
      i = getNext(i)
    i
  }

  //////// writes

  def put[T](handle: Handle[T], freshOwner: Boolean, value: T) {
    val ref = handle.ref
    val offset = handle.offset
    val slot = computeSlot(ref, offset)
    val i = find(ref, offset, slot)
    if (i != 0) {
      //assert(!freshOwner) // TODO: remove
      // hit, update an existing entry, optionally with undo
      if (i <= _undoThreshold)
        _undoLog.record(i, getSpecValue(i))
      setSpecValue(i, value)
    } else {
      // miss, create a new entry
      append(ref, offset, handle, freshOwner, value, slot)
    }
  }

  def allocatingGet[T](handle: Handle[T], freshOwner: Boolean): T = {
    return getSpecValue[T](findOrAllocate(handle, freshOwner))
  }

  def swap[T](handle: Handle[T], freshOwner: Boolean, value: T): T = {
    val i = findOrAllocate(handle, freshOwner)
    val before = getSpecValue[T](i)
    setSpecValue(i, value)
    return before
  }

  def getAndTransform[T](handle: Handle[T], freshOwner: Boolean, func: T => T): T = {
    val i = findOrAllocate(handle, freshOwner)
    val before = getSpecValue[T](i)
    setSpecValue(i, func(before))
    return before
  }

  private def findOrAllocate(handle: Handle[_], freshOwner: Boolean): Int = {
    val ref = handle.ref
    val offset = handle.offset
    val slot = computeSlot(ref, offset)
    val i = find(ref, offset, slot)
    if (i != 0) {
      //assert(!freshOwner) // TODO: remove
      // hit, undo log entry is required to capture the potential reads that
      // won't be recorded in this nested txn's read set
      if (i <= _undoThreshold)
        _undoLog.record(i, getSpecValue(i))
      return i
    } else {
      // miss, create a new entry using the existing data value
      return append(ref, offset, handle, freshOwner, handle.data, slot)
    }
  }

  private def append(ref: AnyRef, offset: Int, handle: Handle[_], freshOwner: Boolean, value: Any, slot: Int): Int = {
    val i = _size + 1
    setRef(i, ref)
    setSpecValue(i, value)
    setHandle(i, handle)
    setOffset(i, offset)
    setNextAndFreshOwner(i, _dispatch(slot), freshOwner)
    _dispatch(slot) = i
    _size = i

    if (shouldGrow)
      grow()

    // grow() relinks the buckets but doesn't move them, so s is still valid
    return i
  }

  private def grow() {
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

    // rebuild the dispatch array by iterating the slots
    var i = 1
    while (i <= _size) {
      val slot = computeSlot(getRef(i), getOffset(i))
      setNext(i, _dispatch(slot))
      _dispatch(slot) = i
      i += 1
    }
  }

  def clear() {
    if (_cap <= MaxInitialRealCap) {
      // null out the existing arrays that hold references
      var i = _size
      while (i > 0) {
        setRef(i, null)
        setSpecValue(i, null)
        setHandle(i, null)
        i -= 1
      }

      // zero the portion of the dispatch array that will be reused immediately
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

  //////// nesting management and visitation

  /** Creates and switches to a nested context. */
  def push() {
    _undoLog = new WBUndoLog(_undoLog, _undoThreshold)
    _undoThreshold = _size
  }

  /** Commits the active nested context into the parent context. */
  def popWithNestedCommit() {
    _undoThreshold = _undoLog.prevThreshold
    _undoLog = _undoLog.prevLog
  }

  /** Restores the contents of the write buffer to the state at which `push`
   *  was called, calling `unlock.apply` for each fresh-owner handle that was
   *  added since the matching push.
   */
  def popWithNestedRollback() {
    // apply the undo log in reverse order
    var i = _undoLog.size - 1
    while (i >= 0) {
      setSpecValue(_undoLog.index(i), _undoLog.prevValue(i))
      i -= 1
    }

    // null out references from the discarded buckets
    i = _size
    while (i > _undoThreshold) {
      if (wasFreshOwner(i)) {
        // unlock
        val h = getHandle(i)
        h.meta = STMImpl.withRollback(h.meta)
      }

      setRef(i, null)
      setSpecValue(i, null)
      setHandle(i, null)
      i -= 1
    }
    _size = _undoThreshold

    // revert to previous context
    popWithNestedCommit()
  }

  /** Adds to `accum` all handles that were written to in the current nesting
   *  level but not in any parent level.
   */
  def accumulateLevel(accum: ReadSetBuilder) {
    var i = _size
    while (i > _undoThreshold) {
      val h = getHandle(i)
      accum.add(h, STMImpl.version(h.meta))
      i -= 1
    }
  }
}

private class WBUndoLog(val prevLog: WBUndoLog, val prevThreshold: Int) {
  var size = 0
  var index: Array[Int] = null
  var prevValue: Array[AnyRef] = null

  def record(i: Int, v: AnyRef) {
    if (size == 0 || size == index.length) {
      grow()
    }
    index(size) = i
    prevValue(size) = v
    size += 1
  }

  private def grow() {
    if (size == 0) {
      index = new Array[Int](16)
      prevValue = new Array[AnyRef](16)
    } else {
      index = java.util.Arrays.copyOf(index, index.length * 2)
      prevValue = java.util.Arrays.copyOf(prevValue, prevValue.length * 2)
    }
  }
}

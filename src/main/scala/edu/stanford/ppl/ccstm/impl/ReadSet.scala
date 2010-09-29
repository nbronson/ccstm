/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// ReadSet

package edu.stanford.ppl.ccstm.impl


private[impl] object ReadSet {
  private def InitialCapacity = 1024
  private def MaxEmptyCapacity = 8192

  trait Visitor {
    def visit(handle: Handle[_], ver: STMImpl.Version): Boolean
  }
}

import ReadSet._

/** A read set representation. */
private[impl] final class ReadSet(private var _end: Int,
                                  private var _handles: Array[Handle[_]],
                                  private var _versions: Array[STMImpl.Version],
                                  private var _nestingDepth: Int,
                                  private var _nestingStack: Array[Int]) {
  def this() = this(0, 0, new Array[Handle[_]](InitialCapacity), new Array[Long](InitialCapacity))

  override def clone(): ReadSet = {
    val hh = new Array[Handle[_]](_end)
    System.arraycopy(_handles, 0, hh, 0, _end)
    val vv = new Array[STMImpl.Version](_end)
    System.arraycopy(_versions, 0, vv, 0, _end)
    val ss = new Array[Int](_nestingDepth)
    System.arraycopy(_nestingStack, 0, ss, 0, _nestingDepth)
    new ReadSet(_end, hh, vv, _nestingDepth, ss)
  }

  def indexEnd = _end

  def handle(i: Int): Handle[_] = _handles(i)
  def version(i: Int): STMImpl.Version = _versions(i)
  def nestingLevel(i: Int): Int = {
    // level == # of push locations <= i
    val index = java.util.Arrays.binarySearch(_nestingStack, 0, _nestingDepth, i)
    if (index >= 0) index + 1 else -(index + 1)
  }

  def add(handle: Handle[_], version: STMImpl.Version) {
    if (_end == _handles.length)
      grow()
    _handles(_end) = handle
    _versions(_end) = version
    _end += 1
  }

  private def grow() {
    val n = math.max(InitialCapacity, _end * 2)
    val hh = new Array[Handle[_]](n)
    System.arraycopy(_handles, 0, hh, 0, _end)
    _handles = hh
    val vv = new Array[STMImpl.Version](n)
    System.arraycopy(_versions, 0, vv, 0, _end)
    _versions = vv
  }

  def release(i: Int) {
    _handles(i) = null
  }

  def clear() {
    if (_handles.length > MaxEmptyCapacity) {
      _handles = new Array[Handle[_]](InitialCapacity)
      _versions = new Array[STMImpl.Version](InitialCapacity)
    } else {
      java.util.Arrays.fill(_handles.asInstanceOf[Array[AnyRef]], 0, _end, null)
    }
    _end = 0
    _nestingDepth = 0
  }

  def destroy() {
    _handles = null
    _versions = null
    _nestingStack = null
  }

  def push() {
    if (_nestingDepth == _nestingStack.length)
      _nestingStack = java.util.Arrays.copyOf(_nestingStack, math.max(8, _nestingDepth * 2))
    _nestingStack(_nestingDepth) = _end
    _nestingDepth += 1
  }

  def popWithNestedCommit() {
    _nestingDepth -= 1
  }

  def popWithNestedRollback() {
    _nestingDepth -= 1
    val e = _nestingStack(_nestingDepth)
    java.util.Arrays.fill(_handles.asInstanceOf[Array[AnyRef]], e, _end, null)
    _end = e
  }

  /** Call prior to `popWithRollback` to accumulate the entries added since
   *  the last `push`.
   */
  def accumulateLevel(accum: ReadSetBuilder) {
    val b = _nestingStack(_nestingDepth - 1)
    var i = _end - 1
    while (i >= b) {
      val h = _handles(i)
      if (h != null)
        accum.add(h, _versions(i))
      i -= 1
    }
  }

  def visitAllLevels(visitor: Visitor): Boolean = {
    var i = 0
    while (i < _end) {
      val h = _handles(i)
      if (h != null && !visitor.visit(h, _versions(i)))
        return false
      i += 1
    }
    return true
  }
}

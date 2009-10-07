/* CCSTM - (c) 2009 Stanford University - PPL */

// ReadSet

package edu.stanford.ppl.ccstm.impl


private[impl] object ReadSet {
  val InitialCapacity = 1024
  val MaxEmptyCapacity = 8192

  trait Visitor {
    def visit(handle: Handle[_], ver: STMImpl.Version): Boolean
  }
}

import ReadSet._

/** A read set representation. */
private[impl] class ReadSet private(private var _size: Int,
                                    private var _handles: Array[Handle[_]],
                                    private var _versions: Array[STMImpl.Version]) {

  def this() = this(0, new Array[Handle[_]](InitialCapacity), new Array[Long](InitialCapacity))

  override def clone(): ReadSet = {
    new ReadSet(_size,
                java.util.Arrays.copyOf(_handles, _size),
                java.util.Arrays.copyOf(_versions, _size))
  }

  def isEmpty = _size == 0
  def size = _size

  def add(handle: Handle[_], version: STMImpl.Version) {
    if (_size == _handles.length) grow()
    _handles(_size) = handle
    _versions(_size) = version
    _size += 1
  }

  private def grow() {
    _handles = java.util.Arrays.copyOf(_handles, _size * 2)
    _versions = java.util.Arrays.copyOf(_versions, _size * 2)    
  }

  def clear() {
    if (_handles.length > MaxEmptyCapacity) {
      _handles = new Array[Handle[_]](InitialCapacity)
      _versions = new Array[STMImpl.Version](InitialCapacity)
    } else {
      java.util.Arrays.fill(_handles.asInstanceOf[Array[AnyRef]], 0, _size, null)
    }
    _size = 0
  }

  def visit(visitor: ReadSet.Visitor): Boolean = {
    var i = 0
    while (i < _size) {
      if (!visitor.visit(_handles(i), _versions(i))) return false
      i += 1
    }
    return true
  }
}

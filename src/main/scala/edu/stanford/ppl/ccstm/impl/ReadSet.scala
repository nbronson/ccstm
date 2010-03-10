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
private[impl] final class ReadSet(private var _size: Int,
                                  private var _released: Int,
                                  private var _handles: Array[Handle[_]],
                                  private var _versions: Array[STMImpl.Version]) {
  def this() = this(0, 0, new Array[Handle[_]](InitialCapacity), new Array[Long](InitialCapacity))

  override def clone(): ReadSet = {
    val hh = new Array[Handle[_]](_size)
    System.arraycopy(_handles, 0, hh, 0, _size);
    val vv = new Array[STMImpl.Version](_size)
    System.arraycopy(_versions, 0, vv, 0, _size);
    new ReadSet(_size, _released, hh, vv)
  }

  def size = _size - _released
  def maxIndex = _size

  def handle(i: Int): Handle[_] = _handles(i)
  def version(i: Int): STMImpl.Version = _versions(i)

  def add(handle: Handle[_], version: STMImpl.Version) {
    if (_size == _handles.length) grow()
    _handles(_size) = handle
    _versions(_size) = version
    _size += 1
  }

  private def grow() {
    val hh = new Array[Handle[_]](_size * 2)
    System.arraycopy(_handles, 0, hh, 0, _size);
    _handles = hh
    val vv = new Array[STMImpl.Version](_size * 2)
    System.arraycopy(_versions, 0, vv, 0, _size);
    _versions = vv
  }

  def release(i: Int) {
    if (null != _handles(i)) {
      _handles(i) = null
      _released += 1
    }
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

  def destroy() {
    _handles = null
    _versions = null
  }

  def visit(visitor: Visitor): Boolean = {
    var i = 0
    while (i < _size) {
      val h = _handles(i)
      if (null != h && !visitor.visit(h, _versions(i))) return false
      i += 1
    }
    return true
  }
}

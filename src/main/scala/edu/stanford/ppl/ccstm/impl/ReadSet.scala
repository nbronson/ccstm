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
    val hh = new Array[Handle[_]](_size)
    System.arraycopy(_handles, 0, hh, 0, _size);
    val vv = new Array[STMImpl.Version](_size)
    System.arraycopy(_versions, 0, vv, 0, _size);
    new ReadSet(_size, hh, vv)
  }

  def isEmpty = _size == 0
  def size = _size

  def add(handle: Handle[_], version: STMImpl.Version) {
    if (_size == _handles.length) grow()
    _handles(_size) = handle
    _versions(_size) = version
    _size += 1
  }

  def lastHandle: Handle[_] = _handles(_size - 1)
  def lastVersion: STMImpl.Version = _versions(_size - 1)

  private def grow() {
    val n = _size * 2
    val hh = new Array[Handle[_]](n)
    System.arraycopy(_handles, 0, hh, 0, n);
    _handles = hh
    val vv = new Array[STMImpl.Version](n)
    System.arraycopy(_versions, 0, vv, 0, n);
    _versions = vv
  }

  /** Removes at most one entry whose ref and offset are the same as those of
   *  <code>handle</code> and with the matching <code>version</code>.
   *  Returns true if something was removed.
   */  
  def remove(handle: Handle[_], version: STMImpl.Version): Boolean = {
    val ref = handle.ref
    val offset = handle.offset
    var i = _size - 1
    while (i >= 0) {
      val h = _handles(i)
      if (_versions(i) == version && ((h eq handle) || ((h.ref eq ref) && h.offset == offset))) {
        // copy down from the end
        _size -= 1
        _handles(i) = _handles(_size)
        _handles(_size) = null
        _versions(i) = _versions(_size)
        return true
      }
      i -= 1
    }
    return false
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

  def visit(visitor: ReadSet.Visitor): Boolean = {
    var i = 0
    while (i < _size) {
      if (!visitor.visit(_handles(i), _versions(i))) return false
      i += 1
    }
    return true
  }
}

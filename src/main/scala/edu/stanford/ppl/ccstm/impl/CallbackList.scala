/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// CallbackList

package edu.stanford.ppl.ccstm.impl


/** This data structure is used to implement callback lists with optional
 *  priorities.  It supports insertion during traversal, with a guarantee that
 *  elements inserted during traversal will be visited by that traversal.
 *  Recursive traversal is not supported.
 *
 *  @author Nathan Bronson
 */
private[ccstm] final class CallbackList[T <: AnyRef] {
  private val _zeroSlot = new CallbackPrioSlot
  private val _slotsByPrio = new java.util.TreeMap[Int,CallbackPrioSlot]
  private var _size = 0

  def size = _size + _zeroSlot.size
  def isEmpty = size == 0

  def add(elem: T, priority: Int) {
    add(elem, priority, true, true)
  }

  def add(elem: T, priority: Int, callOnCommit: Boolean, callOnRollback: Boolean) {
    if (null == elem) {
      throw new NullPointerException
    }
    val m = (if (callOnCommit) 1 else 0) + (if (callOnRollback) 2 else 0)
    if (priority == 0) {
      _zeroSlot.add(elem, m)
    } else {
      var slot = _slotsByPrio.get(priority)
      if (null == slot) {
        slot = new CallbackPrioSlot
        _slotsByPrio.put(priority, slot)
      }
      slot.add(elem, m)
      _size += 1
    }
  }

  def foreach(block: T => Unit) { foreach(3, block) }

  def foreach(committing: Boolean)(block: T => Unit) { foreach((if (committing) 1 else 2), block) }

  private def foreach(mask: Int, block: T => Unit) {
    try {
      while (!attemptForeach(mask, block)) {}
    } finally {
      reset
    }
  }

  def clear() {
    _zeroSlot.clear()
    _slotsByPrio.clear()
    _size = 0
  }

  private def attemptForeach(mask: Int, block: T => Unit): Boolean = {
    val expectedSize = size
    if (_size == 0) {
      if (!attemptSlot(mask, block, expectedSize, _zeroSlot)) return false
    } else {
      _slotsByPrio.put(0, _zeroSlot)
      val iter = _slotsByPrio.values().iterator
      while (iter.hasNext) {
        if (!attemptSlot(mask, block, expectedSize, iter.next)) return false
      }
    }
    return true
  }

  private def attemptSlot(mask: Int, block: T => Unit, expectedSize: Int, slot: CallbackPrioSlot): Boolean = {
    while (slot.visitOne(mask, block)) {
      if (expectedSize != size) return false
    }
    return true
  }

  private def reset() {
    _zeroSlot.reset()
    if (_size > 0) {
      val iter = _slotsByPrio.values().iterator
      while (iter.hasNext) iter.next.reset()
    }
  }
}

private final class CallbackPrioSlot {
  private def InitialCap = 8
  private def MaxInitialCap = 512

  private var _count = 0
  private var _elems = new Array[AnyRef](InitialCap * 2)
  private var _visited = 0

  def size = _count

  def reset() { _visited = 0 }

  def add(elem: AnyRef, mask: Int) {
    if (_count * 2 + 2 > _elems.length) {
      _elems = java.util.Arrays.copyOf(_elems, _count * 4)
    }
    _elems(_count * 2) = elem
    _elems(_count * 2 + 1) = mask.asInstanceOf[AnyRef]
    _count += 1
  }

  def clear() {
    if (_elems.length > MaxInitialCap * 2) {
      // complete reset
      _elems = new Array[AnyRef](MaxInitialCap * 2)
    } else {
      java.util.Arrays.fill(_elems, 0, _count * 2, null)
    }
    _count = 0
  }

  def visitOne[T](mask: Int, block: T => Unit): Boolean = {
    val v = _visited
    if (v < _count) {
      val m = _elems(v * 2 + 1).asInstanceOf[Int]
      if ((m & mask) != 0) {
        block(_elems(v * 2).asInstanceOf[T])
      }
      _visited = v + 1
      true
    } else {
      false
    }
  }
}

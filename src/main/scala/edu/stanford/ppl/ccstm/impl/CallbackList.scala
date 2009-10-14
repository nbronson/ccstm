/* CCSTM - (c) 2009 Stanford University - PPL */

// CallbackList

package edu.stanford.ppl.ccstm.impl


/** This data structure is used to implement callback lists with optional
 *  priorities.  It supports insertion during traversal, with a guarantee that
 *  elements inserted during traversal will be visited by that traversal.
 *  Recursive traversal is not supported.
 *
 *  @author Nathan Bronson
 */
private[ccstm] class CallbackList[T <: AnyRef] {
  private val _slotsByPrio = new java.util.TreeMap[Int,CallbackPrioSlot]
  private var _size = 0

  def size = _size
  def isEmpty = _size == 0

  def add(elem: T, priority: Int) {
    var slot = _slotsByPrio.get(priority)
    if (null == slot) {
      slot = new CallbackPrioSlot
      _slotsByPrio.put(priority, slot)
    }
    slot.add(elem)
    _size += 1
  }

  def foreach(block: T => Unit) {
    try {
      while (!attemptForeach(block)) {}
    } finally {
      reset
    }
  }

  def clear() {
    _slotsByPrio.clear()
    _size = 0
  }

  private def attemptForeach(block: T => Unit): Boolean = {
    val expectedSize = _size
    val iter = _slotsByPrio.values.iterator
    while (iter.hasNext) {
      val slot = iter.next
      while (slot._visited < slot._count) {
        block(slot._elems(slot._visited).asInstanceOf[T])
        slot._visited += 1
        if (expectedSize != _size) return false
      }
    }
    return true
  }

  private def reset {
    val iter = _slotsByPrio.values.iterator
    while (iter.hasNext) iter.next._visited = 0
  }
}

private class CallbackPrioSlot {
  var _count = 0
  // TODO: make _elems an Array[T] without getting a BoxedAnyArray, maybe in 2.8?
  var _elems = new Array[AnyRef](4)
  var _visited = 0

  def add(elem: AnyRef) {
    if (_count == _elems.length) {
      _elems = java.util.Arrays.copyOf(_elems, _count * 2)
    }
    _elems(_count) = elem
    _count += 1
  }
}
/* CCSTM - (c) 2009 Stanford University - PPL */

// CallbackList

package edu.stanford.ppl.ccstm


/** This data structure is used to implement callback lists with optional
 *  priorities.  It supports insertion during traversal, with a guarantee that
 *  elements inserted during traversal will be visited by that traversal.
 *  Recursive traversal is not supported.
 */
private[ccstm] class CallbackList[T] {
  private val _slotsByPrio = new java.util.TreeMap[Int,CallbackPrioSlot[T]]
  private var _size = 0

  def add(elem: T, priority: Int) {
    var slot = _slotsByPrio.get(priority)
    if (slot == null) {
      slot = new CallbackPrioSlot[T]
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

  private def attemptForeach(block: T => Unit): Boolean = {
    val expectedSize = _size
    val iter = _slotsByPrio.values.iterator
    while (iter.hasNext) {
      val slot = iter.next
      while (slot._visited < slot._count) {
        block(slot._elems(slot._visited))
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

private class CallbackPrioSlot[T] {
  var _count = 0
  var _elems = new Array[T](4)
  var _visited = 0

  def add(elem: T) {
    if (_count == _elems.length) {
      val n = new Array[T](_count * 2)
      Array.copy(_elems, 0, n, 0, _count)
      _elems = n
    }
    _elems(_count) = elem
    _count += 1
  }
}
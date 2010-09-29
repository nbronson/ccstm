/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// CallbackList

package edu.stanford.ppl.ccstm.impl

import collection.mutable.ArrayBuffer
import java.util.Comparator


private case class CallbackWithPriority[A <: AnyRef](callback: A, priority: Int)

/** This data structure is used to implement callback lists with optional
 *  priorities.  It provides extra features to support nested transactions.
 *  There are two usage models: success handlers and failure handlers.
 *
 *  Validation handlers or those invoked due to pending or recent success
 *  (before-commit and after-commit) are never partially invoked, may be
 *  invoked more than once (read-resource), and INCLUDE handlers added during
 *  the visitation in the current visitation.  Handlers are visited in
 *  increasing priority order, then FIFO order.
 *
 *  Rollback handlers are invoked at most once, and untriggered handlers must
 *  be retained if a visitation throws an exception.  They also may be
 *  partially visited (using a prevSize threshold).  Handlers that are added
 *  reentrantly are NOT INCLUDED in the current visitation.  Handlers are
 *  visited in increasing priority, then LIFO order.
 *
 *  @author Nathan Bronson
 */
private[ccstm] final class CallbackList[A <: AnyRef] {

  private var _callbacks = new ArrayBuffer[AnyRef] {
    def contents: Array[AnyRef] = array
  }
  private var _nonZeroPrioritySeen = false

  def add(callback: A, priority: Int) {
    if (priority == 0)
      _callbacks += callback
    else {
      _callbacks += CallbackWithPriority(callback, priority)
      _nonZeroPrioritySeen = true
    }
  }

  private def prio(o: AnyRef): Int = o match {
    case x: CallbackWithPriority[_] => x.priority
    case _ => 0
  }

  private def callback(o: AnyRef): A = (o match {
    case x: CallbackWithPriority[_] => x.callback
    case _ => o
  }).asInstanceOf[A]

  private def ascendingStableSort() {
    if (_nonZeroPrioritySeen) {
      java.util.Arrays.sort(_callbacks.contents, 0, size, new Comparator[AnyRef] {
        def compare(o1: AnyRef, o2: AnyRef) = Ordering.Int.compare(prio(o1), prio(o2))
      })
    }
  }

  private def descendingStableSort(begin: Int, end: Int) {
    if (_nonZeroPrioritySeen) {
      java.util.Arrays.sort(_callbacks.contents, begin, end, new Comparator[AnyRef] {
        def compare(o1: AnyRef, o2: AnyRef) = -Ordering.Int.compare(prio(o1), prio(o2))
      })
    }
  }

  def size = _callbacks.size
  def isEmpty = size == 0

  def clear() { trim(0) }

  def trim(prevSize: Int) { _callbacks.trimEnd(size - prevSize) }

  def foreach(block: A => Unit) {
    ascendingStableSort()
    var ii = 0
    while (ii < size) {
      block(callback(_callbacks(ii)))
      ii += 1
    }
  }

  /** On failure (exception) unvisited entries are retained. */
  def reverseVisitAndTrim(prevSize: Int)(block: A => Unit) {
    val sizeOnEntry = size
    descendingStableSort(prevSize, sizeOnEntry)
    var ii = sizeOnEntry - 1
    try {
      while (ii >= prevSize) {
        val cc = callback(_callbacks(ii))
        _callbacks(ii) = null
        ii -= 1

        block(cc)
      }
    } finally {
      // ii is the largest index that has not been visited, remove [ii+1,sizeOnEntry)
      _callbacks.remove(ii, sizeOnEntry - (ii + 1))
    }
  }
}

/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// Callbacks

package edu.stanford.ppl.ccstm.impl

import edu.stanford.ppl.ccstm.Txn


private[ccstm] final class Callbacks {
  val readResources = new CallbackList[Txn.ReadResource]
  val beforeCommit = new CallbackList[Txn => Unit]
  val writeResources = new CallbackList[Txn.WriteResource]
  val afterCommit = new CallbackList[Txn => Unit]
  val afterRollback = new CallbackList[Txn => Unit]

  private var _stack = new Array[Int](16 * 5)
  private var _depth = 0

  def push() {
    val d = _depth
    _depth = d + 5
    if (d + 5 > _stack.length)
      _stack = java.util.Arrays.copyOf(_stack, _stack.length * 2)
    _stack(d + 0) = readResources.size
    _stack(d + 1) = beforeCommit.size
    _stack(d + 2) = writeResources.size
    _stack(d + 3) = afterCommit.size
    _stack(d + 4) = afterRollback.size
  }

  def popWithNestedCommit() {
    _depth -= 5
  }

  def popWithNestedRollback(txn: Txn) {
    val d = _depth - 5
    _depth = d
    readResources.trim(_stack(d + 0))
    beforeCommit.trim(_stack(d + 1))
    afterCommit.trim(_stack(d + 3))

    writeResources.reverseVisitAndTrim(_stack(d + 2)) { wr =>
      try {
        wr.performRollback(txn)
      } catch {
        case x => Txn.handlePostDecisionException(txn, wr, x)
      }
    }
    afterRollback.reverseVisitAndTrim(_stack(d + 4)) { cb =>
      try {
        cb(txn)
      } catch {
        case x => Txn.handlePostDecisionException(txn, cb, x)
      }
    }
  }

  def clear() {
    readResources.clear()
    beforeCommit.clear()
    writeResources.clear()
    afterCommit.clear()
    afterRollback.clear()
  }
}

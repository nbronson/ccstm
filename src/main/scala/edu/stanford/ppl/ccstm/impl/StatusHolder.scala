/* CCSTM - (c) 2009 Stanford University - PPL */

// StatusHolder.scala

package edu.stanford.ppl.ccstm.impl


import java.util.concurrent.atomic.AtomicReferenceFieldUpdater

private[ccstm] object StatusHolder {
  val statusUpdater = (new StatusHolder).newStatusUpdater
}

private[ccstm] class StatusHolder {
  import STMImpl.{SpinCount, YieldCount}

  @volatile private var __status: Txn.Status = Txn.Active
  @volatile private var _anyAwaitingDecided = false
  @volatile private var _anyAwaitingCompletedOrDoomed = false

  private[ccstm] def newStatusUpdater = {
    // we have to create the updater from a method of StatusHolder, because
    // all Scala variables are private under the covers
    AtomicReferenceFieldUpdater.newUpdater(classOf[StatusHolder], classOf[Txn.Status], "__status")
  }

  //////////////// Access to the status, as _status

  private[ccstm] def _status = __status
  private[ccstm] def _status_=(after: Txn.Status) {
    val before = __status
    __status = after
    statusWakeups(before, after)
  }

  private[ccstm] def _statusCAS(before: Txn.Status, after: Txn.Status): Boolean = {
    if (StatusHolder.statusUpdater.compareAndSet(this, before, after)) {
      statusWakeups(before, after)
      true
    } else {
      false
    }
  }

  private def statusWakeups(before: Txn.Status, after: Txn.Status) {
    if ((_anyAwaitingDecided && after.decided && !before.decided) ||
        (_anyAwaitingCompletedOrDoomed && (after.completed || after.mustRollBack) && !(before.completed || before.mustRollBack))) {
      this.synchronized {
        this.notifyAll
      }
    }
  }

  //////////////// Assertions about the current status

  /** Throws <code>RollbackError</code> is the status is
   *  <code>RollingBack</code>, otherwise throws an
   *  <code>IllegalStateException</code> if the status is something other than
   *  <code>Active</code>.
   */
  private[ccstm] def requireActive() {
    val s = _status
    if (s ne Txn.Active) slowRequireActive(s)
  }

  private def slowRequireActive(s: Txn.Status) {
    if (s.isInstanceOf[Txn.RollingBack]) {
      throw RollbackError
    } else {
      throw new IllegalStateException("txn.status is " + s)
    }
  }

  /** Throws <code>RollbackError</code> is the status is 
   *  <code>RollingBack</code>, throws an <code>IllegalStateException</code> if
   *  the status is <code>Committed</code> or <code>RolledBack</code>.
   */
  private[ccstm] def requireNotCompleted() {
    val s = _status
    if (s != Txn.Active) {
      if (s.isInstanceOf[Txn.RollingBack]) {
        throw RollbackError
      } else if (s.completed) {
        throw new IllegalStateException("txn.status is " + s)
      }
    }
  }

  //////////////// Status change waiting

  private[ccstm] def awaitDecided() {
    // spin a bit
    var spins = 0
    while (spins < SpinCount + YieldCount) {
      spins += 1
      if (spins > SpinCount) Thread.`yield`

      if (_status.decided) return
    }

    // spin failed, put ourself to sleep
    _anyAwaitingDecided = true
    this.synchronized {
      while (!_status.decided) {
        this.wait
      }
    }
  }

  private[ccstm] def awaitCompletedOrDoomed() {
    // spin a bit
    var spins = 0
    while (spins < SpinCount + YieldCount) {
      spins += 1
      if (spins > SpinCount) Thread.`yield`

      if (completedOrDoomed) return
    }

    // spin failed, put ourself to sleep
    _anyAwaitingCompletedOrDoomed = true
    this.synchronized {
      while (!completedOrDoomed) {
        this.wait
      }
    }
  }

  private[impl] def completedOrDoomed = {
    val s = _status
    s.completed || s.mustRollBack
  }
}

/* CCSTM - (c) 2009 Stanford University - PPL */

// StatusHolder.scala

package edu.stanford.ppl.ccstm


import java.util.concurrent.atomic.AtomicReferenceFieldUpdater

private[ccstm] object StatusHolder {
  val statusUpdater = (new StatusHolder).newStatusUpdater
}

private[ccstm] class StatusHolder {

  @volatile private var __status: Txn.Status = Txn.Active
  @volatile private var _anyAwaitingDecided = false
  @volatile private var _anyAwaitingCompletedOrDoomed = false

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

  private[ccstm] def awaitDecided() {
    // spin a bit
    var spins = 0
    while (spins < 200) {
      spins += 1
      if (spins > 100) Thread.`yield`

      if (_status.decided) return
    }

    // spin failed, put ourself to sleep
    _anyAwaitingDecided = true
    this.synchronized {
      while (!_status.decided) { this.wait }
    }
  }

  private[ccstm] def awaitCompletedOrDoomed() {
    // spin a bit
    var spins = 0
    while (spins < 200) {
      spins += 1
      if (spins > 100) Thread.`yield`

      if (completedOrDoomed) return
    }

    // spin failed, put ourself to sleep
    _anyAwaitingCompletedOrDoomed = true
    this.synchronized {
      while (!completedOrDoomed) { this.wait }
    }
  }

  private[ccstm] def completedOrDoomed = {
    val s = _status
    s.completed || s.mustRollBack
  }

  private[ccstm] def newStatusUpdater = {
    // we have to create the updater from a method of StatusHolder, because
    // all Scala variables are private under the covers
    AtomicReferenceFieldUpdater.newUpdater(classOf[StatusHolder], classOf[Txn.Status], "__status")
  }
}

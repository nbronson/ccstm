/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// StatusHolder.scala

package edu.stanford.ppl.ccstm.impl


import edu.stanford.ppl.ccstm._
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater

private[ccstm] object StatusHolder {
  val statusUpdater = (new StatusHolder).newStatusUpdater
}

private[ccstm] class StatusHolder {
  import STMImpl.{SpinCount, YieldCount}

  @volatile private var __status: Txn.Status = Txn.Active(0, 0)
  @volatile private var _anyAwaitingDecided = false
  @volatile private var _anyAwaitingCompletedOrDoomed = false

  private[ccstm] def newStatusUpdater = {
    // we have to create the updater from a method of StatusHolder, because
    // all Scala variables are private under the covers
    AtomicReferenceFieldUpdater.newUpdater(classOf[StatusHolder], classOf[Txn.Status], "__status")
  }

  //////////////// Access to the status, as _status

  private[ccstm] final def _status = __status
  private[ccstm] final def _status_=(after: Txn.Status) {
    val before = __status
    __status = after
    statusWakeups(before, after)
  }

  private[ccstm] final def _statusCAS(before: Txn.Status, after: Txn.Status): Boolean = {
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

  /** Throws `RollbackError` if the current atomic block should be rolled back,
   *  or throws `IllegalStateException` if the transaction state does not allow
   *  a `Ref` access.
   */
  private[ccstm] final def checkAccess() {
    val s = _status
    if (!s.mayAccess)
      doThrow(s)
  }

  private def doThrow(s: Txn.Status) = s match {
    case _: Txn.Active => throw RollbackError // must be partial rollback
    case _: Txn.RollingBack => throw RollbackError
    case _ => throw new IllegalStateException(s.toString)
  }

  /** Throws `RollbackError` if the current atomic block should be rolled back,
   *  or throws `IllegalArgumentException` if the transaction state does not
   *  allow an unrecorded read.  This is like `checkAccess`, but it also
   *  accepts the `Preparing` state.
   */
  private[ccstm] final def checkUnrecordedRead() {
    checkAccessOrPreparing()
  }

  private def checkAccessOrPreparing() {
    val s = _status
    if (!s.mayAccess && (s ne Txn.Preparing)) doThrow(s)
  }

  /** Checks if a before-commit handler may be registered from the current
   *  context.  This is equivalent to `checkAccess`.
   */
  private[ccstm] final def checkAddBeforeCommit() { checkAccess() }

  /** Checks if a read-resource may be registered from the current context.
   *  This is equivalent to `checkAccess`.
   */
  private[ccstm] final def checkAddReadResource() { checkAccess() }

  /** Checks if a write-resource may be registered from the current context.
   *  Write-resources may be added any time that accesses are allowed, or
   *  during the preparing phase.  New write resources during the preparing
   *  phase are allowed because the visitation explicitly _includes_ handlers
   *  added reentrantly.
   */
  private[ccstm] final def checkAddWriteResource() { checkAccessOrPreparing() }

  /** Checks if an after-commit, after-rollback, or after-completion handler
   *  may be attached to the current context.  This is allowed so long as the
   *  after-completion handlers have not yet started firing.  Throws
   *  `RollbackError` if one is pending.
   */
  private[ccstm] final def checkAddAfterCompletion() { checkNotCompleted() }

  /** Throws `RollbackError` is the status is `RollingBack`, throws an
   *  `IllegalStateException` if the status is `Committed` or `RolledBack`.
   */
  private def checkNotCompleted() {
    val s = _status
    if (s.isInstanceOf[Txn.RollingBack])
      throw RollbackError
    else if (s.completed)
      throw new IllegalStateException(s.toString)
  }

  //////////////// Status change waiting

  private[ccstm] final def awaitCompletedOrDoomed() {
    // spin a bit
    var spins = 0
    while (spins < SpinCount + YieldCount) {
      spins += 1
      if (spins > SpinCount) Thread.`yield`

      if (completedOrDoomed)
        return
    }

    // spin failed, put ourself to sleep
    _anyAwaitingCompletedOrDoomed = true
    this.synchronized {
      while (!completedOrDoomed) {
        this.wait
      }
    }
  }

  private[impl] final def completedOrDoomed: Boolean = {
    val s = _status
    s.completed || s.mustRollBack
  }
}

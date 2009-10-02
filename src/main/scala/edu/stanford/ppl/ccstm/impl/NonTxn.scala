/* CCSTM - (c) 2009 Stanford University - PPL */

// NonTxn

package edu.stanford.ppl.ccstm.impl


/** The object that contains the code for non-transactional read and write
 *  barriers.
 */
private[ccstm] object NonTxn {
  import STMImpl._

  //////////////// lock waiting

  private def weakAwaitUnowned(handle: Handle[_], m0: Meta) {
    // spin a bit
    var spins = 0
    while (spins < SpinCount + YieldCount) {
      spins += 1
      if (spins > SpinCount) Thread.`yield`

      val m = handle.meta
      if (ownerAndVersion(m) != ownerAndVersion(m0)) return
    }

    owner(m0) match {
      case NonTxnSlot => weakNoSpinAwaitNonTxnUnowned(handle, m0)
      case FrozenSlot => throw new IllegalStateException("frozen")
      case _ => weakNoSpinAwaitTxnUnowned(handle, m0)
    }
  }

  private def weakNoSpinAwaitNonTxnUnowned(handle: Handle[_], m0: Meta) {
    // to wait for a non-txn owner, we use pendingWakeups
    val event = wakeupManager.subscribe
    event.addSource(handle.ref, handle.offset)
    do {
      val m = handle.meta
      if (ownerAndVersion(m) != ownerAndVersion(m0)) {
        // observed unowned
        return
      }

      if (pendingWakeups(m) || handle.metaCAS(m, withPendingWakeups(m))) {
        // after the block, things will have changed with reasonably high
        // likelihood (spurious wakeups are okay)
        event.await
        return
      }
    } while (!event.triggered)
  }

  private def weakNoSpinAwaitTxnUnowned(handle: Handle[_], m0: Meta) {
    // to wait for a txn owner, we track down the Txn and wait on it
    val owningSlot = owner(m0)
    val owningTxn = slotManager.lookup(owningSlot)
    if (owningSlot == owner(handle.meta)) {
      // The slot numbers are the same, which means that either owningTxn is
      // the current owner or it has completed and its slot has been reused.
      // Either way it is okay to wait for it.
      owningTxn.awaitCompletedOrDoomed

      if (ownerAndVersion(handle.meta) == ownerAndVersion(m0)) {
        assert(owningTxn.status.mustRollBack)
        stealHandle(handle, m0, owningTxn)
      }
    }
    // else invalid read of owningTxn, which means there was an ownership change
  }

  //////////////// value waiting

  private def weakAwaitNewVersion(handle: Handle[_], m0: Meta) {
    // spin a bit
    var m = 0L
    var spins = 0
    do {
      val m = handle.meta
      if (version(m) != version(m0)) return

      spins += 1
      if (spins > SpinCount) Thread.`yield`
    } while (spins < SpinCount + YieldCount)

    if (changing(m)) {
      weakAwaitUnowned(handle, m)
    } else {
      weakNoSpinAwaitNewVersion(handle, m)
    }
  }

  private def weakNoSpinAwaitNewVersion(handle: Handle[_], m0: Meta) {
    if (owner(m0) == FrozenSlot) throw new IllegalStateException("futile wait on frozen data")

    val event = wakeupManager.subscribe
    event.addSource(handle.ref, handle.offset)
    do {
      val m = handle.meta
      if (version(m) != version(m0) || changing(m)) {
        // observed new version, or likely new version (after unlock)
        return
      }

      if (pendingWakeups(m) || handle.metaCAS(m, withPendingWakeups(m))) {
        // after the block, things will have changed with reasonably high
        // likelihood (spurious wakeups are okay)
        event.await
        return
      }
    } while (!event.triggered)
  }

  //////////////// lock acquisition

  private def acquireLock(handle: Handle[_], exclusive: Boolean): Meta = {
    while (true) {
      val m0 = handle.meta
      if (owner(m0) != UnownedSlot) {
        weakAwaitUnowned(handle, m0)
      } else {
        val mOwned = withOwner(m0, NonTxnSlot)
        val m1 = if (exclusive) withChanging(mOwned) else mOwned
        if (handle.metaCAS(m0, m1)) {
          // lock acquired
          return m1
        }
      }
    }
    throw new Error
  }

  private def upgradeLock(handle: Handle[_], m0: Meta): Meta = {
    var before = m0
    if (!handle.metaCAS(before, withChanging(before))) {
      // must have been a concurrent set of pendingWakeups
      before = withPendingWakeups(before)
      if (!handle.metaCAS(before, withChanging(before))) {
        // should never happen
        throw new Error
      }
    }
    withChanging(before)
  }

  private def commitLock(handle: Handle[_], m0: Meta) {
    releaseLock(handle, m0, STMImpl.nonTxnWriteVersion(version(m0)))
  }

  private def discardLock(handle: Handle[_], m0: Meta) {
    releaseLock(handle, m0, version(m0))
  }

  private def releaseLock(handle: Handle[_], m0: Meta, newVersion: Version) {
    var before = m0
    if (!handle.metaCAS(before, withCommit(before, newVersion))) {
      // must have been a concurrent set of pendingWakeups
      before = withPendingWakeups(before)
      if (!handle.metaCAS(before, withCommit(before, newVersion))) {
        // should never happen
        throw new Error
      }
    }

    // trigger any required wakeups
    if (pendingWakeups(before)) {
      wakeupManager.trigger(wakeupManager.prepareToTrigger(handle.ref, handle.offset))
    }
  }

  //////////////// public interface

  def get[T](handle: Handle[T]): T = {
    while (true) {
      val m0 = handle.meta
      if (changing(m0)) {
        weakAwaitUnowned(handle, m0)
      } else {
        val v = handle.data
        val m1 = handle.meta
        if (changingAndVersion(m0) == changingAndVersion(m1)) {
          // stable read of v
          return v
        }
      }
    }
    throw new Error
  }

  def await[T](handle: Handle[T], pred: T => Boolean) {
    while (true) {
      val m0 = handle.meta
      if (changing(m0)) {
        weakAwaitUnowned(handle, m0)
      } else {
        val v = handle.data
        val m1 = handle.meta
        if (changingAndVersion(m0) == changingAndVersion(m1)) {
          // stable read of v
          if (pred(v)) {
            // success!
            return
          }

          // wait for a new version
          weakAwaitNewVersion(handle, m1)
        }
      }
    }
  }

  def unrecordedRead[T](handle: Handle[T]): UnrecordedRead[T] = {
    while (true) {
      val m0 = handle.meta
      if (changing(m0)) {
        weakAwaitUnowned(handle, m0)
      } else {
        val v = handle.data
        val m1 = handle.meta
        if (changingAndVersion(m0) == changingAndVersion(m1)) {
          // stable read of v
          return new UnrecordedRead[T] {
            def context = None
            val value = v
            def stillValid = changingAndVersion(handle.meta) == changingAndVersion(m1)
            def recorded = false
          }
        }
      }
    }
    throw new Error
  }

  def set[T](handle: Handle[T], v: T) {
    val m0 = acquireLock(handle, true)
    handle.data = v
    commitLock(handle, m0)
  }

  def tryWrite[T](handle: Handle[T], v: T): Boolean = {
    // try to acquire
    val m0 = handle.meta
    if (owner(m0) != UnownedSlot) {
      return false
    }
    val m1 = withChanging(withOwner(m0, NonTxnSlot))
    if (!handle.metaCAS(m0, m1)) {
      return false
    }

    // acquired, we can set
    handle.data = v
    commitLock(handle, m1)
    return true
  }

  def freeze[T](handle: Handle[T]) {
    while (true) {
      val m0 = handle.meta
      owner(m0) match {
        case FrozenSlot => {
          // success
          return
        }
        case UnownedSlot => {
          // attempt freeze
          val m1 = withOwner(m0, FrozenSlot)
          if (handle.metaCAS(m0, m1)) {
            // success
            return
          }
        }
        case _ => {
          weakAwaitUnowned(handle, m0)
          // try again
        }
      }
    }
  }

  def compareAndSet[T](handle: Handle[T], before: T, after: T): Boolean = {
    if (before == get(handle)) {
      val m0 = acquireLock(handle, true)
      if (before == handle.data) {
        handle.data = after
        commitLock(handle, m0)
        true
      } else {
        discardLock(handle, m0)
        false
      }
    } else {
      // invisible failure
      false
    }
  }

  def compareAndSetIdentity[T,R <: AnyRef with T](handle: Handle[T], before: R, after: T): Boolean = {
    if (before eq get(handle).asInstanceOf[AnyRef]) {
      val m0 = acquireLock(handle, true)
      if (before eq handle.data.asInstanceOf[AnyRef]) {
        handle.data = after
        commitLock(handle, m0)
        true
      } else {
        discardLock(handle, m0)
        false
      }
    } else {
      // invisible failure
      false
    }
  }

  def weakCompareAndSet[T](handle: Handle[T], before: T, after: T): Boolean = {
    compareAndSet(handle, before, after)
  }

  def weakCompareAndSetIdentity[T,R <: AnyRef with T](handle: Handle[T], before: R, after: T): Boolean = {
    weakCompareAndSet(handle, before, after)
  }

  def transform[T](handle: Handle[T], f: T => T) {
    val m0 = acquireLock(handle, false)
    val repl = f(handle.data)
    val m1 = upgradeLock(handle, m0)
    handle.data = repl
    commitLock(handle, m1)
  }

  def transformIfDefined[T](handle: Handle[T], pf: PartialFunction[T,T]): Boolean = {
    if (pf.isDefinedAt(get(handle))) {
      val m0 = acquireLock(handle, false)
      val v = handle.data
      if (pf.isDefinedAt(v)) {
        val repl = pf(v)
        val m1 = upgradeLock(handle, m0)
        handle.data = repl
        commitLock(handle, m1)
        true
      } else {
        discardLock(handle, m0)
        false
      }
    } else {
      // invisible failure
      false
    }
  }
}
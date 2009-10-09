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
    STMImpl.weakAwaitUnowned(handle, m0, null)
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
    var m0 = 0L
    var m1 = 0L
    do {
      m0 = handle.meta
      while (owner(m0) != UnownedSlot) {
        weakAwaitUnowned(handle, m0)
        m0 = handle.meta
      }
      val mOwned = withOwner(m0, NonTxnSlot)
      m1 = if (exclusive) withChanging(mOwned) else mOwned
    } while (!handle.metaCAS(m0, m1))
    m1
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
    var m = m0
    while (!handle.metaCAS(m, withCommit(m, newVersion))) {
      // There can be a concurrent pendingWakeups <- true, and there can also
      // be a twiddle of the userBit by this thread between when m0 was sampled
      // and now.
      m = handle.meta
    }

    // trigger any required wakeups
    if (pendingWakeups(m)) {
      wakeupManager.trigger(wakeupManager.prepareToTrigger(handle.ref, handle.offset))
    }
  }

  //////////////// public interface

  def get[T](handle: Handle[T]): T = {
    var m0 = 0L
    var m1 = 0L
    var v: T = null.asInstanceOf[T]
    do {
      m0 = handle.meta
      while (changing(m0)) {
        weakAwaitUnowned(handle, m0)
        m0 = handle.meta
      }
      v = handle.data
      m1 = handle.meta
    } while (changingAndVersion(m0) != changingAndVersion(m1))
    v
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
    // try to acquire ownership
    val m0 = handle.meta
    if (owner(m0) != UnownedSlot) {
      return invisibleCAS(handle, before, after)
    }
    val m1 = withOwner(m0, NonTxnSlot)
    if (!handle.metaCAS(m0, m1)) {
      return invisibleCAS(handle, before, after)
    }

    if (before == handle.data) {
      val m2 = upgradeLock(handle, m1)
      handle.data = after
      commitLock(handle, m2)
      true
    } else {
      discardLock(handle, m1)
      false
    }
  }

  private def invisibleCAS[T](handle: Handle[T], before: T, after: T): Boolean = {
    // this is the code from get, inlined so that we have access to the version
    // number as well with no boxing
    var m0 = 0L
    var m1 = 0L
    var v: T = null.asInstanceOf[T]
    do {
      m0 = handle.meta
      while (changing(m0)) {
        weakAwaitUnowned(handle, m0)
        m0 = handle.meta
      }
      v = handle.data
      m1 = handle.meta
    } while (changingAndVersion(m0) != changingAndVersion(m1))

    // invisible failure?
    if (!(before == v)) return false

    // don't go directly to changing, because we can't run user code
    // (before.equals) there
    val m2 = acquireLock(handle, false)
    if (version(m2) == version(m1) || before == handle.data) {
      val m3 = upgradeLock(handle, m2)
      handle.data = after
      commitLock(handle, m3)
      true
    } else {
      discardLock(handle, m2)
      false
    }
  }

  def compareAndSetIdentity[T,R <: AnyRef with T](handle: Handle[T], before: R, after: T): Boolean = {
    // try to acquire exclusive ownership
    val m0 = handle.meta
    if (owner(m0) != UnownedSlot) {
      return invisibleCASI(handle, before, after)
    }
    val m1 = withChanging(withOwner(m0, NonTxnSlot))
    if (!handle.metaCAS(m0, m1)) {
      return invisibleCASI(handle, before, after)
    }

    if (before eq handle.data.asInstanceOf[AnyRef]) {
      handle.data = after
      commitLock(handle, m1)
      true
    } else {
      discardLock(handle, m1)
      false
    }
  }

  private def invisibleCASI[T,R <: T with AnyRef](handle: Handle[T], before: R, after: T): Boolean = {
    if (before eq get(handle).asInstanceOf[AnyRef]) {
      // CASI is different than CAS, because we don't have to invoke user code to
      // perform the comparison
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
    compareAndSetIdentity(handle, before, after)
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
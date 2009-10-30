/* CCSTM - (c) 2009 Stanford University - PPL */

// NonTxnBound

package edu.stanford.ppl.ccstm.impl


import edu.stanford.ppl.ccstm._

private[ccstm] class NonTxnBound[T](val unbind: Ref[T],
                                    protected val handle: Handle[T]) extends Ref.Bound[T] {
  import STMImpl._

  //////////////// lock waiting

  private def weakAwaitUnowned(m0: Meta) {
    STMImpl.weakAwaitUnowned(handle, m0, null)
  }

  //////////////// value waiting

  private def weakAwaitNewVersion(m0: Meta) {
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
      weakAwaitUnowned(m)
    } else {
      weakNoSpinAwaitNewVersion(m)
    }
  }

  private def weakNoSpinAwaitNewVersion(m0: Meta) {
    val event = wakeupManager.subscribe
    event.addSource(handle.ref, handle.offset)
    do {
      val m = handle.meta
      if (version(m) != version(m0) || changing(m)) {
        // observed new version, or likely new version (after unlock)
        return
      }

      // not changing, so okay to set PW bit
      if (pendingWakeups(m) || handle.metaCAS(m, withPendingWakeups(m))) {
        // after the block, things will have changed with reasonably high
        // likelihood (spurious wakeups are okay)
        event.await
        return
      }
    } while (!event.triggered)
  }

  //////////////// lock acquisition

  private def acquireLock(exclusive: Boolean): Meta = {
    var m0 = 0L
    var m1 = 0L
    do {
      m0 = handle.meta
      while (owner(m0) != UnownedSlot) {
        weakAwaitUnowned(m0)
        m0 = handle.meta
      }
      val mOwned = withOwner(m0, NonTxnSlot)
      m1 = if (exclusive) withChanging(mOwned) else mOwned
    } while (!handle.metaCAS(m0, m1))
    m1
  }

  /** Returns 0L on failure. */
  private def tryAcquireLock(exclusive: Boolean): Meta = {
    val m0 = handle.meta
    if (owner(m0) != UnownedSlot) return 0L

    val mOwned = withOwner(m0, NonTxnSlot)
    val m1 = if (exclusive) withChanging(mOwned) else mOwned

    if (!handle.metaCAS(m0, m1)) return 0L

    return m1
  }

  private def upgradeLock(m0: Meta): Meta = {
    var before = m0
    if (!handle.metaCAS(before, withChanging(before))) {
      // must have been a concurrent set of pendingWakeups
      before = withPendingWakeups(before)
      handle.meta = withChanging(before)
    }
    withChanging(before)
  }

  private def commitLock(m0: Meta) {
    releaseLock(m0, STMImpl.nonTxnWriteVersion(version(m0)))
  }

  private def discardLock(m0: Meta) {
    releaseLock(m0, version(m0))
  }

  private def releaseLock(m0: Meta, newVersion: Version) {
    // If pendingWakeups is set, then we are not racing with any other updates.
    // If the CAS fails, then we lost a race with pendingWakeups <- true, so we
    // can just assume that it's true.
    if (pendingWakeups(m0) || !handle.metaCAS(m0, withCommit(m0, newVersion))) {
      handle.meta = withCommit(withPendingWakeups(m0), newVersion)
      wakeupManager.trigger(wakeupManager.prepareToTrigger(handle.ref, handle.offset))
    }
  }

  //////////////// public interface

  def context: Option[Txn] = None

  def get: T = {
    var m0 = 0L
    var m1 = 0L
    var v: T = null.asInstanceOf[T]
    do {
      m0 = handle.meta
      while (changing(m0)) {
        weakAwaitUnowned(m0)
        m0 = handle.meta
      }
      v = handle.data
      m1 = handle.meta
      // TODO: fall back to pessimistic if we are starving
    } while (changingAndVersion(m0) != changingAndVersion(m1))
    v
  }

  def map[Z](f: (T) => Z): Z = f(get)

  def await(pred: T => Boolean) {
    while (true) {
      val m0 = handle.meta
      if (changing(m0)) {
        weakAwaitUnowned(m0)
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
          weakAwaitNewVersion(m1)
        }
      }
    }
  }

  def unrecordedRead: UnrecordedRead[T] = {
    while (true) {
      val m0 = handle.meta
      if (changing(m0)) {
        weakAwaitUnowned(m0)
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

  def releasableRead: ReleasableRead[T] = new ReleasableRead[T] {
    def context: Option[Txn] = None
    val value: T = get
    def release() {}
  }

  def set(v: T) {
    val m0 = acquireLock(true)
    handle.data = v
    commitLock(m0)
  }

  def getAndSet(v: T): T = {
    val m0 = acquireLock(true)
    val z = handle.data
    handle.data = v
    commitLock(m0)
    z
  }

  def tryWrite(v: T): Boolean = {
    val m0 = tryAcquireLock(true)
    if (m0 == 0L) {
      false
    } else {
      handle.data = v
      commitLock(m0)
      true
    }
  }

  def readForWrite: T = get

  def compareAndSet(before: T, after: T): Boolean = {
    // try to acquire ownership
    val m0 = handle.meta
    if (owner(m0) != UnownedSlot) {
      return invisibleCAS(before, after)
    }
    val m1 = withOwner(m0, NonTxnSlot)
    if (!handle.metaCAS(m0, m1)) {
      return invisibleCAS(before, after)
    }

    if (before == handle.data) {
      val m2 = upgradeLock(m1)
      handle.data = after
      commitLock(m2)
      true
    } else {
      discardLock(m1)
      false
    }
  }

  private def invisibleCAS(before: T, after: T): Boolean = {
    // this is the code from get, inlined so that we have access to the version
    // number as well with no boxing
    var m0 = 0L
    var m1 = 0L
    var v: T = null.asInstanceOf[T]
    do {
      m0 = handle.meta
      while (changing(m0)) {
        weakAwaitUnowned(m0)
        m0 = handle.meta
      }
      v = handle.data
      m1 = handle.meta
    } while (changingAndVersion(m0) != changingAndVersion(m1))

    // invisible failure?
    if (!(before == v)) return false

    // don't go directly to changing, because we can't run user code
    // (before.equals) there
    val m2 = acquireLock(false)
    if (version(m2) == version(m1) || before == handle.data) {
      val m3 = upgradeLock(m2)
      handle.data = after
      commitLock(m3)
      true
    } else {
      discardLock(m2)
      false
    }
  }

  def compareAndSetIdentity[R <: AnyRef with T](before: R, after: T): Boolean = {
    // try to acquire exclusive ownership
    val m0 = handle.meta
    if (owner(m0) != UnownedSlot) {
      return invisibleCASI(before, after)
    }
    val m1 = withChanging(withOwner(m0, NonTxnSlot))
    if (!handle.metaCAS(m0, m1)) {
      return invisibleCASI(before, after)
    }

    if (before eq handle.data.asInstanceOf[AnyRef]) {
      handle.data = after
      commitLock(m1)
      true
    } else {
      discardLock(m1)
      false
    }
  }

  private def invisibleCASI[R <: T with AnyRef](before: R, after: T): Boolean = {
    if (before eq get.asInstanceOf[AnyRef]) {
      // CASI is different than CAS, because we don't have to invoke user code to
      // perform the comparison
      val m0 = acquireLock(true)
      if (before eq handle.data.asInstanceOf[AnyRef]) {
        handle.data = after
        commitLock(m0)
        true
      } else {
        discardLock(m0)
        false
      }
    } else {
      // invisible failure
      false
    }
  }

  def weakCompareAndSet(before: T, after: T): Boolean = {
    compareAndSet(before, after)
  }

  def weakCompareAndSetIdentity[R <: AnyRef with T](before: R, after: T): Boolean = {
    compareAndSetIdentity(before, after)
  }

  def transform(f: T => T) {
    getAndTransform(f)
  }

  def getAndTransform(f: T => T): T = {
    getAndTransformImpl(f, acquireLock(false))
  }

  def tryTransform(f: T => T): Boolean = {
    val m0 = tryAcquireLock(false)
    if (m0 == 0L) {
      false
    } else {
      getAndTransformImpl(f, m0)
      true
    }
  }

  private def getAndTransformImpl(f: T => T, m0: Meta): T = {
    val v0 = handle.data
    val repl = try { f(v0) } catch { case x => discardLock(m0) ; throw x }
    val m1 = upgradeLock(m0)
    handle.data = repl
    commitLock(m1)
    v0
  }

  def transformIfDefined(pf: PartialFunction[T,T]): Boolean = {
    if (pf.isDefinedAt(get)) {
      val m0 = acquireLock(false)
      val v = handle.data
      if (try { pf.isDefinedAt(v) } catch { case x => discardLock(m0) ; throw x }) {
        val repl = try { pf(v) } catch { case x => discardLock(m0) ; throw x }
        val m1 = upgradeLock(m0)
        handle.data = repl
        commitLock(m1)
        true
      } else {
        discardLock(m0)
        false
      }
    } else {
      // invisible failure
      false
    }
  }
  
}
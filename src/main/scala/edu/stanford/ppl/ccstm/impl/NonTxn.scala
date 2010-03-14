/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// NonTxn

package edu.stanford.ppl.ccstm.impl

import edu.stanford.ppl.ccstm._


/** The object that contains the code for non-transactional read and write
 *  barriers.
 */
private[ccstm] object NonTxn {
  import STMImpl._

  //////////////// lock waiting

  private def weakAwaitUnowned(handle: Handle[_], m0: Meta) {
    STMImpl.weakAwaitUnowned(handle, m0, null)
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

  /** Returns 0L on failure. */
  private def tryAcquireLock(handle: Handle[_], exclusive: Boolean): Meta = {
    val m0 = handle.meta
    if (owner(m0) != UnownedSlot) return 0L

    val mOwned = withOwner(m0, NonTxnSlot)
    val m1 = if (exclusive) withChanging(mOwned) else mOwned

    if (!handle.metaCAS(m0, m1)) return 0L

    return m1
  }

  private def upgradeLock(handle: Handle[_], m0: Meta): Meta = {
    var before = m0
    if (!handle.metaCAS(before, withChanging(before))) {
      // must have been a concurrent set of pendingWakeups
      before = withPendingWakeups(before)
      handle.meta = withChanging(before)
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
    // If pendingWakeups is set, then we are not racing with any other updates.
    // If the CAS fails, then we lost a race with pendingWakeups <- true, so we
    // can just assume that it's true.  When a non-transactional thread holds a
    // lock there is no txn on which to wait for completion, so we overload by
    // waiting for a wakeup.  Metadata may be shared between multiple offsets,
    // though, so we need to have a single channel to wake up those waiters,
    // which is the metaOffset.
    if (pendingWakeups(m0) || !handle.metaCAS(m0, withCommit(m0, newVersion))) {
      handle.meta = withCommit(withPendingWakeups(m0), newVersion)
      val r = handle.ref
      val o1 = handle.offset
      val o2 = handle.metaOffset
      var wakeups = wakeupManager.prepareToTrigger(r, o1)
      if (o1 != o2) wakeups |= wakeupManager.prepareToTrigger(r, o2)
      wakeupManager.trigger(wakeups)
    }
  }

  //////////////// public interface

  def transform2[A,B,Z](handleA: Handle[A], handleB: Handle[B], f: (A,B) => (A,B,Z)): Z = {
    var mA0: Long = 0L
    var mB0: Long = 0L
    var tries = 0
    do {
      mA0 = acquireLock(handleA, true)
      mB0 = tryAcquireLock(handleB, true)
      if (mB0 == 0) {
        // tryAcquire failed
        discardLock(handleA, mA0)
        mA0 = 0

        // did it fail because the handles are equal?
        if (handleA == handleB) throw new IllegalArgumentException("transform2 targets must be distinct")

        // try it in the opposite direction
        mB0 = acquireLock(handleB, true)
        mA0 = tryAcquireLock(handleA, true)

        if (mA0 == 0) {
          // tryAcquire failed
          discardLock(handleB, mB0)
          mB0 = 0

          tries += 1
          if (tries > 10) {
            // fall back to a txn, which is guaranteed to eventually succeed
            return STM.atomic((t: Txn) => {
              val refA = new Ref.TxnBound(null, handleA, t)
              val refB = new Ref.TxnBound(null, handleB, t)
              val (a,b,z) = f(refA.readForWrite, refB.readForWrite)
              refA := a
              refB := b
              z
            })
          }
        }
      }
    } while (mB0 == 0)

    val (a,b,z) = try {
      f(handleA.data, handleB.data)
    } catch {
      case x => {
        discardLock(handleA, mA0)
        discardLock(handleB, mB0)
        throw x
      }
    }

    handleA.data = a
    handleB.data = b

    val wv = STMImpl.nonTxnWriteVersion(Math.max(version(mA0), version(mB0)))
    releaseLock(handleA, mA0, wv)
    releaseLock(handleB, mB0, wv)
    return z
  }

  def getAndAdd(handle: Handle[Int], delta: Int): Int = {
    val m0 = acquireLock(handle, true)
    val v0 = handle.data
    handle.data = v0 + delta
    commitLock(handle, m0)
    v0
  }
}

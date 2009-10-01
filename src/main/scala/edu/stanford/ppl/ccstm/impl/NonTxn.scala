/* CCSTM - (c) 2009 Stanford University - PPL */

// NonTxn

package edu.stanford.ppl.ccstm.impl


/** The object that contains the code for non-transactional read and write
 *  barriers.
 */
private[ccstm] object NonTxn {
  import STMImpl._

  def get[T](handle: Handle[T]): T = {
    var m0 = handle.meta
    var v: T = null.asInstanceOf[T]
    var m1 = 0L
    do {
      if (changing(m0)) {
        m0 = waitForUnlock(handle)
      }
      v = handle.data
      m1 = handle.meta
    } while (changingAndVersion(m0) != changingAndVersion(m1))
    v
  }

  private def awaitUnlock(handle: Handle[_]): Meta = {
    var m = handle.meta
    while (changing(m)) {
      if (owner(m) == NonTxnSlot) {
        awaitNonTxnUnlock(handle)
      } else {
        val o = slotManager(owner(m))
        val m1 = handle.meta
        if (owner(m0) == owner(m1)) {
          // stable read of owner
          o.awaitCompletedOrDoomed
        }
      }
      m = handle.meta
    }
    m
  }

  private def awaitUnowned(handle: Handle[_]) {
    // spin a bit
    var m = 0L
    var spins = 0
    while (spins < SpinCount + YieldCount) {
      spins += 1
      if (spins > SpinCount) Thread.`yield`

      m = handle.meta
      if (owner(m) == UnownedSlot) return
    }

    // to wait for a non-txn owner, we use pendingWakeups
    if (owner(m) == NonTxnSlot) {
      val event = wakeupManager.subscribe
      event.addSource(handle.ref, handle.offset)
      var done = false
      while (!done) {
        m = handle.meta
        if (owner(m) != NonTxnSlot) {
          done = true
        }
        else if (pendingWakeups(m) || handle.metaCAS(m, withPendingWakeups(m))) {
          event.await
          done = true
        }
        else {
          done = event.triggered
        }
      }
    } else {
      // to wait for a txn owner, we track down the Txn and wait on it
      val o = slotManager(owner(m)) // TODO: fix this race
      val m1 = handle.meta
      if (owner(m) == owner(m1)) {
        o.awaitCompletedOrDoomed

        // unlocked in definition, but might still be marked owned

        val m2 = handle.meta
        if (owner(m2) == owner(m) && version(m1) == version(m2)) {
          assert(o.status.mustRollBack)
          // help out the doomed txn
          handle.metaCAS(m2, withRollback(m2))
        }
      }
      // else invalid read of owner, means ownership change, means condition met
    }
  }


  /** Returns 1 for wait-is-over, -1 for not wait-is-over, 0 for unstable read. */
  private def waitIsOver[T](handle: Handle[T],
                            m: Meta,
                            metaPred: Long => Boolean,
                            pred: T => Boolean): Int = {
    if (metaPred != null && !metaPred(m)) {
      return -1
    }

    if (pred != null) {
      // can't read if changing
      if (changing(m)) return -1

      val v = handle.data
      val m1 = handle.meta

      // was read stable
      if (changingAndVersion(m) != changingAndVersion(m1)) return 0

      if (!pred(v)) return -1
    }

    // passed the gauntlet
    return 1
  }


  /** Returns after owner(handle.meta) is observed to be something other than
   *  NonTxnSlot.
   */
  private def awaitNotNonTxnOwned(handle: Handle[_]) {
    // spin a bit
    var spins = 0
    while (spins < SpinCount + YieldCount) {
      spins += 1
      if (spins > SpinCount) Thread.`yield`

      val m = handle.meta
      if (!changing(m) || owner(m) != NonTxnSlot) return
    }

    // enqueue ourself for wakeup
    val event = wakeupManager.subscribe
    event.addSource(handle.ref, handle.offset)
    while (!event.triggered) {
      val m = handle.meta
      if (!changing(m) || owner(m) != NonTxnSlot) return

      if (pendingWakeups(m) || handle.metaCAS(m, withPendingWakeups(m))) {
        event.await
        return
      }
    }
  }

  def await[T](handle: Handle[T], pred: T => Boolean) {
  }

  def unrecordedRead[T](handle: Handle[T]): UnrecordedRead[T] = {
  }

  def set[T](handle: Handle[T], v: T) {

  }

  def tryWrite[T](handle: Handle[T], v: T): Boolean = {

  }

  def freeze[T](handle: Handle[T]) {

  }

  def compareAndSet[T](handle: Handle[T], before: T, after: T): Boolean = {

  }

  def compareAndSetIdentity[T,R <: AnyRef with T](handle: Handle[T], before: R, after: T): Boolean = {

  }

  def weakCompareAndSet[T](handle: Handle[T], before: T, after: T): Boolean = {

  }

  def weakCompareAndSetIdentity[T,R <: AnyRef with T](handle: Handle[T], before: R, after: T): Boolean = {

  }

  def transform[T](handle: Handle[T], f: T => T) {

  }

  def transformIfDefined[T](handle: Handle[T], pf: PartialFunction[T,T]): Boolean = {
    
  }
}
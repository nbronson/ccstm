/* CCSTM - (c) 2009 Stanford University - PPL */

// STMImpl

package edu.stanford.ppl.ccstm.impl


/** An STM implementation that uses a TL2-style timestamp system, but that
 *  performs eager acquisition of write locks and that revalidates the
 *  transaction to extend the read version, rather than rolling back.  Values
 *  are stored separately from version metadata, so metadata may be shared
 *  between multiple data slots.  Speculative values are stored in a separate
 *  write buffer, but since write permission is acquired eagerly the write
 *  permission bit is used to gate lookup in the write buffer.  (Write buffer
 *  lookups may miss only if metadata is shared.)
 *  <p>
 *  Metadata is a 64 bit long, of which 1 bit records whether any pending
 *  wakeups should be triggered if the associated data is changed, 10 bits
 *  record the write permission owner (0 means no owner, 1 means non-txn
 *  owner, 2 means a value is frozen), 1 bit flags values that are committing
 *  and may not be accessed, 1 bit is available for the user of a Handle, and
 *  51 bits record the version number.  2^51 is a bit more than 2*10^15.  On a
 *  hypothetical computer that could perform a non-transactional write in 10
 *  nanoseconds (each of which requires at least 3 CAS-s), version numbers
 *  would not overflow for 250 days of continuous writes.  The all 1-s version
 *  number is used to indicate a value that is frozen.
 *
 *  @author Nathan Bronson
 */
private[ccstm] object STMImpl extends GV6 {

  /** The number of times to spin tightly when waiting for a condition to
   *  become true.
   */
  val SpinCount = 100

  /** The number of times to spin tightly when waiting for another thread to
   *  perform work that we can also perform.
   */
  val StealSpinCount = 10

  /** The number of times to spin with intervening calls to
   *  <code>Thread.yield</code> when waiting for a condition to become true.
   *  These spins will occur after the <code>SpinCount</code> spins.  After
   *  <code>SpinCount + YieldCount</code> spins have been performed, the
   *  waiting thread will be blocked on a Java mutex.
   */
  val YieldCount = 100

  val slotManager = new TxnSlotManager[TxnImpl](1024, 3)
  val wakeupManager = new WakeupManager // default size

  /** Hashes <code>ref</code> with <code>offset</code>, mixing the resulting
   *  bits.  This hash function is chosen so that it is suitable as a basis for
   *  hopscotch hashing (among other purposes).
   *  @throw NullPointerException if <code>ref</code> is null. 
   */
  def hash(ref: AnyRef, offset: Int): Int = {
    // Hopscotch will fail if there are more than H entries that end up in the
    // same bucket.  This can lead to a pathological case if a single instance
    // is used with offsets that are strided by a power-of-two, because if we
    // use a simple hashCode(ref) op M*offset, we will fail to get any unique
    // bits from the M*offset portion.  Our solution is to use the bit-mixing
    // function from java.util.HashMap after merging with the system identity
    // hash code.
    if (ref == null) throw new NullPointerException
    var h = System.identityHashCode(ref) ^ (0x40108097 * offset)
    h ^= (h >>> 20) ^ (h >>> 12)
    h ^= (h >>> 7) ^ (h >>> 4)
    h
  }

  //////////////// Metadata bit packing

  // metadata bits are:
  //  63 = locked for change
  //  62 = pending wakeups
  //  61 = user bit
  //  51..60 = owner slot
  //  0..50 = version
  type Meta = Long
  type Slot = Int
  type Version = Long

  /** The slot number used when a memory location has not been reserved or
   *  locked for writing.
   */
  val UnownedSlot: Slot = 0

  /** The slot number used by non-transactional code that reserves or locks
   *  a location for writing.
   */
  val NonTxnSlot: Slot = 1

  /** The slot number that claims ownership of frozen handles. */
  val FrozenSlot: Slot = 2


  // TODO: clean up the following mess
  
  def owner(m: Meta): Slot = (m >> 51).asInstanceOf[Int] & 1023
  def version(m: Meta): Version = (m & ((1L << 51) - 1))
  def pendingWakeups(m: Meta): Boolean = (m & (1L << 62)) != 0
  def changing(m: Meta): Boolean = m < 0
  def userBit(m: Meta): Boolean = (m & (1L << 61)) != 0

  // masks off userBit, owner, and pendingWakeups
  def changingAndVersion(m: Meta) = m & ((1L << 63) | ((1L << 51) - 1))
  def ownerAndVersion(m: Meta) = m & ((1023L << 51) | ((1L << 51) - 1))

  def withOwner(m: Meta, o: Slot): Meta = (m & ~(1023L << 51)) | (o.asInstanceOf[Long] << 51)
  def withUnowned(m: Meta): Meta = withOwner(m, UnownedSlot)
  def withVersion(m: Meta, ver: Version) = (m & ~((1L << 51) - 1)) | ver

  /** It is not allowed to set PendingWakeups if Changing and Owner != NonTxnSlot. */
  def withPendingWakeups(m: Meta): Meta = m | (1L << 62)
  def withNoPendingWakeups(m: Meta): Meta = m & ~(1L << 62)
  def withChanging(m: Meta): Meta = m | (1L << 63)
  def withUnchanging(m: Meta): Meta = m & ~(1L << 63)
  def withUserBit(m: Meta, bit: Boolean): Meta = if (bit) m | (1L << 61) else m & ~(1L << 61)

  /** Includes withUnowned, withNoPendingWakeups, withUnchanging, and withVersion. */
  def withCommit(m: Meta, ver: Version) = (m & (1L << 61)) | ver

  /** Includes withUnowned and withUnchanging. */
  def withRollback(m: Meta) = withUnowned(withUnchanging(m))

  //////////////// Conditional retry

  def awaitRetry(explicitRetries: Txn.ExplicitRetryCause*) {
    assert(explicitRetries.exists(_.readSet.asInstanceOf[ReadSet].size > 0))

    // Spin a few times, counting one spin per read set element
    var spins = 0
    while (spins < SpinCount + YieldCount) {
      for (er <- explicitRetries) {
        val rs = er.readSet.asInstanceOf[ReadSet]
        if (!readSetStillValid(rs)) return
        spins += rs.size
      }
      if (spins > SpinCount) Thread.`yield`
    }

    while (true) {
      val event = wakeupManager.subscribe
      for (er <- explicitRetries) {
        val rs = er.readSet.asInstanceOf[ReadSet]
        var i = 0
        while (i < rs.size) {
          val handle = rs.handles(i)

          if (!event.addSource(handle.ref, handle.offset)) return

          var m = 0L
          do {
            m = handle.meta
            if (changing(m) || version(m) != rs.versions(i)) return
          } while (!pendingWakeups(m) && !handle.metaCAS(m, withPendingWakeups(m)))

          i += 1
        }
      }
      event.await
    }
  }

  private def readSetStillValid(rs: ReadSet): Boolean = {
    var i = 0
    while (i < rs.size) {
      val m = rs.handles(i).meta
      if (changing(m) || version(m) != rs.versions(i)) return false
      i += 1
    }
    return true
  }

  //////////////// lock release helping

  def stealHandle(handle: Handle[_], m0: Meta, owningTxn: TxnImpl) {
    assert(owningTxn.status.mustRollBack)

    // We can definitely make forward progress at the expense of a couple of
    // extra CAS, but it is not useful for us to do a big spin with yields.
    var spins = 0
    do {
      val m = handle.meta
      if (ownerAndVersion(m) != ownerAndVersion(m0)) {
        // no steal needed
        return
      }

      spins += 1
    } while (spins < StealSpinCount)

    // If owningTxn has been doomed it might be a while before it releases its
    // lock on the handle.  Slot numbers are reused, however, so we have to
    // manage a reference count on the slot while we steal the handle.  This is
    // expensive, which is why we just spun.

    val owningSlot = owner(m0)
    val o = slotManager.beginLookup(owningSlot)
    try {
      if (o ne owningTxn) {
        // owningTxn unregistered itself from slotManager, so it has already
        // released all of its locks
        return
      }

      while (true) {
        val m = handle.meta
        if (ownerAndVersion(m) != ownerAndVersion(m0) || handle.metaCAS(m, withRollback(m))) {
          // no longer locked, or steal succeeded
          return
        }
      }
    } finally {
      slotManager.endLookup(owningSlot, o)
    }
  }

  //////////////// lock waiting

  /** Once <code>handle.meta</code> has been unlocked since a time it had
   *  value <code>m0</code>, the method will return.  It might return sooner,
   *  but an attempt is made to do the right thing.  If <code>currentTxn</code>
   *  is non-null, <code>currentTxn.requireActive</code> will be called before
   *  blocking and <code>currentTxn.resolveWriteWriteConflict</code> will be
   *  called before waiting for a transaction.
   */
  private[impl] def weakAwaitUnowned(handle: Handle[_], m0: Meta, currentTxn: TxnImpl) {
    owner(m0) match {
      case NonTxnSlot => weakAwaitNonTxnUnowned(handle, m0, currentTxn)
      case FrozenSlot => throw new IllegalStateException("frozen")
      case _ => weakAwaitTxnUnowned(handle, m0, currentTxn)
    }
  }

  private def weakAwaitNonTxnUnowned(handle: Handle[_], m0: Meta, currentTxn: TxnImpl) {
    // spin a bit
    var spins = 0
    while (spins < SpinCount + YieldCount) {
      spins += 1
      if (spins > SpinCount) Thread.`yield`

      val m = handle.meta
      if (ownerAndVersion(m) != ownerAndVersion(m0)) return

      if (currentTxn != null) currentTxn.requireActive
    }

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
        event.await(currentTxn)
        return
      }
    } while (!event.triggered)
  }

  private def weakAwaitTxnUnowned(handle: Handle[_], m0: Meta, currentTxn: TxnImpl) {
    if (currentTxn == null) {
      // Spin a bit, but only from a non-txn context.  If this is a txn context
      // We need to roll ourself back ASAP if that is the proper resolution.
      var spins = 0
      while (spins < SpinCount + YieldCount) {
        spins += 1
        if (spins > SpinCount) Thread.`yield`

        val m = handle.meta
        if (ownerAndVersion(m) != ownerAndVersion(m0)) return

        if (currentTxn != null) currentTxn.requireActive
      }
    }

    // to wait for a txn owner, we track down the Txn and wait on it
    val owningSlot = owner(m0)
    val owningTxn = slotManager.beginLookup(owningSlot)
    try {
      if (owningTxn != null && owningSlot == owner(handle.meta)) {
        if (!owningTxn.completedOrDoomed) {
          if (currentTxn != null) {
            currentTxn.resolveWriteWriteConflict(owningTxn, handle)
          }
          owningTxn.awaitCompletedOrDoomed()
        }

        // we've already got the beginLookup, so no need to do a standalone
        // stealHandle
        var m = 0L
        do {
          m = handle.meta
          assert(ownerAndVersion(m) != ownerAndVersion(m0) || owningTxn._status.mustRollBack)
        } while (ownerAndVersion(m) == ownerAndVersion(m0) && !handle.metaCAS(m, withRollback(m)))

        // no longer locked, or steal succeeded
      }
    } finally {
      slotManager.endLookup(owningSlot, owningTxn)
    }
  }
}
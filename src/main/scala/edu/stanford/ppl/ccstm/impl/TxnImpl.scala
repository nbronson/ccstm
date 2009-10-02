/* CCSTM - (c) 2009 Stanford University - PPL */

// TxnImpl

package edu.stanford.ppl.ccstm.impl


abstract class TxnImpl(failureHistory: List[Txn.RollbackCause]) extends AbstractTxn with TxnWriteBuffer {

  import STMImpl._
  import Txn._

  //////////////// State

  // _status and _statusCAS come from StatusHolder via AbstractTxn

  // writeBufferSize, writeBufferGet, writeBufferPut, and writeBufferVisit come
  // from TxnWriteBuffer

  private[impl] var _readCount = 0
  private[impl] var _readHandles = new Array[Handle[_]](16)
  private[impl] var _readVersions = new Array[Long](16)

  private[impl] def addToReadSet(handle: Handle[_], version: Long) {
    if (_readCount == _readHandles.length) growReadSet()
    _readHandles(_readCount) = handle
    _readVersions(_readCount) = version
    _readCount += 1
  }

  private def growReadSet() {
    _readHandles = java.util.Arrays.copyOf(_readHandles, _readHandles.length * 2)
    _readVersions = java.util.Arrays.copyOf(_readVersions, _readVersions.length * 2)
  }

  /** The <code>VersionTrap</code> associated with <code>_readVersion</code>,
   *  used to make sure that any ephemeral handles created after this
   *  transaction's virtual read snapshot will survive until the snapshot's
   *  validation.
   */
  private var _readVersionTrap: VersionTrap = STMImpl.freshReadVersion

  /** The read version of this transaction.  It is guaranteed that all values
   *  read by this transaction have a version number less than or equal to this
   *  value, and that any transaction whose writes conflict with this
   *  transaction will label those writes with a version number greater than
   *  this value.  The read version must never be greater than
   *  <code>globalVersion.get</code>, must never decrease, and each time it is
   *  changed the read set must be revalidated.
   */
  private[impl] var _readVersion: Version = _readVersionTrap.version


  /** True if all reads should be performed as writes. */
  private[impl] val barging: Boolean = {
    // barge if we have already had 2 failures since the last explicit retry
    var cur = failureHistory
    var count = 0
    while (count < 2 && !cur.isEmpty && !cur.head.isInstanceOf[ExplicitRetryCause]) {
      cur = cur.tail
      count += 1
    }
    (count == 3)
  }

  /** The slot assigned to this transaction. */
  private val slot = slotManager.assign(this)

  /** Higher is better. */
  // TODO: think about this more deeply
  private val priority = -System.identityHashCode(this)

  override def toString = {
    ("Txn@" + Integer.toHexString(hashCode) + "(" + status +
            ", slot=" + slot +
            ", priority=" + priority +
            ", readCount=" + _readCount +
            ", writeBufferSize=" + writeBufferSize +
            ", readVersion=" + _readVersion +
            (if (barging) ", barging" else "") + ")")
  }


  //////////////// Implementation

  /** On return, the read version will have been the global version at some
   *  point during the call, the read version will be &ge; minReadVersion, and
   *  all reads will have been validated against the new read version.  Throws
   *  <code>RollbackError</code> if invalid.
   */
  private def revalidate(minReadVersion: Version) {
    _readVersionTrap = freshReadVersion(minReadVersion)
    _readVersion = _readVersionTrap.version
    if (!revalidateImpl()) throw RollbackError
  }

  /** Returns true if valid. */
  private def revalidateImpl(): Boolean = {
    var i = 0
    while (i < _readCount) {
      val handle = _readHandles(i)
      val m1 = handle.meta
      if (!changing(m1) || owner(m1) == slot) {
        if (version(m1) != _readVersions(i)) {
          forceRollback(InvalidReadCause(handle.ref, handle.offset))
          return false
        }
        i += 1
      } else if (owner(m1) == NonTxnSlot) {
        // non-txn updates don't set changing unless they will install a new
        // value, so we are the only party that can yield
        forceRollback(InvalidReadCause(handle.ref, handle.offset))
        return false
      } else {
        val o = slotManager.lookup(owner(m1))
        val s = o._status
        val m2 = handle.meta
        if (owner(m2) == owner(m1)) {
          // Either this txn or the owning must roll back.  We choose to give
          // precedence to the owning txn, as it is the writer and is
          // Validating.  There's a bit of trickiness since o may not be the
          // owning transaction, it may be a new txn that reused the same slot.
          // If the actual owning txn committed then the version number will
          // have changed, which we will detect on the next pass because we
          // aren't incrementing i.  If it rolled back then we don't have to.
          // If the status that we see is Active, then we must have observed
          // the race and we don't roll back here.
          if (s.mightCommit && s != Active) {
            forceRollback(InvalidReadCause(handle.ref, handle.offset))
            return false
          }

          stealHandle(handle, m2, o)

          // try again on this i
        }
        // else stale read of owner, try again on this i
      }
    }

    // STM managed reads were okay, what about explicitly registered read-only
    // resources?
    return readResourcesValidate()
  }

  /** After this method returns, either the current transaction will have been
   *  rolled back or <code>currentOwner</code> will allow the write resource to
   *  be acquired.
   */
  private[impl] def resolveWriteWriteConflict(currentOwner: TxnImpl, contended: AnyRef) {
    val cause = WriteConflictCause(contended, null)

    // This test is _almost_ symmetric.  Tie goes to neither.
    if (this.priority <= currentOwner.priority) {
      forceRollback(cause)
      throw RollbackError
    } else {
      currentOwner.requestRollback(cause)
    }
  }

  private[ccstm] def retryImpl() {
    if (_readCount == 0) {
      throw new IllegalStateException("retry doesn't make sense with empty read set")
    }

    forceRollback(new ExplicitRetryCause(ReadSet(_readCount, _readVersions, _readHandles)))
    throw RollbackError
  }

  private[ccstm] def commitImpl(): Status = {
    if (status.mustRollBack || !writeLikeResourcesPrepare()) {
      return completeRollback()
    }

    if (writeBufferSize == 0 && !writeResourcesPresent) {
      // read-only transactions are easy to commit, because all of the reads
      // are already guaranteed to be consistent
      if (!_statusCAS(Active, Committed)) {
        // remote requestRollback got us at the last moment
        assert(_status.isInstanceOf[RollingBack])
        _status = Rolledback(status.rollbackCause)
      }
      callAfter()
      return _status
    }

    if (!_statusCAS(Active, Validating)) return completeRollback()

    if (!acquireLocks()) return completeRollback()

    // this is our linearization point
    val commitVersion = freshCommitVersion(globalVersion.get)

    // if the reads are still valid, then they were valid at the linearization
    // point
    if (!revalidateImpl()) return completeRollback()

    // attempt to decide commit
    if (!_statusCAS(Validating, Committing)) return completeRollback()

    commitWrites(commitVersion)
    writeResourcesPerformCommit()
    _status = Committed
    callAfter()

    return Committed
  }

  private def completeRollback(): Status = {
    rollbackWrites()
    writeResourcesPerformRollback()
    _status = Rolledback(status.rollbackCause)
    callAfter()

    return _status
  }

  private def rollbackWrites() {
    assert(_status.isInstanceOf[RollingBack])
    writeBufferVisit(new TxnWriteBuffer.Visitor {
      def visit(specValue: Any, handle: Handle[_]): Boolean = {
        var m = handle.meta
        while (owner(m) == slot) {
          // we must use CAS because there can be concurrent pendingWaiter adds
          // and concurrent "helpers" that release the lock
          if (handle.metaCAS(m, withRollback(m))) return true
          m = handle.meta
        }
        return true
      }
    })

    // we don't need our slot any more
    slotManager.release(slot)
  }

  private def acquireLocks(): Boolean = {
    writeBufferVisit(new TxnWriteBuffer.Visitor {
      def visit(specValue: Any, handle: Handle[_]): Boolean = {
        var m = handle.meta
        if (!changing(m)) {
          // remote requestRollback might have doomed us, followed by a steal
          // of this handle, so we must verify ownership each try
          while (owner(m) == slot) {
            if (handle.metaCAS(m, withChanging(m))) return true
            m = handle.meta
          }
          assert(_status.isInstanceOf[RollingBack])
          return false
        }
        return true
      }
    }) && (_status == Validating)    
  }

  private def commitWrites(commitVersion: Version) {
    // first pass
    var wakeups = 0L
    writeBufferVisit(new TxnWriteBuffer.Visitor {
      def visit(specValue: Any, handle: Handle[_]): Boolean = {
        // TODO: remove this check
        assert(version(handle.meta) < commitVersion)

        // update the values
        handle.asInstanceOf[Handle[Any]].data = specValue

        // We must accumulate the pending wakeups during this pass, because
        // during the second pass we clear the PW bit on the first handle that
        // references a particular metadata.
        if (pendingWakeups(handle.meta)) {
          wakeups |= wakeupManager.prepareToTrigger(handle.ref, handle.offset)
        }
        true
      }
    })

    // second pass
    writeBufferVisit(new TxnWriteBuffer.Visitor {
      def visit(specValue: Any, handle: Handle[_]): Boolean = {
        val m = handle.meta
        if (owner(m) == slot) {
          // release the lock, clear the PW bit, and update the version
          handle.meta = withCommit(m, commitVersion)
        }
        true
      }
    })

    // we don't need our slot any more
    slotManager.release(slot)

    // unblock anybody waiting on a value change that has just occurred
    wakeupManager.trigger(wakeups)
  }

  private[ccstm] def requestRollbackImpl(cause: RollbackCause): Boolean = {
    while (true) {
      val s = _status
      if (s.mustCommit) {
        return false
      } else if (s.mustRollBack) {
        return true
      } else {
        assert(s == Active || s == Validating)
        if (_statusCAS(s, RollingBack(cause))) return true
      }
    }
    throw new Error("unreachable")
  }

  private[ccstm] def explicitlyValidateReadsImpl() {
    revalidate(0)
  }

  //////////////// lock waiting - similar to NonTxn but we also check for remote rollback

  private def weakAwaitUnowned(handle: Handle[_], m0: Meta) {
    // spin a bit
    var spins = 0
    while (spins < SpinCount + YieldCount) {
      spins += 1
      if (spins > SpinCount) Thread.`yield`

      val m = handle.meta
      if (ownerAndVersion(m) != ownerAndVersion(m0)) return

      requireActive
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
        requireActive
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
      requireActive
      owningTxn.awaitCompletedOrDoomed

      if (ownerAndVersion(handle.meta) == ownerAndVersion(m0)) {
        assert(owningTxn.status.mustRollBack)
        requireActive
        stealHandle(handle, m0, owningTxn)
      }
    }
    // else invalid read of owningTxn, which means there was an ownership change
  }

  //////////////// barrier implementations
  
  def get[T](handle: Handle[T]): T = {
    requireActive

    var m = handle.meta
    if (owner(m) == slot) {
      // Self-owned.  This particular ref+offset might not be in the write
      // buffer, but it's definitely not in anybody else's.
      return writeBufferGet(handle)
    }

    while (true) {
      if (changing(m)) {
        weakAwaitUnowned(handle, m)
      } else {
        val v = handle.data
        val m1 = handle.meta
        if (changingAndVersion(m) == changingAndVersion(m1)) {
          // Stable read.  The second read of handle.meta is required for
          // opacity, and it also enables our read-only commit optimization.
          addToReadSet(handle, version(m))
          return v
        }
        // try again
        m = m1
      }
    }
  }

  def map[T,Z](handle: Handle[T], f: T => Z): Z = {
    val u = unrecordedRead(handle)
    val result = f(u.value)
    if (!u.recorded) {
      val callback = new Txn.ReadResource {
        var _latestRead = u

        def valid(t: Txn) = {
          if (!_latestRead.stillValid) {
            // reread, and see if that changes the result
            _latestRead = unrecordedRead(handle)
            val reapply = f(_latestRead.value)
            result == reapply
          } else {
            true
          }
        }
      }

      // It is safe to skip calling callback.valid() here, because we
      // have made no calls into the txn that might have resulted in it
      // moving its virtual snapshot forward.  This means that the
      // unrecorded read that initialized u is consistent with all of the
      // reads performed so far.
      addReadResource(callback, 0, false)
    }

    result
  }

  // TODO: think about blind writes to values that share metadata

  def unrecordedRead[T](handle: Handle[T]): UnrecordedRead[T] = null // TODO implement

  def set[T](handle: Handle[T], v: T) {} // TODO implement
  def tryWrite[T](handle: Handle[T], v: T): Boolean = false // TODO implement
  def freeze(handle: Handle[_]) {} // TODO implement

  def readForWrite[T](handle: Handle[T]): T = null.asInstanceOf[T] // TODO implement

  def compareAndSet[T](handle: Handle[T], before: T, after: T): Boolean = {
    transformIfDefined(handle, new PartialFunction[T,T] {
      def isDefinedAt(v: T): Boolean = before == v
      def apply(v: T): T = after
    })
  }

  def compareAndSetIdentity[T, R <: T with AnyRef](handle: Handle[T], before: R, after: T): Boolean = {
    transformIfDefined(handle, new PartialFunction[T,T] {
      def isDefinedAt(v: T): Boolean = (before eq v.asInstanceOf[AnyRef])
      def apply(v: T): T = after
    })
  }

  def weakCompareAndSet[T](handle: Handle[T], before: T, after: T): Boolean = {
    compareAndSet(handle, before, after)
  }

  def weakCompareAndSetIdentity[T, R <: T with AnyRef](handle: Handle[T], before: R, after: T): Boolean = {
    compareAndSetIdentity(handle, before, after)
  }

  def transform[T](handle: Handle[T], f: T => T) = {
    set(handle, f(readForWrite(handle)))
  }
  
  def transformIfDefined[T](handle: Handle[T], pf: PartialFunction[T,T]): Boolean = {
    val u = unrecordedRead(handle)
    if (!pf.isDefinedAt(u.value)) {
      // make sure it stays undefined
      if (!u.recorded) {
        val callback = new Txn.ReadResource {
          var _latestRead = u

          def valid(t: Txn) = {
            if (!_latestRead.stillValid) {
              // if defined after reread then return false==invalid
              _latestRead = unrecordedRead(handle)
              !pf.isDefinedAt(_latestRead.value)
            } else {
              true
            }
          }
        }
        addReadResource(callback, 0, false)
      }
      false
    } else {
      val v = readForWrite(handle)
      if (!pf.isDefinedAt(v)) {
        // value changed after unrecordedRead
        false
      } else {
        // still defined, do the actual transform
        set(handle, pf(v))
        true
      }
    }
  }

}
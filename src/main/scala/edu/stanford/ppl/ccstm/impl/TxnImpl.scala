/* CCSTM - (c) 2009 Stanford University - PPL */

// TxnImpl

package edu.stanford.ppl.ccstm.impl


class TxnImpl(failureHistory: List[Txn.RollbackCause]) extends AbstractTxn with TxnWriteBuffer {

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
      val meta = handle.meta
      if (!changing(meta) || owner(meta) == slot) {
        if (version(meta) != _readVersion(i)) {
          forceRollback(InvalidReadCause(handle.ref, handle.offset))
          return false
        }
        i += 1
      } else if (owner(meta) == NonTxnSlot) {
        // non-txn updates don't set changing unless they will install a new
        // value, so we are the only party that can yield
        forceRollback(InvalidReadCause(handle.ref, handle.offset))
        return false
      } else {
        val o = slotManager(owner(m))
        val m2 = handle.meta
        if (owner(m2) == owner(m)) {
          // Either this or o must roll back.  We choose to give precedence to
          // o, as it is the writer and is Validating.  We verify that o still
          // has a chance to commit.
          if (o._status.mightCommit) {
            forceRollback(InvalidReadCause(handle.ref, handle.offset))
            return false
          }

          // Help it release the lock.  We don't have to worry about any
          // pending wakeups because nothing has been committed.
          handle.metaCAS(m2, withRollback(m2))
        }
        // something has definitely changed, so recheck this i
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
    val commitVersion = freshCommitVersion

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
}
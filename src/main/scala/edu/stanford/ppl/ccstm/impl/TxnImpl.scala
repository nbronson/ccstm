/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TxnImpl

package edu.stanford.ppl.ccstm.impl

import edu.stanford.ppl.ccstm._


abstract class TxnImpl(failureHistory: List[Txn.RollbackCause]) extends AbstractTxn {
  import STMImpl._
  import Txn._

  //////////////// State

  // _status and _statusCAS come from StatusHolder via AbstractTxn

  private[ccstm] var _callbacks: Callbacks = null
  private var _readSet: ReadSet = null
  private var _writeBuffer: WriteBuffer = null
  private var _strongRefSet: StrongRefSet = null
  private var _slot: Slot = 0

  /** Higher wins. */
  private def priority = STMImpl.hash(this, 0)

  {
    val ctx = ThreadContext.get
    _callbacks = ctx.takeCallbacks
    _readSet = ctx.takeReadSet
    _writeBuffer = ctx.takeWriteBuffer
    _strongRefSet = ctx.takeStrongRefSet
    _slot = slotManager.assign(this, ctx.preferredSlot)
  }

  /** The read version of this transaction.  It is guaranteed that all values
   *  read by this transaction have a version number less than or equal to this
   *  value, and that any transaction whose writes conflict with this
   *  transaction will label those writes with a version number greater than
   *  this value.  The read version must never be greater than
   *  <code>globalVersion.get</code>, must never decrease, and each time it is
   *  changed the read set must be revalidated.  Lazily assigned.
   */
  private[impl] var _readVersion: Version = freshReadVersion

  /** True if all reads should be performed as writes. */
  private[ccstm] val barging: Boolean = shouldBarge(failureHistory)

  /** True if the most recent rollback was due to an explicit retry. */
  private[ccstm] val explicitRetrying: Boolean = {
    !failureHistory.isEmpty && failureHistory.head.isInstanceOf[ExplicitRetryCause]
  }

  private def shouldBarge(failureHistory: List[Txn.RollbackCause]) = {
    // barge if we have already had 2 failures since the last explicit retry
    var cur = failureHistory
    var count = 2
    while (count != 0 && !cur.isEmpty && !cur.head.isInstanceOf[ExplicitRetryCause]) {
      cur = cur.tail
      count -= 1
    }
    (count == 0)
  }

  override def toString = {
    ("Txn@" + hashCode.toHexString + "(" + status +
            ", slot=" + _slot +
            ", priority=" + priority +
            ", readSet.size=" + (if (null == _readSet) "discarded" else _readSet.size.toString) +
            ", writeBuffer.size=" + (if (null == _writeBuffer) "discarded" else _writeBuffer.size.toString) +
            ", readVersion=0x" + _readVersion.toHexString +
            (if (barging) ", barging" else "") + ")")
  }


  //////////////// Implementation

  /** On return, the read version will have been the global version at some
   *  point during the call, the read version will be &ge; minReadVersion, and
   *  all reads will have been validated against the new read version.  Throws
   *  <code>RollbackError</code> if invalid.
   */
  private def revalidate(minReadVersion: Version) {
    _readVersion = freshReadVersion(minReadVersion)
    if (!revalidateImpl()) throw RollbackError
  }

  /** Calls revalidate(version) if version &le; _readVersion. */
  private def revalidateIfRequired(version: Version) {
    if (version > _readVersion) {
      revalidate(version)
    }
  }

  /** Returns true if valid. */
  private def revalidateImpl(): Boolean = {
    var i = 0
    while (i < _readSet.maxIndex) {
      val h = _readSet.handle(i)
      if (null != h && !revalidateImpl(h, _readSet.version(i))) return false
      i += 1
    }
    return readResourcesValidate()
  }

  private def revalidateImpl(handle: Handle[_], ver: STMImpl.Version): Boolean = {
    var done = false
    while (!done) {
      val m1 = handle.meta
      if (!changing(m1) || owner(m1) == _slot) {
        if (version(m1) != ver) {
          forceRollback(InvalidReadCause(handle, "version changed"))
          return false
        }
        // okay
        done = true
      } else if (owner(m1) == NonTxnSlot) {
        // non-txn updates don't set changing unless they will install a new
        // value, so we are the only party that can yield
        forceRollback(InvalidReadCause(handle, "pending non-txn write"))
        return false
      } else {
        // Either this txn or the owning txn must roll back.  We choose to
        // give precedence to the owning txn, as it is the writer and is
        // Validating.  There's a bit of trickiness since o may not be the
        // owning transaction, it may be a new txn that reused the same
        // slot.  If the actual owning txn committed then the version
        // number will have changed, which we will detect on the next pass
        // (because we aren't incrementing i, so we will revisit this
        // entry).  If it rolled back then we don't have to roll back, so
        // looking at o to make the decision doesn't affect correctness
        // (although it might result in an unnecessary rollback).  If the
        // owner slot is the same but the changing bit is not set (or if
        // the owner txn is null) then we are definitely observing a reused
        // slot and we can avoid the spurious rollback.
        val o = slotManager.lookup(owner(m1))
        if (null != o) {
          val s = o._status
          val m2 = handle.meta
          if (changing(m2) && owner(m2) == owner(m1)) {
            if (s.mightCommit) {
              forceRollback(InvalidReadCause(handle, "pending commit"))
              return false
            }

            stealHandle(handle, m2, o)
          }
        }
      }
      // try again unless done
    }
    return true
  }


  /** After this method returns, either the current transaction will have been
   *  rolled back or <code>currentOwner</code> will allow the write resource to
   *  be acquired.
   */
  private[impl] def resolveWriteWriteConflict(currentOwner: TxnImpl, contended: AnyRef) {
    requireActive()

    // TODO: drop priority if no writes yet?

    // This test is _almost_ symmetric.  Tie goes to neither.
    if (this.priority <= currentOwner.priority) {
      if (_writeBuffer.isEmpty) {
        // We haven't acquired ownership of anything, so we can safely block
        // without obstructing anybody.
        currentOwner.awaitCompletedOrDoomed()
      } else {
        // We could block here without deadlock if
        // this.priority < currentOwner.priority, but we are obstructing so we
        // choose not to.
        forceRollback(WriteConflictCause(contended, "existing owner wins"))
        throw RollbackError
      }
    } else {
      // This will resolve the conflict regardless of whether it succeeds or
      // fails.
      currentOwner.requestRollback(WriteConflictCause(contended, "steal from existing owner"))
    }
  }

  private[ccstm] def retryImpl(): Nothing = {
    // writeBuffer entries are part of the conceptual read set
    _writeBuffer.visit(new WriteBuffer.Visitor {
      def visit(handle: Handle[_], specValue: Any): Boolean = {
        _readSet.add(handle, version(handle.meta))
        true
      }
    })

    forceRollback(new ExplicitRetryCause(_readSet.clone))
    throw RollbackError
  }

  private[ccstm] def commitImpl(): Status = {
    try {
      if (status.mustRollBack || !writeLikeResourcesPrepare()) {
        return completeRollback()
      }

      if (_writeBuffer.size == 0 && !writeResourcesPresent) {
        // read-only transactions are easy to commit, because all of the reads
        // are already guaranteed to be consistent
        if (!_statusCAS(Active, Committed)) {
          // remote requestRollback got us at the last moment
          assert(_status.isInstanceOf[RollingBack])
          _status = Rolledback(status.rollbackCause)
        }
        return _status
      }

      if (!_statusCAS(Active, Validating)) return completeRollback()

      if (!acquireLocks()) return completeRollback()

      // this is our linearization point
      val cv = freshCommitVersion(_readVersion, globalVersion.get)

      // if the reads are still valid, then they were valid at the linearization
      // point
      if (!revalidateImpl()) return completeRollback()

      // attempt to decide commit
      if (!_statusCAS(Validating, Committing)) return completeRollback()

      commitWrites(cv)
      writeResourcesPerformCommit()
      _status = Committed

      return Committed
      
    } finally {
      callAfter()

      // cleanup
      val ctx = ThreadContext.get
      ctx.put(_callbacks, _readSet, _writeBuffer, _strongRefSet, _slot)
      _callbacks = null
      _readSet = null
      _writeBuffer = null
      slotManager.release(_slot)
    }
  }

  private def completeRollback(): Status = {
    rollbackWrites()
    writeResourcesPerformRollback()
    _status = Rolledback(status.rollbackCause)

    return _status
  }

  private def rollbackWrites() {
    assert(_status.isInstanceOf[RollingBack])

    var i = _writeBuffer.size
    while (i > 0) {
      rollbackWrite(_writeBuffer.visitHandle(i))
      i -= 1
    }
  }

  private def rollbackWrite(handle: Handle[_]) {
    var m = handle.meta
    while (owner(m) == _slot) {
      // we must use CAS because there can be concurrent pendingWaiter adds
      // and concurrent "helpers" that release the lock
      if (handle.metaCAS(m, withRollback(m))) return
      m = handle.meta
    }
  }

  private def acquireLocks(): Boolean = {
    var wakeups = 0L
    var i = _writeBuffer.size
    while (i > 0) {
      if (!acquireLock(_writeBuffer.visitHandle(i))) return false
      i -= 1
    }
    return _status == Validating
  }

  private def acquireLock(handle: Handle[_]): Boolean = {
    var m = handle.meta
    if (!changing(m)) {
      // remote requestRollback might have doomed us, followed by a steal
      // of this handle, so we must verify ownership each try
      while (owner(m) == _slot) {
        if (handle.metaCAS(m, withChanging(m))) return true
        m = handle.meta
      }
      return false
    }
    return true
  }

  private def commitWrites(cv: Long) {
    if (_writeBuffer.isEmpty) {
      return
    }

    // first pass
    var wakeups = 0L
    var i = _writeBuffer.size
    while (i > 0) {
      val handle = _writeBuffer.visitHandle(i)
      val specValue = _writeBuffer.visitSpecValue(i)
      i -= 1

      // update the value
      handle.asInstanceOf[Handle[Any]].data = specValue

      // We must accumulate the pending wakeups during this pass, because
      // during the second pass we clear the PW bit on the first handle that
      // references a particular metadata.
      if (pendingWakeups(handle.meta)) {
        wakeups |= wakeupManager.prepareToTrigger(handle.ref, handle.offset)
      }
    }

    // second pass
    i = _writeBuffer.size
    while (i > 0) {
      val handle = _writeBuffer.visitHandle(i)
      i -= 1

      val m = handle.meta
      if (owner(m) == _slot) {
        // release the lock, clear the PW bit, and update the version
        handle.meta = withCommit(m, cv)
      }
    }

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

  private[ccstm] def addReferenceImpl(ptr: AnyRef) {
    _strongRefSet += ptr
  }
  

  //////////////// lock management - similar to NonTxn but we also check for remote rollback

  private def weakAwaitUnowned(handle: Handle[_], m0: Meta) {
    STMImpl.weakAwaitUnowned(handle, m0, this)
  }

  private def acquireOwnership(handle: Handle[_], m0: Meta): Meta = {
    var m = m0
    var done = false
    while (!done) {
      while (owner(m) != UnownedSlot) {
        weakAwaitUnowned(handle, m)
        m = handle.meta
      }
      val after = withOwner(m, _slot)
      if (handle.metaCAS(m, after)) {
        m = after
        done = true
      } else {
        m = handle.meta
      }
    }
    m
  }

  /** Returns 0L on failure. */
  private def tryAcquireOwnership(handle: Handle[_], m0: Meta): Meta = {
    var m = m0
    if (owner(m) != UnownedSlot) {
      0L
    } else {
      val after = withOwner(m, _slot)
      if (handle.metaCAS(m, after)) {
        after
      } else {
        0L
      }
    }
  }

  //////////////// barrier implementations
  
  def get[T](handle: Handle[T]): T = {
    if (barging) return readForWrite(handle)

    requireActive()

    var m1 = handle.meta
    if (owner(m1) == _slot) {
      // Self-owned.  This particular ref+offset might not be in the write
      // buffer, but it's definitely not in anybody else's.
      return _writeBuffer.get(handle)
    }

    var m0 = 0L
    var value: T = null.asInstanceOf[T]
    do {
      m0 = m1
      while (changing(m0)) {
        weakAwaitUnowned(handle, m0)
        m0 = handle.meta
      }
      revalidateIfRequired(version(m0))
      value = handle.data
      m1 = handle.meta
    } while (changingAndVersion(m0) != changingAndVersion(m1))

    // Stable read.  The second read of handle.meta is required for
    // opacity, and it also enables the read-only commit optimization.
    _readSet.add(handle, version(m1))
    return value
  }

  def map[T,Z](handle: Handle[T], f: T => Z): Z = {
    if (barging) return f(readForWrite(handle))

    val u = unrecordedRead(handle)
    val result = f(u.value)
    if (!u.recorded) {
      val callback = new Txn.ReadResource {
        var _latestRead = u

        def valid(t: Txn): Boolean = {
          if (_latestRead == null || _latestRead.stillValid) return true

          var m1 = handle.meta
          if (owner(m1) == _slot) {
            // We know that our original read did not come from the write
            // buffer, because !u.recorded.  That means that to redo this
            // read we should go to handle.data, which has the most recent
            // value from which we should read.
            _latestRead = null
            return (result == f(handle.data))
          }

          // reread, and see if that changes the result
          _latestRead = unrecordedRead(handle)

          return (result == f(_latestRead.value))
        }
      }

      // It is safe to skip calling callback.valid() here, because we
      // have made no calls into the txn that might have resulted in it
      // moving its virtual snapshot forward.  This means that the
      // unrecorded read that initialized u is consistent with all of the
      // reads performed so far.
      addReadResource(callback, 0, false)
    }

    return result
  }

  def unrecordedRead[T](handle: Handle[T]): UnrecordedRead[T] = {
    requireActiveOrValidating()

    var m1 = handle.meta
    var v: T = null.asInstanceOf[T]
    val rec = (if (owner(m1) == _slot) {
      v = _writeBuffer.get(handle)
      true
    } else {
      var m0 = 0L
      do {
        m0 = m1
        while (changing(m0)) {
          if (_status eq Txn.Validating) {
            // can't wait
            forceRollback(Txn.InvalidReadCause(handle, "contended unrecordedRead while validating"))
            throw RollbackError
          }
          weakAwaitUnowned(handle, m0)
          m0 = handle.meta
        }
        if (version(m0) > _readVersion) {
          if (_status eq Txn.Validating) {
            // can't wait
            forceRollback(Txn.InvalidReadCause(handle, "unrecordedRead of future value while validating"))
            throw RollbackError
          }
          revalidate(version(m0))
        }
        v = handle.data
        m1 = handle.meta
      } while (changingAndVersion(m0) != changingAndVersion(m1))
      false
    })

    new UnrecordedRead[T] {
      def context: Option[Txn] = Some(TxnImpl.this.asInstanceOf[Txn])
      def value: T = v
      def stillValid = {
        val m = handle.meta
        version(m) == version(m1) && (!changing(m) || owner(m) == _slot)
      }
      def recorded = rec
    }
  }

  def releasableRead[T](handle: Handle[T]): ReleasableRead[T] = {
    requireActive
    // this code relies on the implementation details of get() and ReadSet
    val before = _readSet.size
    val v = get(handle)
    (_readSet.size - before) match {
      case 0 => {
        // read was satisfied from write buffer, can't release
        new ReleasableRead[T] {
          def context: Option[Txn] = Some(TxnImpl.this.asInstanceOf[Txn])
          def value: T = v
          def release() {}
        }
      }
      case 1 => {
        // single new addition to read set
        assert(handle eq _readSet.handle(before))
        new ReleasableRead[T] {
          def context: Option[Txn] = Some(TxnImpl.this.asInstanceOf[Txn])
          def value: T = v
          def release(): Unit = if (null != _readSet) _readSet.release(before)
        }
      }
      case _ => {
        throw new Error("logic error in program")
      }
    }
  }

  def set[T](handle: Handle[T], v: T) {
    requireActive()

    val m0 = handle.meta
    if (owner(m0) == _slot) {
      // Self-owned.  This particular ref+offset might not be in the write
      // buffer, but it's definitely not in anybody else's.
      _writeBuffer.put(handle, v)
      return
    }

    val m = acquireOwnership(handle, m0)

    _writeBuffer.put(handle, v)

    // This might not be a blind write, because meta might be shared with other
    // values that are subsequently read by the transaction.  We don't need to
    // record a read set entry, however, because nobody can modify it after we
    // grab ownership.  This means it suffices to check against _readVersion.
    // We must put something in the buffer before calling revalidate in case we
    // roll back, so that the ownership gets released.

    revalidateIfRequired(version(m))
  }
  
  def getAndSet[T](handle: Handle[T], v: T): T = {
    requireActive()

    val m0 = handle.meta
    if (owner(m0) == _slot) {
      val z = _writeBuffer.get(handle)
      _writeBuffer.put(handle, v)
      return z
    }

    val m = acquireOwnership(handle, m0)
    _writeBuffer.put(handle, v)

    revalidateIfRequired(version(m))

    // definitely no value previously in the write buffer
    handle.data
  }

  def tryWrite[T](handle: Handle[T], v: T): Boolean = {
    requireActive()

    val m0 = handle.meta
    if (owner(m0) == _slot) {
      _writeBuffer.put(handle, v)
      return true
    }

    val m = tryAcquireOwnership(handle, m0)
    if (m == 0L) return false

    _writeBuffer.put(handle, v)

    revalidateIfRequired(version(m))
    return true
  }

  def readForWrite[T](handle: Handle[T]): T = {
    requireActive()

    val m0 = handle.meta
    if (owner(m0) == _slot) {
      return _writeBuffer.allocatingGet(handle)
    }

    val m = acquireOwnership(handle, m0)

    val v = _writeBuffer.allocatingGet(handle)

    revalidateIfRequired(version(m))

    return v
  }

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

  def getAndTransform[T](handle: Handle[T], f: T => T): T = {
    requireActive()

    val m0 = handle.meta
    if (owner(m0) == _slot) {
      return _writeBuffer.getAndTransform(handle, f)
    }

    val m = acquireOwnership(handle, m0)

    val v0 = _writeBuffer.getAndTransform(handle, f)

    revalidateIfRequired(version(m))

    return v0
  }

  def tryTransform[T](handle: Handle[T], f: T => T): Boolean = {
    requireActive()

    val m0 = handle.meta
    if (owner(m0) == _slot) {
      _writeBuffer.getAndTransform(handle, f)
      return true
    }

    val m = tryAcquireOwnership(handle, m0)
    if (m == 0L) return false

    _writeBuffer.getAndTransform(handle, f)

    revalidateIfRequired(version(m))

    return true
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
      if (!u.stillValid && !pf.isDefinedAt(v)) {
        // value changed after unrecordedRead
        false
      } else {
        // still defined, do the actual getAndTransform
        set(handle, pf(v))
        true
      }
    }
  }

}

/* CCSTM - (c) 2009 Stanford University - PPL */

// TxnImpl

package edu.stanford.ppl.ccstm.impl


abstract class TxnImpl(failureHistory: List[Txn.RollbackCause]) extends AbstractTxn {
  import STMImpl._
  import Txn._

  //////////////// State

  // _status and _statusCAS come from StatusHolder via AbstractTxn

  private var _readSet: ReadSet = null
  private var _writeBuffer: WriteBuffer = null
  private var _slot: Slot = 0

  /** Higher wins. */
  private def priority = STMImpl.hash(this, 0)

  {
    val ctx = ThreadContext.get
    _readSet = ctx.takeReadSet
    _writeBuffer = ctx.takeWriteBuffer
    _slot = slotManager.assign(this, ctx.preferredSlot)
  }

  /** The <code>VersionTrap</code> associated with <code>_readVersion</code>,
   *  used to make sure that any ephemeral handles created after this
   *  transaction's virtual read snapshot will survive until the snapshot's
   *  validation.
   */
  private var _readVersionTrap: VersionTrap = freshReadVersion

  /** The read version of this transaction.  It is guaranteed that all values
   *  read by this transaction have a version number less than or equal to this
   *  value, and that any transaction whose writes conflict with this
   *  transaction will label those writes with a version number greater than
   *  this value.  The read version must never be greater than
   *  <code>globalVersion.get</code>, must never decrease, and each time it is
   *  changed the read set must be revalidated.  Lazily assigned.
   */
  private[impl] var _readVersion: Version = _readVersionTrap.version

  /** True if all reads should be performed as writes. */
  private[impl] val barging: Boolean = shouldBarge(failureHistory)

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
            ", readSet.size=" + _readSet.size +
            ", writeBuffer.size=" + _writeBuffer.size +
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
    _readVersionTrap = freshReadVersion(minReadVersion)
    _readVersion = _readVersionTrap.version
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
    _readSet.visit(new ReadSet.Visitor {
      def visit(handle: Handle[_], ver: STMImpl.Version): Boolean = {
        var done = false
        while (!done) {
          val m1 = handle.meta
          if (!changing(m1) || owner(m1) == _slot) {
            if (version(m1) != ver) {
              forceRollback(InvalidReadCause(handle))
              return false
            }
            // okay
            done = true
          } else if (owner(m1) == NonTxnSlot) {
            // non-txn updates don't set changing unless they will install a new
            // value, so we are the only party that can yield
            forceRollback(InvalidReadCause(handle))
            return false
          } else {
            // Either this txn or the owning txn must roll back.  We choose to give
            // precedence to the owning txn, as it is the writer and is
            // Validating.  There's a bit of trickiness since o may not be the
            // owning transaction, it may be a new txn that reused the same slot.
            // If the actual owning txn committed then the version number will
            // have changed, which we will detect on the next pass because we
            // aren't incrementing i.  If it rolled back then we don't have to.
            // If the ownership is the same but the changing bit is not set (or if
            // the owner txn is null) then we must have observed the race and we
            // don't roll back here.
            val o = slotManager.lookup(owner(m1))
            if (o != null) {
              val s = o._status
              val m2 = handle.meta
              if (changing(m2) && owner(m2) == owner(m1)) {
                if (s.mightCommit) {
                  forceRollback(InvalidReadCause(handle))
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
    }) && readResourcesValidate()
  }

  /** After this method returns, either the current transaction will have been
   *  rolled back or <code>currentOwner</code> will allow the write resource to
   *  be acquired.
   */
  private[impl] def resolveWriteWriteConflict(currentOwner: TxnImpl, contended: AnyRef) {
    requireActive

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

  private[ccstm] def retryImpl() {
    if (_readSet.isEmpty && _writeBuffer.isEmpty) {
      throw new IllegalStateException("retry doesn't make sense with empty read set")
    }

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
      val cv = freshCommitVersion(globalVersion.get)

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
      // cleanup
      val ctx = ThreadContext.get
      ctx.put(_readSet, _writeBuffer, _slot)
      _readSet = null
      _writeBuffer = null
      slotManager.release(_slot)

      // Clear out references from this Txn, to aid GC in case someone keeps a
      // Txn reference around.  The trap is especially important because it
      // will pin all future VersionTrap-s, and therefore pin all future 
      // TxnImpl-s.
      _readVersionTrap = null

      callAfter()
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

    _writeBuffer.visit(new WriteBuffer.Visitor {
      def visit(handle: Handle[_], specValue: Any): Boolean = {
        var m = handle.meta
        while (owner(m) == _slot) {
          // we must use CAS because there can be concurrent pendingWaiter adds
          // and concurrent "helpers" that release the lock
          if (handle.metaCAS(m, withRollback(m))) return true
          m = handle.meta
        }
        return true
      }
    })
  }

  private def acquireLocks(): Boolean = {
    _writeBuffer.visit(new WriteBuffer.Visitor {
      def visit(handle: Handle[_], specValue: Any): Boolean = {
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
    }) && (_status == Validating)    
  }

  private def commitWrites(cv: Long) {
    if (_writeBuffer.isEmpty) {
      return
    }

    // first pass
    var wakeups = 0L
    _writeBuffer.visit(new WriteBuffer.Visitor {
      def visit(handle: Handle[_], specValue: Any): Boolean = {
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
    _writeBuffer.visit(new WriteBuffer.Visitor {
      def visit(handle: Handle[_], specValue: Any): Boolean = {
        val m = handle.meta
        if (owner(m) == _slot) {
          // release the lock, clear the PW bit, and update the version
          handle.meta = withCommit(m, cv)
        }
        true
      }
    })

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

  //////////////// barrier implementations
  
  def get[T](handle: Handle[T]): T = {
    if (barging) return readForWrite(handle)

    requireActive

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
      if (owner(m0) == FrozenSlot) {
        // version can't change, just checking against _readVersion is enough
        return value
      }
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

    return result
  }

  def unrecordedRead[T](handle: Handle[T]): UnrecordedRead[T] = {
    requireActive

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
          weakAwaitUnowned(handle, m0)
          m0 = handle.meta
        }
        revalidateIfRequired(version(m0))
        v = handle.data
        m1 = handle.meta
      } while (changingAndVersion(m0) != changingAndVersion(m1))
      false
    })

    // Everything above this line is the same as for get().  Is there a way to
    // share some of that code without incurring boxing/unboxing penalties?

    return new UnrecordedRead[T] {
      def context: Option[Txn] = Some(TxnImpl.this.asInstanceOf[Txn])
      def value: T = v
      def stillValid = changingAndVersion(handle.meta) == changingAndVersion(m1)
      def recorded = rec
    }
  }

  def set[T](handle: Handle[T], v: T) {
    requireActive

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
    // add a read set entry, however, because nobody can modify it after we
    // grab ownership.  This means it suffices to check against _readVersion.
    // We must put something in the buffer before calling revalidate in case we
    // roll back, so that the ownership gets released.

    revalidateIfRequired(version(m))
  }
  
  def tryWrite[T](handle: Handle[T], v: T): Boolean = {
    requireActive

    val m0 = handle.meta
    owner(m0) match {
      case UnownedSlot => {
        if (handle.metaCAS(m0, withOwner(m0, _slot))) {
          _writeBuffer.put(handle, v)
          revalidateIfRequired(version(m0))
          true
        } else {
          false
        }
      }
      case FrozenSlot => {
        throw new IllegalStateException("frozen")
      }
      case s if (s == _slot) => {
        // Self-owned.  This particular ref+offset might not be in the write
        // buffer, but it's definitely not in anybody else's.
        _writeBuffer.put(handle, v)
        true
      }
      case _ => {
        false
      }
    }
  }

  def freeze(handle: Handle[_]) {} // TODO implement

  def readForWrite[T](handle: Handle[T]): T = {
    requireActive

    val m0 = handle.meta
    if (owner(m0) == FrozenSlot) {
      // revert back to normal read
      revalidateIfRequired(version(m0))
      return handle.data
    }

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
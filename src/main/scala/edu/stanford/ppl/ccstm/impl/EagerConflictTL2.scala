///* CCSTM - (c) 2009 Stanford University - PPL */
//
//// EagerConflictTL2.scala
//
//package edu.stanford.ppl.ccstm.impl
//
//import edu.stanford.ppl.ccstm.Ref
//import edu.stanford.ppl.ccstm.UnrecordedRead
//import java.util.concurrent.atomic.{AtomicReference, AtomicLongFieldUpdater, AtomicLong}
//
//
//private[ccstm] object EagerConflictTL2 extends GV6 {
//
//
//  private[impl] class ReadSet(val readCount: Int,
//                               val readAccessors: Array[MetadataHandle[Long]],
//                               val readVersions: Array[Long]) {
//    override def toString = {
//      "ReadSet(size=" + readCount + ")"
//    }
//  }
//
//  private[impl] def awaitRetry(explicitRetries: Txn.ExplicitRetryCause*) {
//
//  //////////////// Internal per-slot blocking
//
//  private[impl] def awaitNonTxnUnlock(ref: AnyRef, offset: Int, acc: MetadataHandle[VLock]) {
//    // spin a bit
//    var spins = 0
//    while (spins < SpinCount + YieldCount) {
//      spins += 1
//      if (spins > SpinCount) Thread.`yield`
//
//      val v = acc.metadata
//      if (owner(v) == UnownedSlot) return
//    }
//
//    // Try to put ourself to sleep.  This requires that we register a pending
//    // wakeup on this acc, which we can only do if the version number is not
//    // equal to ChangingVersion.
//    val e = wakeupManager.subscribe
//    e.addSource(ref, offset)
//
//    spins = 0
//    while (true) {
//      if (e.triggered) return
//      val v = acc.metadata
//      if (owner(v) == UnownedSlot) return
//      if (pendingWakeups(v) ||
//              (version(v) != ChangingVersion && acc.metadataCAS(v, withPendingWakeups(v)))) {
//        e.await
//        return
//      }
//      spins += 1
//      if (spins > SpinCount) Thread.`yield`
//    }
//  }
//}
//
///** @see edu.stanford.ppl.ccstm.EagerConflictTL2
// *
// *  @author Nathan Bronson
// */
//private[ccstm] abstract class EagerConflictTL2Txn(failureHistory: List[Txn.RollbackCause]
//        ) extends AbstractTxn with ReadSet with TxnWriteBuffer {
//
//  import EagerConflictTL2._
//  import Txn._
//
//  //////////////// State
//
//  // _status and _statusCAS come from StatusHolder via AbstractTxn
//
//  // _readCount, _readAccessors, and _readVersions come from TxnReadSet
//
//  // writeBufferSize, writeBufferGet, writeBufferPut, and writeBufferVisit come
//  // from TxnWriteBuffer
//
//  /** The read version of this transaction.  It is guaranteed that all values
//   *  read by this transaction have a version number less than or equal to this
//   *  value, and that any transaction whose writes conflict with this
//   *  transaction will label those writes with a version number greater than
//   *  this value.  The read version must never be greater than
//   *  <code>globalVersion.get</code>, must never decrease, and each time it is
//   *  changed the read set must be revalidated.
//   */
//  private[impl] var _readVersion: VLock = globalVersion.get
//
//  /** The version number with which this transaction's writes are labelled, or
//   *  zero if this transaction has not yet decided on commit or is in the
//   *  <code>Committing</code> state but has not yet had time to query the
//   *  global version.
//   *  <p>
//   *  Other transactions that encounter a TxnLocked owned by this transaction
//   *  must wait for _commitVersion to be non-zero if they observe our status as
//   *  Committing, because linearize after us.
//   */
//  @volatile private[impl] var _commitVersion: VLock = 0
//
//  /** True if all reads should be performed as writes. */
//  private[impl] val barging: Boolean = {
//    // barge if we have already had 3 failures since the last explicit retry
//    var cur = failureHistory
//    var count = 0
//    while (count < 3 && !cur.isEmpty && !cur.head.isInstanceOf[ExplicitRetryCause]) {
//      cur = cur.tail
//      count += 1
//    }
//    (count == 3)
//  }
//
//
//  override def toString = {
//    ("Txn@" + Integer.toHexString(hashCode) + "(" + status +
//            ", readCount=" + _readCount +
//            ", writeBufferSize=" + writeBufferSize +
//            ", readVersion=" + _readVersion +
//            ", commitVersion=" + _commitVersion +
//            (if (barging) ", barging" else "") + ")")
//  }
//
//
//  //////////////// Implementation
//
//  /** On return, the read version will have been the global version at some
//   *  point during the call, the read version will be &ge; minReadVersion, and
//   *  all reads will have been validated against the new read version.  Throws
//   *  <code>RollbackError</code> if invalid.
//   */
//  private[impl] def revalidate(minReadVersion: VLock) {
//    _readVersion = freshReadVersion(minReadVersion)
//    if (!revalidateImpl()) throw RollbackError
//  }
//
//  /** Returns true if valid. */
//  private[impl] def revalidateImpl(): Boolean = {
//    var i = 0
//    while (i < _readCount) {
//      val expected = _readVersion(i)
//      val v = _readAccessors(i).metadata
//      val cur = r.data
//      if (!cur.txnStillValid(this, r._readSnapshot.version)) {
//        forceRollback(InvalidReadCause(r, null))
//        return false
//      }
//      i += 1
//    }
//
//    // STM managed reads were okay, what about explicitly registered read-only
//    // resources?
//    return readResourcesValidate()
//  }
//
//  /** Returns true if the caller should wait for the txn to complete, false if
//   *  the reader should proceed with the old value, hoping to commit before
//   *  <code>currentOwner</code>.  May also choose to doom either txn, in which
//   *  case <code>RollbackError</code> will be thrown.  Will never return false
//   *  if currentOwner.status == Validating.
//   */
//  private[impl] def shouldWaitForRead(currentOwner: EagerConflictTL2Txn): Boolean = {
//    // TODO: something more sophisticated?
//
//    // This method must take care to avoid deadlock cycles, via the other txn
//    // waiting on a read, or a block inside resolveWriteWriteConflict.  We
//    // choose to never wait on a txn that is still performing reads or writes,
//    // which makes is simple.
//
//    currentOwner._status == Validating
//  }
//
//  /** After this method returns, either the current transaction will have been
//   *  rolled back or <code>currentOwner</code> will allow the write resource to
//   *  be acquired.
//   */
//  private[impl] def resolveWriteWriteConflict(currentOwner: EagerConflictTL2Txn, contended: AnyRef) {
//    val cause = WriteConflictCause(contended, null)
//
//    // this test is _almost_ symmetric
//    if (this.hashCode <= currentOwner.hashCode) {
//      forceRollback(cause)
//      throw RollbackError
//    } else {
//      currentOwner.requestRollback(cause)
//    }
//  }
//
//  private[ccstm] def retryImpl() {
//    if (_readCount == 0) {
//      throw new IllegalStateException("retry doesn't make sense with empty read set")
//    }
//
//    forceRollback(new ExplicitRetryCause(new ReadSet(_readCount, _reads)))
//    throw RollbackError
//  }
//
//  private[ccstm] def commitImpl(): Status = {
//    assert(status == Active || status.isInstanceOf[RollingBack])
//
//    if (status.mustRollBack || !writeLikeResourcesPrepare()) {
//      return completeRollback()
//    }
//
//    if (_writeCount == 0 && !writeResourcesPresent) {
//      // read-only transactions are easy to commit, because all of the reads
//      // are already guaranteed to be consistent
//      if (!_statusCAS(Active, Committed)) {
//        // remote requestRollback()
//        assert(_status.isInstanceOf[RollingBack])
//        _status = Rolledback(status.rollbackCause)
//      }
//      callAfter()
//      return _status
//    }
//
//    if (!_statusCAS(Active, Validating)) return completeRollback()
//
//    // this is our linearization point
//    _commitVersion = freshCommitVersion
//
//    // if the reads are still valid, then they were valid at the linearization
//    // point
//    if (!revalidateImpl()) return completeRollback()
//
//    // attempt to decide commit
//    if (!_statusCAS(Validating, Committing)) return completeRollback()
//
//    commitWrites()
//    writeResourcesPerformCommit()
//    _status = Committed
//    callAfter()
//
//    return Committed
//  }
//
//  private def completeRollback(): Status = {
//    rollbackWrites()
//    writeResourcesPerformRollback()
//    _status = Rolledback(status.rollbackCause)
//    callAfter()
//
//    return _status
//  }
//
//  private def rollbackWrites() {
//    assert(_status.isInstanceOf[RollingBack])
//    var i = 0
//    while (i < _writeCount) {
//      _writes(i).rollback(this)
//      i += 1
//    }
//  }
//
//  private def commitWrites() {
//    val cv = _commitVersion
//    var i = 0
//    var mask: WakeupMask = 0
//    while (i < _writeCount) {
//      mask |= _writes(i).commit(cv)
//      i += 1
//    }
//    triggerWakeups(mask)
//  }
//
//  private[ccstm] def requestRollbackImpl(cause: RollbackCause): Boolean = {
//    while (true) {
//      val s = _status
//      if (s.mustCommit) {
//        return false
//      } else if (s.mustRollBack) {
//        return true
//      } else {
//        assert(s == Active || s == Validating)
//        if (_statusCAS(s, RollingBack(cause))) return true
//      }
//    }
//    throw new Error("unreachable")
//  }
//
//  private[ccstm] def explicitlyValidateReadsImpl() {
//    revalidate(0)
//  }
//}
//
//private[ccstm] abstract class EagerConflictTL2TxnAccessor[T] extends Ref.Bound[T] {
//  import EagerConflictTL2._
//
//  //////////////// Abstract methods
//
//  def data: WW[T]
//  def data_=(v: WW[T])
//  def dataCAS(before: WW[T], after: WW[T]): Boolean
//  def txn: EagerConflictTL2Txn
//
//  //////////////// Implementation
//
//  private[impl] var _readSnapshot: WW[T] = null
//
//  def context: Option[Txn] = Some(txn.asInstanceOf[Txn])
//
//  def get: T = {
//    val t = txn
//
//    t.requireActive
//
//    val w0 = data
//    if (w0.isLockedBy(t)) {
//      w0.asInstanceOf[TxnLocked[T]].specValue
//    } else {
//      readImpl(t, w0, false).value
//    }
//  }
//
//  def map[Z](f: T => Z): Z = {
//    val u = unrecordedRead
//    val result = f(u.value)
//    if (!u.recorded) {
//      val callback = new Txn.ReadResource {
//        var _latestRead = u
//
//        def valid(t: Txn) = {
//          if (!_latestRead.stillValid) {
//            // reread, and see if that changes the result
//            _latestRead = unrecordedRead
//            val reapply = f(_latestRead.value)
//            result == reapply
//          } else {
//            true
//          }
//        }
//      }
//
//      // It is safe to skip calling callback.valid() here, because we
//      // have made no calls into the txn that might have resulted in it
//      // moving its virtual snapshot forward.  This means that the
//      // unrecorded read that initialized u is consistent with all of the
//      // reads performed so far.
//      txn.addReadResource(callback, 0, false)
//    }
//
//    result
//  }
//
//  // does not check for read-after-write
//  private[impl] def readImpl(t: EagerConflictTL2Txn, w0: WW[T], unrecorded: Boolean): WW[T] = {
//    var w: WW[T] = w0
//
//    if (_readSnapshot != null) {
//      // Repeat previous read that used this accessor.  This helps performance
//      // when using the bind(txn) API.
//      return _readSnapshot
//    }
//
//    // is this txn desperate?
//    if (t.barging) {
//      return readForWriteImpl(t, w0, true)
//    }
//
//    var okay = false
//    do {
//      w match {
//        case u: Unlocked[_] => {
//          // No waiting necessary
//          okay = true
//        }
//        case nl: NonTxnLocked[_] => {
//          // NonTxnLocked is only used by non-transactional transform-like
//          // operations that fail to succeed with a weakCompareAndSet on their
//          // first try.  Although it would be correct to try to use the old
//          // value and see if we can commit first, it seems unlikely that this
//          // will be profitable in practice (this would also require changing
//          // the non-txn code).
//          w = awaitNonTxnUnlock(() => data, nl)
//        }
//        case tl: TxnLocked[_] => {
//          // We checked for read-after-write earlier, so this is a true
//          // conflict.  We can either wait, or we can use the old value and
//          // hope we commit before them.  To avoid deadlock cycles, we impose a
//          // partial ordering on transactions and only wait for those that are
//          // less than the current one.  shouldWaitForRead will honor the
//          // _writesPreventRead flag.
//          if (t.shouldWaitForRead(tl.owner)) {
//            tl.owner.awaitDecided
//            w = data
//          } else {
//            // If commit is inevitable, we can (should) use the new value.  If
//            // commit is not inevitable, then we are allowed to use the old
//            // value (owner is not yet committing) or we must use the old value
//            // (owner has decided to roll back).
//            if (tl.owner.status.mustCommit) {
//              w = new Unlocked(tl.specValue, tl.owner._commitVersion)
//            }
//            okay = true
//          }
//        }
//      }
//    } while (!okay)
//
//    // attempt to read at w.version
//    if (w.version > t._readVersion) t.revalidate(w.version)
//
//    // read is stable and consistent
//
//    if (!unrecorded) {
//      _readSnapshot = w
//      t.addToReadSet(this)
//    }
//
//    return w
//  }
//
//  def await(pred: T => Boolean) {
//    if (!pred(get)) txn.retry
//  }
//
//  def unrecordedRead: UnrecordedRead[T] = {
//    val t = txn
//    t.requireActive
//
//    val w0 = data
//    if (w0.isLockedBy(t)) {
//      return new UnrecordedRead[T] {
//        def context = EagerConflictTL2TxnAccessor.this.context
//        val value = w0.asInstanceOf[TxnLocked[T]].specValue
//        def recorded = true
//        def stillValid = (w0 eq data.asInstanceOf[AnyRef])
//      }
//    } else {
//      val w = readImpl(t, w0, false)
//      return new UnrecordedRead[T] {
//        def context = EagerConflictTL2TxnAccessor.this.context
//        def value = w.value
//        val recorded = (_readSnapshot != null)
//        def stillValid = data.txnStillValid(t, w.version)
//      }
//    }
//  }
//
//  def set(v: T) {
//    readForWriteImpl(false).specValue = v
//  }
//
//  private[impl] def readForWriteImpl(addToRS: Boolean): TxnLocked[T] = {
//    val t = txn
//    t.requireActive
//
//    val w0 = data
//    if (w0.isLockedBy(t)) {
//      // easy
//      w0.asInstanceOf[TxnLocked[T]]
//    } else {
//      readForWriteImpl(t, w0, addToRS)
//    }
//  }
//
//  private[impl] def readForWriteImpl(t: EagerConflictTL2Txn, w0: WW[T], addToRS: Boolean): TxnLocked[T] = {
//    var w = w0
//    var result: TxnLocked[T] = null
//    do {
//      if (w.isAcquirable) {
//        // Locked by a doomed txn, or unlocked
//
//        // Validate previous version
//        if (w.version > t._readVersion) t.revalidate(w.version)
//
//        // If we are acquiring an unlocked slot, then w.unlocked is just the
//        // slot itself.  If we are stealing a locked slot from a doomed txn,
//        // then w.unlocked is the most recent unlocked version of the slot.
//        val after = new TxnLocked(w.unlocked, w.value, t)
//        if (dataCAS(w, after)) {
//          // success!  we don't need to add to the read set for validation
//          // purposes, but we do need the snapshot value if we later want to do
//          // a conditional retry
//          if (addToRS && _readSnapshot == null) {
//            _readSnapshot = after.unlocked
//            t.addToReadSet(this)
//          }
//          t.addToWriteSet(this)
//          result = after
//        } else {
//          // failed, try again with new value
//          w = data
//        }
//      } else {
//        // Locked, we must wait
//        w match {
//          case nl: NonTxnLocked[_] => {
//            // Nothing to do but wait.
//            w = awaitNonTxnUnlock(() => data, nl)
//          }
//          case tl: TxnLocked[_] => {
//            // Since we can't buffer multiple speculative versions for this
//            // data type in this STM, write-write conflicts must be handled
//            // immediately.  We can: doom the other txn and steal the lock,
//            // block the current txn, or roll back the current txn.  If the
//            // contention manager in resolveWWConflict wants to roll back the
//            // current txn it will throw RollbackError itself.  If it
//            // wants to doom tl.owner it will also do that directly.  Once the
//            // other transaction has decided, we can proceed.
//            t.resolveWriteWriteConflict(tl.owner, this)
//            tl.owner.awaitCompletedOrDoomed()
//            w = data
//          }
//          case u: Unlocked[_] => throw new Error("shouldn't get here")
//        }
//      }
//    } while (result == null)
//
//    return result
//  }
//
//  def tryWrite(v: T): Boolean = {
//    val t = txn
//    val w0 = data
//
//    t.requireActive
//
//    if (w0.isLockedBy(t)) {
//      // easy
//      w0.asInstanceOf[TxnLocked[T]].specValue = v
//      return true
//    }
//
//    if (!w0.isAcquirable) {
//      // fail without blocking
//      return false
//    }
//
//    // Unlocked, or locked by a doomed txn
//
//    // Validate previous version
//    if (w0.version > t._readVersion) t.revalidate(w0.version)
//
//    // If we are acquiring an unlocked slot, then w.unlocked is just the
//    // slot itself.  If we are stealing a locked slot from a doomed txn,
//    // then w.unlocked is the most recent unlocked version of the slot.
//    val after = new TxnLocked(w0.unlocked, v, t)
//    if (dataCAS(w0, after)) {
//      // success!
//      t.addToWriteSet(this)
//      return true
//    } else {
//      // rollbackCause, don't retry
//      return false
//    }
//  }
//
//  def freeze() {
//    // TODO: implement
//    // TODO: implement
//    // TODO: implement
//  }
//
//  def readForWrite: T = {
//    readForWriteImpl(true).specValue
//  }
//
//  def compareAndSet(before: T, after: T): Boolean = {
//    transformIfDefined(new PartialFunction[T,T] {
//      def isDefinedAt(v: T): Boolean = before == v
//      def apply(v: T): T = after
//    })
//  }
//
//  def compareAndSetIdentity[A <: T with AnyRef](before: A, after: T): Boolean = {
//    transformIfDefined(new PartialFunction[T,T] {
//      def isDefinedAt(v: T): Boolean = (before eq v.asInstanceOf[AnyRef])
//      def apply(v: T): T = after
//    })
//  }
//
//  def weakCompareAndSet(before: T, after: T): Boolean = {
//    compareAndSet(before, after)
//  }
//
//  def weakCompareAndSetIdentity[A <: T with AnyRef](before: A, after: T): Boolean = {
//    compareAndSetIdentity(before, after)
//  }
//
//  def transform(f: T => T) {
//    val tl = readForWriteImpl(true)
//    tl.specValue = f(tl.specValue)
//  }
//
//  def transformIfDefined(pf: PartialFunction[T,T]): Boolean = {
//    val u = unrecordedRead
//    if (!pf.isDefinedAt(u.value)) {
//      // make sure it stays undefined
//      if (!u.recorded) {
//        val callback = new Txn.ReadResource {
//          var _latestRead = u
//
//          def valid(t: Txn) = {
//            if (!_latestRead.stillValid) {
//              // if defined after reread then return false==invalid
//              _latestRead = unrecordedRead
//              !pf.isDefinedAt(_latestRead.value)
//            } else {
//              true
//            }
//          }
//        }
//        txn.addReadResource(callback, 0, false)
//      }
//      false
//    } else {
//      val tl = readForWriteImpl(true)
//      if (!pf.isDefinedAt(tl.specValue)) {
//        // value changed after unrecordedRead
//        false
//      } else {
//        // still defined, do the actual transform
//        tl.specValue = pf(tl.specValue)
//        true
//      }
//    }
//  }
//
//  //////////////// Local use only
//
//  private[impl] def commit(cv: VLock): WakeupMask = {
//    // we don't need to use CAS, because stealing can only occur from doomed
//    // transactions
//    val before = data.asInstanceOf[TxnLocked[T]]
//    data = before.committed(cv)
//    before.unlocked.pendingWakeups
//  }
//
//  private[impl] def rollback(failingTxn: EagerConflictTL2Txn) {
//    // we must use CAS, to account for stealing, but we don't need to retry
//    // because we can only fail if there was a thief
//    data match {
//      case tl: TxnLocked[_] => if (tl.owner == failingTxn) dataCAS(tl, tl.unlocked)
//      case _ => {}
//    }
//  }
//}
//
//private[ccstm] abstract class EagerConflictTL2NonTxnAccessor[T] extends Ref.Bound[T] {
//  import EagerConflictTL2._
//
//  //////////////// Abstract methods
//
//  def metadata: Long
//  def metadata_=(m: Long)
//  def metadataCAS(before: Long, after: Long): Boolean
//  def data: T
//  def data_=(v: T)
//
//  //////////////// Implementation
//
//  def context: Option[Txn] = None
//
//  def get: T = data.nonTxnRead
//
//  def map[Z](f: T => Z) = f(get)
//
//  def await(pred: T => Boolean) {
//    val w0 = data
//    if (!pred(w0.nonTxnRead)) awaitImpl(pred, w0)
//  }
//
//  private def awaitImpl(pred: T => Boolean, w0: WW[T]) {
//    // spin a bit
//    var w = w0
//    var spins = 0
//    while (spins < SpinCount + YieldCount) {
//      spins += 1
//      if (spins > SpinCount) Thread.`yield`
//
//      val w1 = data
//      if (!(w1 eq w) && pred(w1.nonTxnRead)) return
//      w = w1
//    }
//
//    // spin failed, put ourself to sleep
//    val h = Thread.currentThread.hashCode
//    while (true) {
//      val w1 = data
//      if (!(w1 eq w) && pred(w1.nonTxnRead)) return
//      w = w1
//      w.unlocked.addPendingWakeup(h)
//      val e = subscribeToWakeup(h)
//      if (w eq data) e.await
//    }
//  }
//
//  def unrecordedRead: UnrecordedRead[T] = {
//    new UnrecordedRead[T] {
//      private val _snapshot = data.nonTxnSnapshot
//
//      def context = None
//      def value: T = _snapshot.value
//      def stillValid: Boolean = data.nonTxnStillValid(_snapshot.version)
//      def recorded: Boolean = false
//    }
//  }
//
//  private def awaitUnlock: Unlocked[T] = {
//    var d = data
//    var result: Unlocked[T] = null
//    while (result == null) {
//      d match {
//        case u: Unlocked[_] => {
//          result = u.asInstanceOf[Unlocked[T]]
//        }
//        case nl: NonTxnLocked[_] => {
//          d = awaitNonTxnUnlock(() => data, nl)
//        }
//        case tl: TxnLocked[_] => {
//          tl.owner.awaitCompletedOrDoomed
//          d = data
//        }
//      }
//    }
//    result
//  }
//
//  private def acquireLock: NonTxnLocked[T] = {
//    var before: Unlocked[T] = null
//    var after: NonTxnLocked[T] = null
//    do {
//      before = awaitUnlock
//      after = new NonTxnLocked(before)
//    } while (!dataCAS(before, after))
//    after
//  }
//
//  private def releaseLock(before: NonTxnLocked[T], after: Unlocked[T]) = {
//    data = after
//    triggerWakeups(before.unlocked.pendingWakeups)
//  }
//
//  def set(v: T) {
//    var u: Unlocked[T] = null
//    do {
//      u = awaitUnlock
//    } while (!dataCAS(u, new Unlocked(v, nonTxnWriteVersion(u.version))))
//    triggerWakeups(u.pendingWakeups)
//  }
//
//  def tryWrite(v: T): Boolean = {
//    val w = data
//    if (w.isInstanceOf[Unlocked[_]] && dataCAS(w, new Unlocked(v, nonTxnWriteVersion(w.version)))) {
//      triggerWakeups(w.asInstanceOf[Unlocked[T]].pendingWakeups)
//      true
//    } else {
//      false
//    }
//  }
//
//  def freeze() {
//    // TODO: implement
//    // TODO: implement
//    // TODO: implement
//  }
//
//  def readForWrite: T = {
//    // no sensible way to grab a write-lock in a non-txn context, just fall
//    // back to a normal read
//    get
//  }
//
//  def compareAndSet(before: T, after: T): Boolean = {
//    var u: Unlocked[T] = null
//    do {
//      u = awaitUnlock
//      if (!(before == u.value)) return false
//    } while (!dataCAS(u, new Unlocked(after, nonTxnWriteVersion(u.version))))
//    triggerWakeups(u.pendingWakeups)
//    return true
//  }
//
//  def compareAndSetIdentity[A <: T with AnyRef](before: A, after: T): Boolean = {
//    var u: Unlocked[T] = null
//    do {
//      u = awaitUnlock
//      if (!(before eq u.value.asInstanceOf[AnyRef])) return false
//    } while (!dataCAS(u, new Unlocked(after, nonTxnWriteVersion(u.version))))
//    triggerWakeups(u.pendingWakeups)
//    return true
//  }
//
//  def weakCompareAndSet(before: T, after: T): Boolean = {
//    data match {
//      case u: Unlocked[_] =>
//        if (before == u.value && dataCAS(u, new Unlocked(after, nonTxnWriteVersion(u.version)))) {
//          triggerWakeups(u.pendingWakeups)
//          true
//        } else {
//          false
//        }
//      case _ =>
//        false
//    }
//  }
//
//  def weakCompareAndSetIdentity[A <: T with AnyRef](before: A, after: T): Boolean = {
//    data match {
//      case u: Unlocked[_] =>
//        if ((before eq u.value.asInstanceOf[AnyRef]) && dataCAS(u, new Unlocked(after, nonTxnWriteVersion(u.version)))) {
//          triggerWakeups(u.pendingWakeups)
//          true
//        } else {
//          false
//        }
//      case _ =>
//        false
//    }
//  }
//
//  def transform(f: T => T) {
//    val v = get
//    if (!weakCompareAndSet(v, f(v))) lockedTransform(f)
//  }
//
//  private def lockedTransform(f: T => T) {
//    val prev = acquireLock
//    releaseLock(prev, new Unlocked(f(prev.value), nonTxnWriteVersion(prev.version)))
//  }
//
//  def transformIfDefined(pf: PartialFunction[T,T]): Boolean = {
//    val v = get
//    if (!pf.isDefinedAt(v)) {
//      false
//    } else if (weakCompareAndSet(v, pf(v))) {
//      true
//    } else {
//      lockedTransformIfDefined(pf)
//    }
//  }
//
//  private def lockedTransformIfDefined(pf: PartialFunction[T,T]): Boolean = {
//    val prev = acquireLock
//    if (!pf.isDefinedAt(prev.value)) {
//      releaseLock(prev, prev.unlocked)
//      false
//    } else {
//      releaseLock(prev, new Unlocked(pf(prev.value), nonTxnWriteVersion(prev.version)))
//      true
//    }
//  }
//}

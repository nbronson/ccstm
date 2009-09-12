/* CCSTM - (c) 2009 Stanford University - PPL */

// IndirectEagerTL2.scala

package edu.stanford.ppl.ccstm.impls

import java.util.concurrent.atomic.AtomicLong
import edu.stanford.ppl.ccstm.TVar
import edu.stanford.ppl.ccstm.UnrecordedRead

/** An STM implementation that uses a TL2-style timestamp system, but that
 *  performs eager acquisition of write locks and that revalidates the
 *  transaction to extend the read version, rather than rolling back.  Version
 *  metadata is associated with each value via an immutable wrapper, so there
 *  is no need to store metadata in the holding object.
 *
 *  @author Nathan Bronson
 */
trait IndirectEagerTL2 {
  type Data[T] = IndirectEagerTL2.Wrapped[T]
  def initialData[T](initialValue: T): Data[T] = new IndirectEagerTL2.Unlocked(initialValue, 0L)
  type Metadata = Unit
  type MetadataHolder = UnitMetadataHolder
  type TxnAccessor[T] = IndirectEagerTL2TxnAccessor[T]
  type NonTxnAccessor[T] = IndirectEagerTL2NonTxnAccessor[T]
  type TxnImpl = IndirectEagerTL2Txn
}

object IndirectEagerTL2 {

  type Version = Long

  sealed abstract class Wrapped[T](val value: T, val version: Version) {
    def nonTxnRead: T
    def nonTxnSnapshot: Wrapped[T]

    def stillValid(v: Version): Boolean

    /** True iff a CAS may be used to obtain write permission. */
    def isAcquirable: Boolean
    def isLockedBy(txn: IndirectEagerTL2Txn): Boolean

    /** Only handles self-reads, does not check if a TxnLocked is present for
     *  a commmitted transaction.
     */
    def txnRead(txn: IndirectEagerTL2Txn): T
  }

  class Unlocked[T](value0: T, version0: Version) extends Wrapped[T](value0, version0) {
    def nonTxnRead = value
    def nonTxnSnapshot = this

    def stillValid(v: Version) = (v == version)

    def isAcquirable = false
    def isLockedBy(txn: IndirectEagerTL2Txn) = false

    def txnRead(txn: IndirectEagerTL2Txn) = value
  }

  class NonTxnLocked[T](val unlocked: Unlocked[T]) extends Wrapped[T](unlocked.value, unlocked.version) {
    def nonTxnRead = value
    def nonTxnSnapshot = this

    def stillValid(v: Version) = (v == version)

    def isAcquirable = true
    def isLockedBy(txn: IndirectEagerTL2Txn) = false

    def txnRead(txn: IndirectEagerTL2Txn) = value
  }

  class TxnLocked[T](value0: T,
                     version0: Version,
                     var specValue: T,
                     val owner: IndirectEagerTL2Txn) extends Wrapped[T](value0, version0) {

    // Owner's commit point is the one where status becomes mustCommit.  We
    // define the linearization point of this operation to be when we check the
    // status.  Since txn._commitVersion is always assigned before commit is
    // decided, committing transactions cannot obstruct either a nonTxnRead or
    // a nonTxnSnapshot (used for unrecorded read).

    def nonTxnRead = if (owner.mustCommit) specValue else value
    def nonTxnSnapshot = if (owner.mustCommit) new Unlocked(specValue, owner._commitVersion) else this

    def stillValid(v: Version) = {
      v == (if (owner.mustCommit) owner._commitVersion else version)
    }

    def isAcquirable = owner.status.mustRollBack
    def isLockedBy(txn: IndirectEagerTL2Txn) = (owner == txn)

    def txnRead(txn: IndirectEagerTL2Txn) = (if (isLockedBy(txn)) specValue else value)

    def reverted = new Unlocked(value, version)
    def committed(commitVersion: Version) = new Unlocked(commitVersion, specValue)
  }

  /** The global timestamp.  We use TL2's GV6 scheme to avoid the need to
   *  increment this during every transactional commit.  Non-transactional
   *  writes are even more conservative, incrementing the global version only
   *  when absolutely required. 
   */
  private val globalVersion = new AtomicLong(1)

  /** The approximate ratio of the number of commits to the number of
   *  increments of <code>globalVersion</code>, as in TL2's GV6 scheme.  If
   *  greater than one, the actual choice to commit or not is made with a
   *  random number generator.
   */
  private val silentCommitRatio = Runtime.getRuntime.availableProcessors max 16

  /** If <i>x</i> is a signed integer evenly chosen from a uniform distribution
   *  between Integer.MIN_VALUE and Integer.MAX_VALUE, then the test
   *  <code>(x <= silentCommitCutoff)</code> will succeed approximately
   *  <code>1.0 / silentCommitRatio</code> of the time.
   */
  private val silentCommitCutoff = {
    ((1 << 31) + ((1L << 32) / silentCommitRatio) - 1).asInstanceOf[Int]
  }

  private val silentCommitRand = new FastPoorRandom

  /** Guarantees that <code>globalVersion</code> is greater than
   *  or equal to <code>prevVersion</code>, and returns a value greater than
   *  the value of <code>globalVersion</code> present on entry and greater
   *  than <code>prevVersion</code>.
   */
  def nonTxnWriteVersion(prevVersion: Version): Version = {
    val g0 = globalVersion.get
    if (g0 >= prevVersion) {
      1 + g0
    } else {
      var g = g0
      while (g < prevVersion && !globalVersion.compareAndSet(g, prevVersion)) {
        g = globalVersion.get
      }
      1 + prevVersion
    }
  }

  /** Returns a read version for use in a new transaction. */
  def freshReadVersion: Version = globalVersion.get

  /** Guarantees that <code>globalVersion</code> is &ge; <code>minRV</code>,
   *  and returns a <code>globalVersion.get</code>.
   */
  def freshReadVersion(minRV: Version): Version = {
    var g = globalVersion.get
    while (g < minRV) {
      if (globalVersion.compareAndSet(g, minRV)) {
        // succeeded, we're done
        return minRV
      }
      // failed, retry
      g = globalVersion.get
    }
    return g
  }

  /** Returns a value that is greater than the <code>globalVersion</code> on
   *  entry, possibly incrementing <code>globalVersion</code>.
   */
  def freshCommitVersion: Version = {
    val g = globalVersion.get
    if (silentCommitRatio <= 1 || silentCommitRand.nextInt <= silentCommitCutoff) {
      globalVersion.compareAndSet(g, g + 1)
    }
    g + 1
  }
}

/** @see edu.stanford.ppl.ccstm.IndirectEagerTL2
 *
 *  @author Nathan Bronson
 */
abstract class IndirectEagerTL2Txn(failureHistory: List[Txn.RollbackCause]) extends AbstractTxn {

  import IndirectEagerTL2._
  import Txn._

  //////////////// State

  // _status and _statusCAS come from AbstractTxn

  /** The read version of this transaction.  It is guaranteed that all values
   *  read by this transaction have a version number less than or equal to this
   *  value, and that any transaction whose writes conflict with this
   *  transaction will label those writes with a version number greater than
   *  this value.  The read version must never be greater than
   *  <code>globalVersion.get</code>, must never decrease, and each time it is
   *  changed the read set must be revalidated.
   */
  private[impls] var _readVersion: Version = globalVersion.get

  /** If true, then other transactions may not use the old value from
   *  <code>TxnLocked</code> instances owned by this transaction.  This does
   *  not necessarily mean this transaction will commit, but it means that the
   *  <code>_commitVersion</code> has already been assigned and hence other
   *  transactions may have <code>_readVersion</code>s larger than the eventual
   *  new version of the data element.
   *  <p>
   *  Transactions may use the <code>TxnLocked.specValue</code> only if
   *  <code>_status.mustCommit</code>.
   */
  @volatile private[impls] var _writesPreventRead = false 

  /** The version number with which this transaction's writes are labelled, or
   *  zero if this transaction has not yet decided on commit or is in the
   *  <code>Committing</code> state but has not yet had time to query the
   *  global version.
   *  <p>
   *  Other transactions that encounter a TxnLocked owned by this transaction
   *  must wait for _commitVersion to be non-zero if they observe our status as
   *  Committing, because linearize after us.
   */
  @volatile private[impls] var _commitVersion: Version = 0

  /** True if all reads should be performed as writes. */
  private[impls] val barging: Boolean = {
    // barge if we have already had 3 failures
    // TODO: ignore explicit retries
    failureHistory.lengthCompare(3) >= 0
  }

  private[impls] var _readCount = 0
  private[impls] var _reads = new Array[IndirectEagerTL2TxnAccessor[_]](8)
  private[impls] var _writeCount = 0
  private[impls] var _writes = new Array[IndirectEagerTL2TxnAccessor[_]](4)


  /** Returns true if this txn has released all of its locks or if the locks
   *  are eligible to be stolen.
   */
  private[impls] def completedOrDoomed: Boolean = {
    val s = status
    (s == Committed || s.mustRollBack)
  }

  /** Returns a non-zero _commitVersion, blocking if necessary. */
  private[impls] def blockingCommitVersion: Version = {
    val v = _commitVersion
    if (v != 0) v else blockingCommitVersionImpl
  }

  private def blockingCommitVersionImpl: Version = {
    assert(_status.mustCommit)

    // The window between entering the Committing state and assigning a
    // commit version is pretty small, so we use a simple spin loop.
    var tries = 0
    var v = _commitVersion
    while (v == 0) {
      tries += 1
      if (tries > 50) Thread.`yield`
      v = _commitVersion
    }
    v
  }

  private[impls] def addToReadSet(w: IndirectEagerTL2TxnAccessor[_]) {
    if (_readCount == _reads.length) _reads = grow(_reads)
    _reads(_readCount) = w
    _readCount += 1
  }

  private[impls] def addToWriteSet(w: IndirectEagerTL2TxnAccessor[_]) {
    if (_writeCount == _writes.length) _writes = grow(_writes)
    _writes(_writeCount) = w
    _writeCount += 1
  }

  private def grow(a: Array[IndirectEagerTL2TxnAccessor[_]]) = {
    val z = new Array[IndirectEagerTL2TxnAccessor[_]](a.length * 2)
    Array.copy(a, 0, z, 0, a.length)
    z
  }

  //////////////// Implementation

  /** On return, the read version will have been the global version at some
   *  point during the call, the read version will be &ge; minReadVersion, and
   *  all reads will have been validated against the new read version.  Throws
   *  <code>RollbackError</code> if invalid.
   */
  private[impls] def revalidate(minReadVersion: Version) {
    if (!revalidateNoThrow(minReadVersion)) throw RollbackError
  }

  private[impls] def revalidateNoThrow(minReadVersion: Version): Boolean = {
    _readVersion = freshReadVersion(minReadVersion)
    var i = 0
    while (i < _readCount) {
      val r = _reads(i)
      val cur = r.data
      if (!cur.stillValid(r._readSnapshot.version)) {
        forceRollback(InvalidReadCause(r, null))
        return false
      }
      i += 1
    }
    // STM managed reads were okay, what about explicitly registered read-only
    // resources?
    return readResourcesValidate()
  }

  /** Returns true if the caller should wait for the txn to complete, false if
   *  the reader should proceed with the old value, hoping to commit before
   *  <code>currentOwner</code>.  May also choose to doom either txn, in which
   *  case <code>RollbackError</code> will be thrown.  At least one of the
   *  following will be true on normal return:<pre>
   *    -  the return value
   *    -  !currentOwner._writesPreventRead
   *    -  currentOwner.decided
   *  </pre>
   */
  private[impls] def shouldWaitForRead(currentOwner: IndirectEagerTL2Txn): Boolean = {
    // TODO: something more sophisticated
    
    // this method must take care to avoid deadlock cycles, via the other txn
    // waiting on a read, or a block inside resolveWriteWriteConflict

    currentOwner._writesPreventRead && !currentOwner.decided
  }

  /** After this method returns, either the current transaction will have been
   *  rolled back or <code>currentOwner</code> will allow the write resource to
   *  be acquired.
   */
  private[impls] def resolveWriteWriteConflict(currentOwner: IndirectEagerTL2Txn, contended: AnyRef) {
    val cause = WriteConflictCause(contended, null)

    // this test is _almost_ symmetric
    if (this.hashCode <= currentOwner.hashCode) {
      forceRollback(cause)
      throw RollbackError
    } else {
      currentOwner.requestRollback(cause)
    }
  }

  
  private[ccstm] def commitImpl(): Status = {
    assert(status == Active || status.isInstanceOf[MarkedRollback])

    if (status.mustRollBack || !callBeforeCommit) {
      // no need to prepare
      _status = RollingBack(rollbackCause)
      rollbackWrites
      writeResourcesPerformRollback
      _status = Rolledback(rollbackCause)
      callAfter
      return _status
    }

    if (_writeCount == 0 && !writeResourcesPresent) {
      // read-only transactions are easy to commit, because all of the reads
      // are already guaranteed to be consistent
      if (!_statusCAS(Active, Committed)) {
        // remote fail
        assert(_status.isInstanceOf[MarkedRollback])
        _status = Rolledback(rollbackCause)
      }
      callAfter
      return _status
    }

    if (!_statusCAS(Active, Preparing) || !writeResourcesPrepare()) {
      // roll back
    }

    // prevent any transactions that begin or revalidate after this point from
    // reading any of the old values
    _writesPreventRead = true

    

    // TODO: implement
    null
  }

  private def rollbackWrites() {
    var i = 0
    while (i < _writeCount) {
      val acc = _writes(i)

      // we must use CAS, to account for stealing, but we don't need to retry
      // because we can only fail if there was a thief
      acc.data match {
        case tl: TxnLocked[_] => acc.dataCAS(tl, tl.reverted)
      }

      i += 1
    }
  }

  private def commitWrites(cv: Version) {
    var i = 0
    while (i < _writeCount) {
      val acc = _writes(i)

      // we don't need to use CAS, because stealing can only occur from doomed
      // transactions
      acc.data = acc.data.asInstanceOf[TxnLocked[_]].committed(cv)

      i += 1
    }
  }

  private[ccstm] def requestRollbackImpl(cause: RollbackCause): Boolean = {
    while (true) {
      val s = _status
      if (s.mustCommit) return false
      if (s.mustRollBack) return true
      assert(s == Active)
      if (_statusCAS(s, MarkedRollback(cause))) return true
    }
    throw new Error("unreachable")
  }

  private[ccstm] def explicitlyValidateReadsImpl() {
    revalidate(0)
  }
}

abstract class IndirectEagerTL2TxnAccessor[T] extends TVar.Bound[T] {
  import IndirectEagerTL2._

  //////////////// Abstract methods

  def data: Wrapped[T]
  def data_=(v: Wrapped[T])
  def dataCAS(before: Wrapped[T], after: Wrapped[T]): Boolean

  def fieldIndex: Int
  val txn: IndirectEagerTL2Txn

  //////////////// Implementation

  var _readSnapshot: Wrapped[T] = null

  def context: Option[Txn] = Some(txn.asInstanceOf[Txn])

  def elem: T = {
    val t = txn
    t.requireActive

    val w0 = data
    if (w0.isLockedBy(t)) {
      w0.asInstanceOf[TxnLocked[T]].specValue
    } else {
      readImpl(t, w0, false).value
    }
  }

  // does not check for read-after-write
  private[impls] def readImpl(t: IndirectEagerTL2Txn, w0: Wrapped[T], unrecorded: Boolean): Wrapped[T] = {
    var w: Wrapped[T] = w0

    if (_readSnapshot != null) {
      // Repeat previous read that used this accessor.  This helps performance
      // when using the bind(txn) API.
      return _readSnapshot
    }

    // is this txn desperate?
    if (t.barging) {
      return readForWriteImpl(t, w0)
    }

    var okay = false
    do {
      w match {
        case u: Unlocked[_] => {
          // No waiting necessary
          okay = true
        }
        case nl: NonTxnLocked[_] => {
          // NonTxnLocked is only used by non-transactional transform-like
          // operations that fail to succeed with a weakCompareAndSet on their
          // first try.  Although it would be correct to try to use the old
          // value and see if we can commit first, it seems unlikely that this
          // will be profitable in practice (this would also require changing
          // the non-txn code).
          w = waitForNonTxn(nl)
        }
        case tl: TxnLocked[_] => {
          // We checked for read-after-write earlier, so this is a true
          // conflict.  We can either wait, or we can use the old value and
          // hope we commit before them.  To avoid deadlock cycles, we impose a
          // partial ordering on transactions and only wait for those that are
          // less than the current one.  shouldWaitForRead will honor the
          // _writesPreventRead flag.
          if (t.shouldWaitForRead(tl.owner)) {
            w = waitForTxnReadPermission(tl)
          } else {
            // If commit is inevitable, we can (should) use the new value.  If
            // commit is not inevitable, then we are allowed to use the old
            // value (owner is not yet committing) or we must use the old value
            // (owner has decided to roll back).
            if (tl.owner.mustCommit) {
              w = new Unlocked(tl.specValue, tl.owner._commitVersion)
            }
            okay = true
          }
        }
      }
    } while (!okay)

    // attempt to read at w.version
    if (w.version > t._readVersion) t.revalidate(w.version)

    // read is stable and consistent
    
    if (!unrecorded) {
      _readSnapshot = w
      t.addToReadSet(this)
    }

    return w
  }

  private def waitForNonTxn(nl: NonTxnLocked[_]): Wrapped[T] = {
    // TODO: park after some spins
    var d = data
    while (d.asInstanceOf[AnyRef] eq nl) {
      Thread.`yield`
      d = data
    }
    d
  }

  private def waitForTxnReadPermission(tl: TxnLocked[_]): Wrapped[T] = {
    // TODO: park after some spins
    while (!tl.owner.decided) {
      Thread.`yield`
    }
    data
  }

  private def waitForTxnWritePermission(tl: TxnLocked[_]): Wrapped[T] = {
    // TODO: park after some spins
    while (!tl.owner.completedOrDoomed) {
      Thread.`yield`
    }
    data
  }

  def unrecordedRead: UnrecordedRead[T] = {
    val t = txn
    t.requireActive

    val w0 = data
    if (w0.isLockedBy(t)) {
      return new UnrecordedRead[T] {
        def context = IndirectEagerTL2TxnAccessor.this.context
        val value = w0.asInstanceOf[TxnLocked[T]].specValue
        def recorded = true
        def stillValid = (w0 eq data.asInstanceOf[AnyRef])
      }
    } else {
      val w = readImpl(t, w0, false)
      return new UnrecordedRead[T] {
        def context = IndirectEagerTL2TxnAccessor.this.context
        def value = w.value
        val recorded = (_readSnapshot != null)
        def stillValid = {
          // The version number should match, and we should still have
          // permission to read (!owner._writesPreventRead)
          
          // have to handle write-after-read, as well as Unlocked instances
          // that have the write version but a different identity
          val cur = data
          (w eq cur) || (w.version == cur.version) (cur.isLockedBy(t) && (w.version == cur.version))
        }
      }
    }
  }

  // write barrier
  def elem_=(v: T) {
    readForWriteImpl.specValue = v
  }

  private[impls] def readForWriteImpl: TxnLocked[T] = {
    val t = txn
    t.requireActive

    val w0 = data
    if (w0.isLockedBy(t)) {
      // easy
      w0.asInstanceOf[TxnLocked[T]]
    } else {
      readForWriteImpl(t, w0)
    }
  }

  private[impls] def readForWriteImpl(t: IndirectEagerTL2Txn, w0: Wrapped[T]): TxnLocked[T] = {
    var w = w0
    var result: TxnLocked[T] = null
    while (result == null) {
      if (w.isAcquirable) {
        // Unlocked, or locked by a doomed txn

        // Validate previous version
        if (w.version > t._readVersion) t.revalidate(w.version)

        // Acquire write permission, possibly stealing the slot from a doomed
        // txn that has not yet completely rolled back.
        val v = w.value
        val after = new TxnLocked(v, w.version, v, t)
        if (dataCAS(w, after)) {
          // success!
          t.addToWriteSet(this)
          result = after
        }
      }
      else {
        // Locked by somebody
        w match {
          case nl: NonTxnLocked[_] => {
            // We never doom a non-txn update.
            w = waitForNonTxn(nl)
          }
          case tl: TxnLocked[_] => {
            // Since we can't buffer multiple speculative versions for this
            // data type in this STM, write-write conflicts must be handled
            // immediately.  We can: doom the other txn and steal the lock,
            // block the current txn, or roll back the current txn.  If the
            // contention manager in resolveWWConflict wants to roll back the
            // current txn it will throw RollbackError itself.  If it
            // wants to doom tl.owner it will also do that directly.  Once the
            // other transaction has decided, we can proceed.
            t.resolveWriteWriteConflict(tl.owner, this)
            w = waitForTxnWritePermission(tl)
          }
          case u: Unlocked[_] => throw new IllegalStateException
        }
      }
    }

    return result
  }

  def tryWrite(v: T): Boolean = {
    val t = txn
    val w0 = data

    t.requireActive

    if (w0.isLockedBy(t)) {
      // easy
      w0.asInstanceOf[TxnLocked[T]].specValue = v
      return true
    }

    if (!w0.isAcquirable) {
      // fail without blocking
      return false
    }

    // Unlocked, or locked by a doomed txn

    // Validate previous version
    if (w0.version > t._readVersion) t.revalidate(w0.version)

    // Attempt to acquire write permission, possibly stealing the slot from a
    // doomed txn that has not yet completely rolled back.
    val after = new TxnLocked(w0.value, w0.version, v, t)
    if (dataCAS(w0, after)) {
      // success!
      t.addToWriteSet(this)
      return true
    } else {
      // rollbackCause, don't retry
      return false
    }
  }

  override def readForWrite: T = {
    readForWriteImpl.specValue
  }

  override def transform(f: T => T) {
    // TODO: a better implementation
    val tl = readForWriteImpl
    tl.specValue = f(tl.specValue)
  }

  def transformIfDefined(pf: PartialFunction[T,T]): Boolean = {
    // TODO: a better implementation
    if (elemMap(v => pf.isDefinedAt(v))) {
      val tl = readForWriteImpl
      tl.specValue = pf(tl.specValue)
      true
    } else {
      false
    }
  }
}

abstract class IndirectEagerTL2NonTxnAccessor[T] extends TVar.Bound[T] {
  import IndirectEagerTL2._

  //////////////// Abstract methods

  def data: Wrapped[T]
  def data_=(v: Wrapped[T])
  def dataCAS(before: Wrapped[T], after: Wrapped[T]): Boolean

  //////////////// Implementation

  def context: Option[Txn] = None

  def elem: T = data.nonTxnRead

  def unrecordedRead: UnrecordedRead[T] = {
    new UnrecordedRead[T] {
      private val _snapshot = data.nonTxnSnapshot

      def context = None
      def value: T = _snapshot.value
      def stillValid: Boolean = data.stillValid(_snapshot.version)
      def recorded: Boolean = false
    }
  }

  private def awaitUnlock: Unlocked[T] = {
    // TODO improve
    var d = data
    while (!d.isInstanceOf[Unlocked[_]]) {
      Thread.`yield`
      d = data
    }
    d.asInstanceOf[Unlocked[T]]
  }

  private def acquireLock: NonTxnLocked[T] = {
    var before: Unlocked[T] = null
    var after: NonTxnLocked[T] = null
    do {
      before = awaitUnlock
      after = new NonTxnLocked(before)
    } while (!dataCAS(before, after))
    after
  }

  private def releaseLock(before: NonTxnLocked[T], after: Unlocked[T]) = {
    // TODO: maybe some notify logic?
    data = after
  }

  def elem_=(v: T) {
    var u: Unlocked[T] = null
    do {
      u = awaitUnlock
    } while (!dataCAS(u, new Unlocked(v, nonTxnWriteVersion(u.version))))
  }

  def tryWrite(v: T): Boolean = {
    val w = data
    w.isInstanceOf[Unlocked[_]] && dataCAS(w, new Unlocked(v, nonTxnWriteVersion(w.version)))
  }

  override def compareAndSet(before: T, after: T): Boolean = {
    var u: Unlocked[T] = null
    do {
      u = awaitUnlock
      if (!(before == u.value)) return false
    } while (!dataCAS(u, new Unlocked(after, nonTxnWriteVersion(u.version))))
    return true
  }

  override def compareAndSetIdentity[A <: T with AnyRef](before: A, after: T): Boolean = {
    var u: Unlocked[T] = null
    do {
      u = awaitUnlock
      if (!(before eq u.value.asInstanceOf[AnyRef])) return false
    } while (!dataCAS(u, new Unlocked(after, nonTxnWriteVersion(u.version))))
    return true
  }

  override def weakCompareAndSet(before: T, after: T): Boolean = {
    data match {
      case u: Unlocked[_] =>
        before == u.value && dataCAS(u, new Unlocked(after, nonTxnWriteVersion(u.version)))
      case _ =>
        false
    }
  }

  override def weakCompareAndSetIdentity[A <: T with AnyRef](before: A, after: T): Boolean = {
    data match {
      case u: Unlocked[_] =>
        (before eq u.value.asInstanceOf[AnyRef]) && dataCAS(u, new Unlocked(after, nonTxnWriteVersion(u.version)))
      case _ =>
        false
    }
  }

  override def transform(f: T => T) {
    val v = elem
    if (!weakCompareAndSet(v, f(v))) lockedTransform(f)
  }

  private def lockedTransform(f: T => T) {
    val prev = acquireLock
    releaseLock(prev, new Unlocked(f(prev.value), nonTxnWriteVersion(prev.version)))
  }

  def transformIfDefined(pf: PartialFunction[T,T]): Boolean = {
    val v = elem
    if (!pf.isDefinedAt(v)) {
      false
    } else if (weakCompareAndSet(v, pf(v))) {
      true
    } else {
      lockedTransformIfDefined(pf)
    }
  }

  private def lockedTransformIfDefined(pf: PartialFunction[T,T]): Boolean = {
    val prev = acquireLock
    if (!pf.isDefinedAt(prev.value)) {
      releaseLock(prev, prev.unlocked)
      false
    } else {
      releaseLock(prev, new Unlocked(pf(prev.value), nonTxnWriteVersion(prev.version)))
      true
    }
  }
}

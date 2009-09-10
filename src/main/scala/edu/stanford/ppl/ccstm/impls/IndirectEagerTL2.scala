/* CCSTM - (c) 2009 Stanford University - PPL */

// IndirectEagerTL2.scala

package edu.stanford.ppl.ccstm.impls

import java.util.concurrent.atomic.AtomicLong
import edu.stanford.ppl.ccstm.TVar
import edu.stanford.ppl.ccstm.UnrecordedRead

/** An STM implementation that uses a TL2-style timestamp system, but that
 *  performs eager acquisition of write locks and that associates version
 *  metadata with each value via an immutable wrapper.
 *
 *  @author Nathan Bronson
 */
trait IndirectEagerTL2 {
  type Metadata = Unit
  type MetadataHolder = UnitMetadataHolder
  def initialData[T](initialValue: T): AnyRef = new IndirectEagerTL2.Unlocked(initialValue, 0L)
  type TxnAccessor[T] = IndirectEagerTL2TxnAccessor[T]
  type NonTxnAccessor[T] = IndirectEagerTL2NonTxnAccessor[T]
  type TxnImpl = IndirectEagerTL2Txn
}

private[impls] object IndirectEagerTL2 {

  type Version = Long

  sealed abstract class Wrapped[T](val value: T, val version: Version) {
    def nonTxnRead: T
    def nonTxnVersion: Version
    def nonTxnSnapshot: Wrapped[T]

    /** True iff a CAS may be used to obtain write permission. */
    def isAcquirable: Boolean
    def isLockedBy(txn: IndirectEagerTL2Txn): Boolean

    def txnRead(txn: IndirectEagerTL2Txn): T
  }

  class Unlocked[T](value0: T, version0: Version) extends Wrapped[T](value0, version0) {
    def nonTxnRead = value
    def nonTxnVersion = version
    def nonTxnSnapshot = this

    def isAcquirable = false
    def isLockedBy(txn: IndirectEagerTL2Txn) = false

    def txnRead(txn: IndirectEagerTL2Txn) = value
  }

  class NonTxnLocked[T](val unlocked: Unlocked[T]) extends Wrapped[T](unlocked.value, unlocked.version) {
    def nonTxnRead = value
    def nonTxnVersion = version
    def nonTxnSnapshot = this

    def isAcquirable = true
    def isLockedBy(txn: IndirectEagerTL2Txn) = false

    def txnRead(txn: IndirectEagerTL2Txn) = value
  }

  class TxnLocked[T](value0: T,
                     version0: Version,
                     var specValue: T,
                     val owner: IndirectEagerTL2Txn) extends Wrapped[T](value0, version0) {
    def nonTxnRead = if (owner.decidedCommit) specValue else value
    def nonTxnVersion = if (owner.decidedCommit) owner.commitVersion else version
    def nonTxnSnapshot = if (owner.decidedCommit) new Unlocked(specValue, owner.commitVersion) else this

    def isAcquirable = owner.status.mustRollBack
    def isLockedBy(txn: IndirectEagerTL2Txn) = (owner == txn)

    def txnRead(txn: IndirectEagerTL2Txn) = (if (isLockedBy(txn)) specValue else value)
  }

  val globalVersion = new AtomicLong

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
}

abstract class IndirectEagerTL2Txn extends AbstractTxn {
  this: Txn =>

  import IndirectEagerTL2._
  import Txn._

  //////////////// State

  private[impls] var _readCount = 0
  private[impls] var _reads = new Array[IndirectEagerTL2TxnAccessor[_]](8)
  private[impls] var _writeCount = 0
  private[impls] var _writes = new Array[IndirectEagerTL2TxnAccessor[_]](4)

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

  //////////////// Private interface

  /** Returns the read version of this transaction.  It is guaranteed that all
   *  values read by this transaction have a version number less than or equal
   *  to this value, and that any transaction whose writes conflict with this
   *  transaction will label those writes with a version number greater than
   *  this value.
   */
  private[impls] val readVersion: Version

  /** True if all reads should be performed as writes. */
  private[impls] val barging: Boolean

  /** Returns true if this txn has released all of its locks or if the locks
   *  are eligible to be stolen.
   */
  private[impls] def completedOrDoomed: Boolean = {
    val s = status
    (s == Committed || s.mustRollBack)
  }

  /** The version number with which this transaction's writes are labelled. */
  private[impls] def commitVersion: Version = 0

  /** */
  private[impls] def revalidate(minReadVersion: Version) {
    // TODO: implement
  }

  /** Returns true if the caller should wait for the txn to complete, false if
   *  the reader should proceed with the old value, hoping to commit before
   *  <code>currentOwner</code>.  May also choose to doom either txn.
   */
  private[impls] def shouldWaitForRead(currentOwner: IndirectEagerTL2Txn): Boolean
  
  private[impls] def resolveWriteWriteConflict(currentOwner: IndirectEagerTL2Txn)

  //////////////// Public interface

  def status: Status = new Rolledback(true, null)
  def validatedStatus: Status = new Rolledback(true, null)

  private[ccstm] def attemptCommit: Boolean = {
    // TODO: implement
    false
  }
}

abstract class IndirectEagerTL2TxnAccessor[T] extends TVar.Bound[T] {
  import IndirectEagerTL2._

  //////////////// Abstract methods

  def data: Any
  def data_=(v: Any)
  def dataCAS(before: Any, after: Any): Boolean

  def fieldIndex: Int
  val txn: IndirectEagerTL2Txn

  //////////////// Implementation

  var _readVersion: Wrapped[T] = null

  def context: Option[Txn] = Some(txn.asInstanceOf[Txn])

  def elem: T = {
    val t = txn
    t.requireActive

    val w0 = data.asInstanceOf[Wrapped[T]]
    if (w0.isLockedBy(t)) {
      w0.asInstanceOf[TxnLocked[T]].specValue
    } else {
      readImpl(t, w0, false).value
    }
  }

  // does not check for read-after-write
  private[impls] def readImpl(t: IndirectEagerTL2Txn, w0: Wrapped[T], unrecorded: Boolean): Wrapped[T] = {
    var w: Wrapped[T] = w0

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
          // will be profitable in practice.
          w = waitForNonTxn(nl)
        }
        case tl: TxnLocked[_] => {
          // We checked for read-after-write earlier, so this is a true
          // conflict.  We can either wait, or we can use the old value and
          // hope we commit before them.  To avoid deadlock cycles, we impose a
          // partial ordering on transactions and only wait for those that are
          // less than the current one.
          if (t.shouldWaitForRead(tl.owner)) {
            w = waitForTxn(tl)
          } else {
            okay = true
          }
        }
      }
    } while (!okay)

    // attempt to read at w.version
    if (w.version > t.readVersion) t.revalidate(w.version)

    // read is stable and consistent
    if (!unrecorded && _readVersion == null) {
      _readVersion = w
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
    d.asInstanceOf[Wrapped[T]]
  }

  private def waitForTxn(tl: TxnLocked[_]): Wrapped[T] = {
    // TODO: park after some spins
    while (!tl.owner.completedOrDoomed) {
      Thread.`yield`
    }
    data.asInstanceOf[Wrapped[T]]
  }

  def unrecordedRead: UnrecordedRead[T] = {
    val t = txn
    t.requireActive

    val w0 = data.asInstanceOf[Wrapped[T]]
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
        val recorded = (_readVersion != null)
        def stillValid = {
          // have to handle write-after-read
          val cur = data.asInstanceOf[Wrapped[_]]
          (w eq cur) || (cur.isLockedBy(t) && (w.version == cur.version))
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

    val w0 = data.asInstanceOf[Wrapped[T]]
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
        if (w.version > t.readVersion) t.revalidate(w.version)

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
            // current txn it will throw RollbackException itself.  If it
            // wants to doom tl.owner it will also do that directly.
            t.resolveWriteWriteConflict(tl.owner)
            w = waitForTxn(tl)
          }
        }
      }
    }

    return result
  }

  def tryWrite(v: T): Boolean = {
    val t = txn
    val w0 = data.asInstanceOf[Wrapped[T]]

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
    if (w0.version > t.readVersion) t.revalidate(w0.version)

    // Attempt to acquire write permission, possibly stealing the slot from a
    // doomed txn that has not yet completely rolled back.
    val after = new TxnLocked(w0.value, w0.version, v, t)
    if (dataCAS(w0, after)) {
      // success!
      t.addToWriteSet(this)
      return true
    } else {
      // failure, don't retry
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

  def data: Any
  def data_=(v: Any)
  def dataCAS(before: Any, after: Any): Boolean

  //////////////// Implementation

  def context: Option[Txn] = None

  def elem: T = {
    data.asInstanceOf[Wrapped[T]].nonTxnRead
  }

  def unrecordedRead: UnrecordedRead[T] = {
    new UnrecordedRead[T] {
      private val _snapshot = data.asInstanceOf[Wrapped[T]].nonTxnSnapshot

      def context = None
      def value: T = _snapshot.value
      def stillValid: Boolean = _snapshot.version == data.asInstanceOf[Wrapped[T]].nonTxnVersion
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
    val d = data
    d.isInstanceOf[Unlocked[_]] && dataCAS(d, new Unlocked(v, nonTxnWriteVersion(d.asInstanceOf[Unlocked[T]].version)))
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

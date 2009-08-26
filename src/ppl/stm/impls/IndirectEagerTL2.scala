/* $Id$
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

package ppl.stm.impls

import java.util.concurrent.atomic.AtomicLong
import ppl.stm.TVar
import ppl.stm.UnrecordedRead

/** An STM implementation that uses a TL2-style timestamp system, but that
 *  performs eager acquisition of write locks and that associates version
 *  metadata with each value via an immutable wrapper.
 */
trait IndirectEagerTL2 {
  type Metadata = Unit
  type MetadataHolder = UnitMetadataHolder
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

    def txnRead(txn: Txn): T
  }

  class Unlocked[T](value0: T, version0: Version) extends Wrapped[T](value0, version0) {
    def nonTxnRead = value
    def nonTxnVersion = version
    def nonTxnSnapshot = this

    def txnRead(txn: Txn) = value
  }

  class NonTxnLocked[T](value0: T, version0: Version) extends Wrapped[T](value0, version0) {
    def nonTxnRead = value
    def nonTxnVersion = version
    def nonTxnSnapshot = this

    def txnRead(txn: Txn) = value
  }

  class TxnLocked[T](value0: T,
                     version0: Version,
                     var specValue: T,
                     val owner: IndirectEagerTL2Txn) extends Wrapped[T](value0, version0) {
    def nonTxnRead = if (owner.decidedCommit) specValue else value
    def nonTxnVersion = if (owner.decidedCommit) owner.commitVersion else version
    def nonTxnSnapshot = if (owner.decidedCommit) new Unlocked(owner.commitVersion, specValue) else this

    def txnRead(txn: Txn) = if ((txn eq owner) || owner.decidedCommit) specValue else value
  }

  val globalVersion = new AtomicLong

  def nonTxnWriteVersion(prevVersion: Version): Version = 0
}

class IndirectEagerTL2Txn extends AbstractTxn {
}

abstract class IndirectEagerTL2TxnAccessor[T] extends TVar.Bound[T] {
  import IndirectEagerTL2._

  def field: Int
  val txn: Txn

  var _addedToReadSet = false
  var _addedToWriteSet = false

  def context: Option[Txn] = Some(txn)

//  def elem: T = {
//    var attempts = 0
//    while (attempts < 100) {
//    }
//
//    null.asInstanceOf[T]
//  }
//
//  private def attemptRead(addToRS: Boolean): Option[T] = {
//    val v = metadata
//    val d = data
//    if (v != metadata) return None
//
//    if (v > txn.readVersion && !txn.revalidate(v)) throw RollbackException
//
//    val z = (if ((v & Txn.VersionChanging) != 0) {
//      // Somebody is changing _data.  If they have installed a Txn.Changing
//      // instance then we can choose to grab the pre-change value and
//      // continue, because we may commit before them.  If no Txn.Changing
//      // instance is available then the situation will quickly change,
//      // because threads do little work in that state, or we are very
//      // unlucky and the blocking thread has been preempted.
//      d match {
//        case c: Txn.Changing[T] => {
//          if (txn == c.txn) return Some(c.after)
//          c.before
//        }
//        case _ => return None
//      }
//    } else {
//      assert(!d.isInstanceOf[Txn.Changing[_]])
//      d.asInstanceOf[T]
//    })
//
//    // read is stable and consistent, should we add it to the read set?
//    if (addToRS) {
//      _addedToReadSet = true
//      txn.addToReadSet(this)
//    }
//  }
//
//  def elemMap[Z](f: (T) => Z): Z = {
//    if (_addedToReadSet || _addedToWriteSet) {
//      // no benefit to reexecution of f later
//      f(elem)
//    } else {
//
//    }
//  }
//
//  def elem_=(newValue: T) {
//    while (true) {
//      (TVar.this.synchronized {
//        data match {
//          case d: Txn.Changing[_] => d.txn
//          case _ => {
//            _version |= Txn.VersionChanging
//            data = newValue
//            _version = Txn.nextNonTxnWriteVersion()
//            return
//          }
//        }
//      }).awaitCompletion()
//    }
//  }
//
//  def transform(f: (T) => T) {
//    while (true) {
//      (TVar.this.synchronized {
//        data match {
//          case d: Txn.Changing[_] => d.txn
//          case d => {
//            val newValue = f(d.asInstanceOf[T])
//            _version |= Txn.VersionChanging
//            data = newValue
//            _version = Txn.nextNonTxnWriteVersion()
//            return
//          }
//        }
//      }).awaitCompletion()
//    }
//  }
//
//  def compareAndSet(before: T, after: T): Boolean = {
//    while (true) {
//      (TVar.this.synchronized {
//        data match {
//          case d: Txn.Changing[_] => d.txn
//          case d => {
//            if (before != d.asInstanceOf[T]) return false
//            _version |= Txn.VersionChanging
//            data = after
//            _version = Txn.nextNonTxnWriteVersion()
//            return true
//          }
//        }
//      }).awaitCompletion()
//    }
//    throw new Error("unreachable")
//  }

  def elem: T = null.asInstanceOf[T]

  def unrecordedRead: UnrecordedRead[T] = null

  def elem_=(v: T) {}

  def transformIfDefined(pf: PartialFunction[T,T]): Boolean = false
}

abstract class IndirectEagerTL2NonTxnAccessor[T] extends TVar.Bound[T] {
  import IndirectEagerTL2._

  def data: Any
  def data_=(v: Any)
  def dataCAS(before: Any, after: Any): Boolean

  def context: Option[Txn] = None

//
//  def elemMap[Z](f: (T) => Z): Z = f(elem)
//
//  def elem_=(newValue: T) {
//    while (true) {
//      (TVar.this.synchronized {
//        data match {
//          case c: Txn.Changing[_] => c.txn
//          case _ => {
//            _version |= Txn.VersionChanging
//            data = newValue
//            _version = Txn.nextNonTxnWriteVersion()
//            return
//          }
//        }
//      }).awaitCompletion()
//    }
//  }
//
//  def transform(f: (T) => T) {
//    while (true) {
//      (TVar.this.synchronized {
//        data match {
//          case c: Txn.Changing[_] => c.txn
//          case v => {
//            val newValue = f(v.asInstanceOf[T])
//            _version |= Txn.VersionChanging
//            data = newValue
//            _version = Txn.nextNonTxnWriteVersion()
//            return
//          }
//        }
//      }).awaitCompletion()
//    }
//  }
//
//  def compareAndSet(before: T, after: T): Boolean = {
//    while (true) {
//      (TVar.this.synchronized {
//        data match {
//          case c: Txn.Changing[_] => c.txn
//          case v => {
//            if (before != v.asInstanceOf[T]) return false
//            _version |= Txn.VersionChanging
//            data = after
//            _version = Txn.nextNonTxnWriteVersion()
//            return true
//          }
//        }
//      }).awaitCompletion()
//    }
//    throw new Error("unreachable")
//  }

  def elem: T = {
    data.asInstanceOf[Wrapped[T]].nonTxnRead
  }

  def unrecordedRead: UnrecordedRead[T] = {
    new UnrecordedRead[T] {
      private val _snapshot = data.asInstanceOf[Wrapped[T]].nonTxnSnapshot

      def value: T = _snapshot.value
      def stillValid: Boolean = _snapshot.version == data.asInstanceOf[Wrapped[T]].nonTxnVersion
      def recorded: Boolean = false
    }
  }

  private def awaitUnlock: Unlocked[T] = {
    // TODO: magic
  }

  private def acquireLock: Unlocked[T] = {
    var u: Unlocked[T] = null
    do {
      u = awaitLock
    } while (!dataCAS(u, new NonTxnLocked(u.value, u.version)))
    u
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
      if (before != u.value) return false
    } while (!dataCAS(u, new Unlocked(after, nonTxnWriteVersion(u.version))))
    return true
  }

  override def weakCompareAndSet(before: T, after: T): Boolean = {
    val d = data
    if (d.isInstanceOf[Unlocked[_]]) {
      val u = d.asInstanceOf[Unlocked[T]]
      before == u.value && dataCAS(d, new Unlocked(after, nonTxnWriteVersion(u.version)))
    } else {
      false
    }
  }

  override def transform(f: T => T) {
    val v = elem
    if (!weakCompareAndSet(v, f(v))) lockedTransform(f)
  }

  private def lockedTransform(f: T => T) {
    val prev = acquireLock
    data = new Unlocked(f(prev.value), nonTxnWriteVersion(prev.version))
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
      data = prev
      false
    } else {
      data = new Unlocked(pf(prev.value), nonTxnWriteVersion(prev.version))
      true
    }
  }
}


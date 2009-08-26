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

  abstract class Wrapped[T](val value: T, val version: Version) {
    def owner: IndirectEagerTL2Txn
  }

  class Unlocked[T](value: T, version: Version) extends Wrapped[T](value, version)

  class Locked[T](value: T,
                  version: Version,
                  val specValue: T,
                  val owner: IndirectEagerTL2Txn) extends Wrapped[T](value, version)


  val globalVersion = new AtomicLong

  def nextNonTxnWriteVersion(): Version = 0
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

  var metadata: Version
  var data: Any

  def context: Option[Txn] = None


//  def elem: T = {
//    data match {
//      case d: Txn.Changing[_] => d.asInstanceOf[Txn.Changing[T]].elem
//      case d => d.asInstanceOf[T]
//    }
//  }
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


  def elem: T = null.asInstanceOf[T]

  def unrecordedRead: UnrecordedRead[T] = null

  def elem_=(v: T) {}

  def transformIfDefined(pf: PartialFunction[T,T]): Boolean = false
}


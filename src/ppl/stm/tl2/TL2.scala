package ppl.stm.tl2

/* TL2
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

trait TL2 {
  type Metadata = Long
  type MetadataHolder = LongMetadataHolder
  type TxnAccessor[T] = TL2TxnAccessor[T]
  type NonTxnAccessor[T] = TL2NonTxnAccessor[T]

  private[stm] case class Changing[T](txn: Txn, before: T, after: T) {
    def elem = if (txn.committed) after else before
  }

  private[stm] type Version = Long

  private[stm] val VersionChanging: Version = 1L

  /** The fundamental correctness assumption of the system is that if txn R
   *  observes {@link #globalVersion} to be
   */
  @volatile private[stm] var globalVersion: Version = 0

  private[stm] def nextNonTxnWriteVersion(): Version = 0



  class TxnImpl {

  }
}

object TL2STM extends TL2

abstract class TL2TxnAccessor[T] extends TVar.Bound[T] {
  import TL2STM._

  def field: Int
  val txn: Txn
  var metadata: Metadata
  var data: Any

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

  def elem_=(v: T) {}

  def elemMap[Z](f: (T) => Z): Z = null.asInstanceOf[Z]

  def elem: T = null.asInstanceOf[T]

  def compareAndSet(before: T, after: T): Boolean = false

  def transform(f: (T) => T) {}

  def testAndTransform(cond: (T) => Boolean, f: (T) => T): Boolean = false
}

abstract class TL2NonTxnAccessor[T] extends TVar.Bound[T] {
  import TL2STM._

  var metadata: Metadata
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


  def elem_=(v: T) {}

  def elemMap[Z](f: (T) => Z): Z = null.asInstanceOf[Z]

  def elem: T = null.asInstanceOf[T]

  def compareAndSet(before: T, after: T): Boolean = false

  def transform(f: (T) => T) {}

  def testAndTransform(cond: (T) => Boolean, f: (T) => T): Boolean = false
}


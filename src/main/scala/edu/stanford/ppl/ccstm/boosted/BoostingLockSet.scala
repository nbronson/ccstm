/* CCSTM - (c) 2009 Stanford University - PPL */

// BoostingLockSet

package edu.stanford.ppl.ccstm.boosted


import java.lang.ref.{ReferenceQueue, WeakReference}
import java.util.concurrent.locks.{ReentrantLock, Lock}
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import scala.collection.mutable.{HashMap, HashSet}
object BoostingLockSet {

  val LockTimeoutMillis = 10
  val UndoPriority = -10
  val UnlockPriority = 10

  private class WeakRefWithKey(val key: Any, lock: Lock, q: ReferenceQueue[Lock]) extends WeakReference[Lock](lock, q)

  class Mapped extends BoostingLockSet {
    
    /** A set of locks already held by the current txn. */ 
    private val txnOwned = new TxnLocal[HashSet[Lock]] {
      override protected def initialValue(txn: Txn): HashSet[Lock] = {
        // arrange to unlock after commit or rollback
        txn.afterCompletion(t => {
          get(txn).foreach(_.unlock())
        }, UnlockPriority)

        new HashSet[Lock]
      }
    }

    /** The RW lock used for operations that touch all elements. */

    /** The locks for this lock set. */
    private val allLocks = new ConcurrentHashMap[Any,WeakRefWithKey]

    /** A queue to reclaim entries. */
    private val refQueue = new ReferenceQueue[Lock]

    private def lockFor(key: Any): Lock = {
      flush()
      var result: Lock = null
      do {
        val ref = allLocks.get(key)
        if (null != ref) {
          result = ref.get
        } else {
          val freshLock = new ReentrantLock
          val freshRef = new WeakRefWithKey(key, freshLock, refQueue)
          val existing = allLocks.putIfAbsent(key, freshRef)
          result = (if (null == existing) freshLock else existing.get)
        }
      } while (null == result)
      result
    }

    private def flush() {
      var ref = refQueue.poll()
      while (null != ref) {
        // we use the two-arg form in case someone has already replaced it
        allLocks.remove(ref.asInstanceOf[WeakRefWithKey].key, ref)
        ref = refQueue.poll()
      }
    }

    def acquire(key, txn: Txn) {
      val lock = lockFor(key)
      val owned = txnOwned.get
      if (owned.add(lock)) {
        // wasn't previously present, acquire it
        if (!lock.tryLock(LockTimeoutMillis, TimeUnit.MILLISECONDS)) {
          // acquisition failed, deadlock?  remove before afterCompletion
          owned.remove(lock)
          txn.forceRollback(Txn.WriteConflictCause(key, "tryLock timeout"))
          throw RollbackError
        }
      }
    }

    def nonTxnOp[A](key: Any, block: => A): A = {
      val lock = lockFor(key)
      // no deadlock potential (with correct usage), and no way to roll back,
      // so don't use tryLock
      lock.lock()
      try {
        block
      } finally {
        lock.unlock()
      }
    }
  }
}

/** Manages a set of pessimistic locks used by transactions to perform
 *  transactional boosting.
 */
trait BoostingLockSet {
  def lockKey(key: Any, txn: Txn)
  def lockSize(txn: Txn)
  def lockAll(txn: Txn)

  def nonTxnLockKey[A](key: Any, block: => A): A
  def nonTxnLockSize[A](block: => A): A
  def nonTxnLockAll[A](block: => A): A
}
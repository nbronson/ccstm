/* CCSTM - (c) 2009 Stanford University - PPL */

// MapBooster

package edu.stanford.ppl.repl.boosted

import edu.stanford.ppl.ccstm._
import java.lang.ref.{WeakReference, ReferenceQueue}
import java.util.concurrent.locks.{Lock, ReentrantLock}
import java.util.concurrent.{TimeUnit, ConcurrentHashMap}
import java.util.{IdentityHashMap, HashMap}


private[boosted] object MapBooster {

  private val LockTimeoutMillis = 10
  private val UndoAndUnlockPriority = -10

  private class LockRef[A](val key: A, lock: Lock, q: ReferenceQueue[Lock]) extends WeakReference[Lock](lock, q)

  trait TxnContext[A] {
    def lock(key: A)
    def undo[C](underlying: ConcurrentHashMap[A,C], key: A, vOpt: Option[C])
  }
}

private[boosted] class MapBooster[A] {
  import MapBooster._

  private val lockRefsByKey = new ConcurrentHashMap[A,LockRef[A]]

  /** A queue to reclaim entries. */
  private val refQueue = new ReferenceQueue[Lock]

  /** Holds the current context. */
  private val currentContext = new TxnLocal[TxnContext[A]] {
    override def initialValue(txn: Txn): TxnContext[A] = {
      val result = new TxnContext[A] with (Txn => Unit) {
        // per-txn state
        private val owned = new IdentityHashMap[Lock,Lock]
        private var undoCount = 0
        private var undoData: Array[Any] = null

        def lock(key: A) {
          val theLock = lockFor(key)
          if (null == owned.put(theLock, theLock)) {
            // wasn't previously present, acquire it
            if (!theLock.tryLock(LockTimeoutMillis, TimeUnit.MILLISECONDS)) {
              // acquisition failed, deadlock?  remove before afterCompletion
              owned.remove(theLock)
              txn.forceRollback(Txn.WriteConflictCause(key, "tryLock timeout"))
              throw RollbackError
            }
          }
        }

        def undo[C](underlying: ConcurrentHashMap[A,C], key: A, vOpt: Option[C]) {
          if (undoCount == 0) {
            undoData = new Array[Any](24)
          } else if (undoCount * 3 == undoData.length) {
            undoData = java.util.Arrays.copyOf(undoData, undoData.length * 2)
          }
          undoData(3 * undoCount + 0) = underlying
          undoData(3 * undoCount + 1) = key
          undoData(3 * undoCount + 2) = vOpt
          undoCount += 1
        }

        def apply(t: Txn) {
          if (t.status != Txn.Committed) {
            // apply the undo log in reverse order
            var i = undoCount
            while (i > 0) {
              i -= 1
              val u = undoData(3 * i + 0).asInstanceOf[ConcurrentHashMap[A,AnyRef]]
              val k = undoData(3 * i + 1).asInstanceOf[A]
              val vo = undoData(3 * i + 2).asInstanceOf[Option[AnyRef]]
              vo match {
                case None => u.remove(k)
                case Some(v) => u.put(k, v)
              }
            }
          }

          // release the locks
          var iter = owned.keySet.iterator
          while (iter.hasNext) iter.next.unlock()
        }
      }
      txn.afterCompletion(result, UndoAndUnlockPriority)
      result
    }
  }

  private def lockFor(key: A): Lock = {
    flush()
    var result: Lock = null
    do {
      val ref = lockRefsByKey.get(key)
      if (null != ref) {
        result = ref.get
        if (null == result) {
          val freshLock = new ReentrantLock
          val freshRef = new LockRef(key, freshLock, refQueue)
          if (lockRefsByKey.replace(key, ref, freshRef)) {
            result = freshLock
          }
        }
      } else {
        val freshLock = new ReentrantLock
        val freshRef = new LockRef(key, freshLock, refQueue)
        val existing = lockRefsByKey.putIfAbsent(key, freshRef)
        result = (if (null == existing) freshLock else existing.get)
      }
    } while (null == result)
    result
  }

  private def flush() {
    var ref = refQueue.poll()
    while (null != ref) {
      // we use the two-arg form in case someone has already replaced it
      lockRefsByKey.remove(ref.asInstanceOf[LockRef[A]].key, ref)
      ref = refQueue.poll()
    }
  }

  def context(implicit txn: Txn): TxnContext[A] = currentContext.get

  def nonTxnOp[Z](key: A)(block: => Z): Z = {
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
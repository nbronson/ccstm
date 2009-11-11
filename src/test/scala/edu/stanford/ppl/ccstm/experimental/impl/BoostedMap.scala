/* CCSTM - (c) 2009 Stanford University - PPL */

// BoostedMap

package edu.stanford.ppl.ccstm.experimental.impl

import edu.stanford.ppl.ccstm._
import experimental.TMap
import java.lang.ref.{WeakReference, ReferenceQueue}
import java.util.IdentityHashMap
import java.util.concurrent.{TimeUnit, ConcurrentHashMap}
import java.util.concurrent.locks._

/** An transactionally boosted <code>ConcurrentHashMap</code>, implemented
 *  directly from M. Herlify and E. Koskinen, <em>Transactional Boosting: A
 *  Methodology for Highly-Concurrent Transactional Objects</em>, PPoPP 2008.
 *  <p>
 *  The basic boosted map does not support transactional iteration or size,
 *  does not garbage collect keys, and uses mutexes to guard both read and
 *  write access (preventing concurrent reads of the same key).
 *  <p>
 *  The current implementation uses ReentrantLock-s internally, so it does not
 *  protect against concurrent access by multiple Txn (or nonTxn) that happen
 *  to share a thread.  Most STMs have an implicit association between the
 *  current transaction context and the current thread, but this is not present
 *  in CCSTM.
 *
 *  @author Nathan Bronson
 *
 *  @see edu.stanford.ppl.ccstm.experimental.impl.BoostedMap_GC
 *  @see edu.stanford.ppl.ccstm.experimental.impl.BoostedMap_GC_RW
 *  @see edu.stanford.ppl.ccstm.experimental.impl.BoostedMap_GC_Enum
 *  @see edu.stanford.ppl.ccstm.experimental.impl.BoostedMap_GC_Enum_RW
 */
class BoostedMap_Basic[A,B] extends BoostedMap[A,B](new BoostedMap.BasicLockHolder[A], null)

/** Adds garbage collection of unused abstract locks to
 *  <code>BoostedMap_Basic</code>.  Uses weak references.
 */
class BoostedMap_GC[A,B] extends BoostedMap[A,B](new BoostedMap.GCLockHolder[A], null)

/** Uses read/write locks to guard access to keys, rather than the mutexes of
 *  <code>BoostedMap_GC</code>.  This potentially allows greater concurrency
 *  during reads, but may have higher overheads.
 */
class BoostedMap_GC_RW[A,B] extends BoostedMap[A,B](new BoostedMap.GCRWLockHolder[A], null)

/** Adds an abstract lock to coordinate transactional enumeration to
 *  <code>BoostedMap_GC</code>.  The best implementation of this lock would use
 *  a multi-mode lock where enumeration was mode S and size change IX (see
 *  Y. Ni, V. Menon, A. Adl-Tabatabai, A. Hosking, R. Hudson, J. Moss, B. Saha,
 *  and T. Shpeisman, <em>Open Nesting in Software Transactional Memory</em>,
 *  PPoPP 2007).  Since we don't have a multi-mode lock handy, we use a
 *  ReentrantReadWriteLock in read mode during membership changes and in write
 *  mode during enumeration.  This yields no concurrency during enumeration, so
 *  be careful not to benchmark that.
 */
class BoostedMap_GC_Enum[A,B] extends BoostedMap[A,B](new BoostedMap.GCLockHolder[A], new ReentrantReadWriteLock)

/** Uses read/write locks to guard access to keys, rather than the mutexes of
 *  <code>BoostedMap_Enum</code>.  This potentially allows greater concurrency
 *  during reads, but may have higher overheads.
 */
class BoostedMap_GC_Enum_RW[A,B] extends BoostedMap[A,B](new BoostedMap.GCRWLockHolder[A], new ReentrantReadWriteLock)

object BoostedMap {

  val LockTimeoutMillis = 10
  val UndoAndUnlockPriority = -10

  //////// on-demand management of keys, without and with GC

  abstract class OnDemandMap[A,B <: AnyRef] {
    private val underlying = new ConcurrentHashMap[A,B]

    def newValue: B
    
    def apply(key: A): B = {
      val existing = underlying.get(key)
      if (null != existing) {
        existing
      } else {
        val fresh = newValue
        val race = underlying.putIfAbsent(key, fresh)
        if (null != race) race else fresh
      }
    }
  }

  class WeakRef[A,B](val key: A, value: B, q: ReferenceQueue[B]) extends WeakReference[B](value, q)

  abstract class WeakOnDemandMap[A,B <: AnyRef] {
    private val underlying = new ConcurrentHashMap[A,WeakRef[A,B]]

    /** A queue to reclaim entries. */
    private val refQueue = new ReferenceQueue[B]

    def newValue: B

    def apply(key: A): B = {
      flush()
      var result: B = null.asInstanceOf[B]
      do {
        val ref = underlying.get(key)
        if (null != ref) {
          result = ref.get
          if (null == result) {
            val freshValue = newValue
            val freshRef = new WeakRef(key, freshValue, refQueue)
            if (underlying.replace(key, ref, freshRef)) {
              result = freshValue
            }
          }
        } else {
          val freshValue = newValue
          val freshRef = new WeakRef(key, freshValue, refQueue)
          val existing = underlying.putIfAbsent(key, freshRef)
          result = (if (null == existing) freshValue else existing.get)
        }
      } while (null == result)
      result
    }

    private def flush() {
      var ref = refQueue.poll()
      while (null != ref) {
        // we use the two-arg form in case someone has already replaced it
        underlying.remove(ref.asInstanceOf[WeakRef[A,B]].key, ref)
        ref = refQueue.poll()
      }
    }
  }

  //////// per-key lock management strategies

  trait LockHolder[A] {
    def readLock(key: A): Lock
    def writeLock(key: A): Lock
  }

  /** A single mutex for reads and writes. */
  class BasicLockHolder[A] extends OnDemandMap[A,Lock] with LockHolder[A] {
    def newValue = new ReentrantLock

    def readLock(key: A) = this(key)
    def writeLock(key: A) = this(key)
  }

  /** A single mutex for reads and writes, with garbage collection. */
  class GCLockHolder[A] extends OnDemandMap[A,Lock] with LockHolder[A] {
    def newValue = new ReentrantLock

    def readLock(key: A) = this(key)
    def writeLock(key: A) = this(key)
  }

  /** A read/write lock per key, with garbage collection. */
  class GCRWLockHolder[A] extends OnDemandMap[A,ReadWriteLock] with LockHolder[A] {
    def newValue = new ReentrantReadWriteLock

    def readLock(key: A) = this(key).readLock
    def writeLock(key: A) = this(key).writeLock
  }

  //////// per-txn state

  trait TxnContext[A] {
    def lockForRead(key: A)
    def lockForWrite(key: A)
    def lockForEnumeration()
    def lockForMembershipChange()
    def undo[C <: AnyRef](underlying: ConcurrentHashMap[A,C], key: A, valueOrNull: C)
  }

  class MapBooster[A](lockHolder: LockHolder[A], enumLock: Lock, mcLock: Lock) {

    /** Holds the current context. */
    private val currentContext = new TxnLocal[TxnContext[A]] {
      override def initialValue(txn: Txn): TxnContext[A] = {
        val result = new TxnContext[A] with (Txn => Unit) {
          // per-txn state
          private val owned = new IdentityHashMap[Lock,Lock]
          private var undoCount = 0
          private var undoData: Array[Any] = null

          def lockForRead(key: A) { acquire(lockHolder.readLock(key), key) }
          def lockForWrite(key: A) { acquire(lockHolder.writeLock(key), key) }
          def lockForEnumeration() {
            if (null == enumLock) throw new UnsupportedOperationException("no enumeration lock present") 
            acquire(enumLock, "enum")
          }
          def lockForMembershipChange() { if (null != mcLock) acquire(mcLock, "member-change") }

          private def acquire(lock: Lock, info: Any) {
            if (null == owned.put(lock, lock)) {
              // wasn't previously present, acquire it
              if (!lock.tryLock(LockTimeoutMillis, TimeUnit.MILLISECONDS)) {
                // acquisition failed, deadlock?  remove before afterCompletion
                owned.remove(lock)
                txn.forceRollback(Txn.WriteConflictCause(info, "tryLock timeout"))
                throw RollbackError
              }
            }
          }

          def undo[C <: AnyRef](underlying: ConcurrentHashMap[A,C], key: A, valueOrNull: C) {
            if (undoCount == 0) {
              undoData = new Array[Any](24)
            } else if (undoCount * 3 == undoData.length) {
              undoData = java.util.Arrays.copyOf(undoData, undoData.length * 2)
            }
            undoData(3 * undoCount + 0) = underlying
            undoData(3 * undoCount + 1) = key
            undoData(3 * undoCount + 2) = valueOrNull
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
                val vo = undoData(3 * i + 2).asInstanceOf[AnyRef]
                if (null == vo) u.remove(k) else u.put(k, vo)
              }
            }

            // release the locks
            var iter = owned.keySet().iterator
            while (iter.hasNext) iter.next.unlock()
          }
        }
        txn.afterCompletion(result, UndoAndUnlockPriority)
        result
      }
    }

    def context(implicit txn: Txn): TxnContext[A] = currentContext.get
  }
}

class BoostedMap[A,B](lockHolder: BoostedMap.LockHolder[A], enumLock: ReadWriteLock) extends TMap[A, B] {
  import BoostedMap._

  private val underlying = new java.util.concurrent.ConcurrentHashMap[A,AnyRef]
  private val booster = new MapBooster[A](lockHolder,
                                          if (null == enumLock) null else enumLock.writeLock,
                                          if (null == enumLock) null else enumLock.readLock)

  val nonTxn: TMap.Bound[A,B] = new TMap.AbstractNonTxnBound[A,B,BoostedMap[A,B]](BoostedMap.this) {

    def get(key: A): Option[B] = {
      val lock = lockHolder.readLock(key)
      lock.lock()
      try {
        NullValue.decodeOption(underlying.get(key))
      } finally {
        lock.unlock()
      }
    }

    override def put(key: A, value: B): Option[B] = {
      val lock = lockHolder.writeLock(key)
      lock.lock()
      val prev = (try {
        if (null != enumLock && !underlying.contains(key)) {
          enumLock.readLock.lock()
          try {
            underlying.put(key, NullValue.encode(value))
          } finally {
            enumLock.readLock.unlock()
          }
        } else {
          underlying.put(key, NullValue.encode(value))
        }
      } finally {
        lock.unlock()
      })
      NullValue.decodeOption(prev)
    }

    override def removeKey(key: A): Option[B] = {
      val lock = lockHolder.writeLock(key)
      lock.lock()
      val prev = (try {
        if (null != enumLock) {
          if (!underlying.contains(key)) {
            null
          } else {
            enumLock.readLock.lock()
            try {
              underlying.remove(key)
            } finally {
              enumLock.readLock.unlock()
            }
          }
        } else {
          underlying.remove(key)
        }
      } finally {
        lock.unlock()
      })
      NullValue.decodeOption(prev)
    }

    protected def transformIfDefined(key: A,
                                     pfOrNull: PartialFunction[Option[B],Option[B]],
                                     f: Option[B] => Option[B]): Boolean = {
      val lock = lockHolder.writeLock(key)
      lock.lock()
      try {
        val prev = underlying.get(key)
        val p = NullValue.decodeOption(prev)
        if (null != pfOrNull && !pfOrNull.isDefinedAt(p)) {
          return false
        }

        val next = NullValue.encodeOption(f(p))
        if (null == prev) {
          if (null != next) {
            // None -> Some
            if (null != enumLock) {
              enumLock.readLock.lock()
              try {
                underlying.put(key, next)
              } finally {
                enumLock.readLock.unlock()
              }
            } else {
              underlying.put(key, next)
            }
          }
        } else {
          if (null == next) {
            // Some -> None
            if (null != enumLock) {
              enumLock.readLock.lock()
              try {
                underlying.remove(key)
              } finally {
                enumLock.readLock.unlock()
              }
            } else {
              underlying.remove(key)
            }
          } else {
            // Some -> Some
            underlying.put(key, next)
          }
        }

        return true
      } finally {
        lock.unlock()
      }
    }

    def elements: Iterator[(A,B)] = new Iterator[(A,B)] {
      val iter = underlying.keySet().iterator
      var avail: (A,B) = null
      advance()

      private def advance() {
        while (iter.hasNext) {
          val k = iter.next()
          get(k) match {
            case Some(v) => {
              avail = (k,v)
              return
            }
            case None => // keep looking
          }
        }
        avail = null
      }

      def hasNext: Boolean = null != avail
      def next(): (A,B) = {
        val z = avail
        advance()
        z
      }
    }
  }

  def bind(implicit txn0: Txn): TMap.Bound[A,B] = new TMap.AbstractTxnBound[A,B,BoostedMap[A,B]](txn0, BoostedMap.this) {
    def elements: Iterator[(A,B)] = {
      return new Iterator[(A,B)] {
        val iter = underlying.keySet().iterator
        var avail: (A,B) = null
        val ctx = booster.context
        ctx.lockForEnumeration()
        advance()

        private def advance() {
          while (iter.hasNext) {
            val k = iter.next()
            ctx.lockForRead(k)
            val v = underlying.get(k)
            if (null != v) {
              avail = (k, NullValue.decode(v))
              return
            }
          }
          avail = null
        }

        def hasNext = null != avail
        def next(): (A,B) = {
          val z = avail
          advance()
          z
        }
      }
    }
  }

  def isEmpty(implicit txn: Txn): Boolean = size > 0
  
  def size(implicit txn: Txn): Int = {
    val ctx = booster.context
    ctx.lockForEnumeration()
    underlying.size()
  }

  def get(key: A)(implicit txn: Txn): Option[B] = {
    val ctx = booster.context
    ctx.lockForRead(key)
    NullValue.decodeOption(underlying.get(key))
  }

  override def put(key: A, value: B)(implicit txn: Txn): Option[B] = {
    val ctx = booster.context
    ctx.lockForWrite(key)
    if (null != enumLock && !underlying.containsKey(key)) {
      ctx.lockForMembershipChange()
    }
    val prev = underlying.put(key, NullValue.encode(value))
    ctx.undo(underlying, key, prev)
    NullValue.decodeOption(prev)
  }

  override def removeKey(key: A)(implicit txn: Txn): Option[B] = {
    val ctx = booster.context
    ctx.lockForWrite(key)
    if (null != enumLock) {
      if (!underlying.containsKey(key)) {
        return None
      }
      ctx.lockForMembershipChange()
    }
    val prev = underlying.remove(key)
    ctx.undo(underlying, key, prev)
    return NullValue.decodeOption(prev)
  }

  protected def transformIfDefined(key: A,
                                   pfOrNull: PartialFunction[Option[B],Option[B]],
                                   f: Option[B] => Option[B])(implicit txn: Txn): Boolean = {
    val ctx = booster.context
    ctx.lockForWrite(key)

    val before = underlying.get(key)
    val b = NullValue.decodeOption(before)

    if (null != pfOrNull && !pfOrNull.isDefinedAt(b)) {
      return false
    }

    val after = NullValue.encodeOption(f(b))

    if (null == before && null == after) {
      // nothing to do, no membership change or undo
      return true
    }

    if (null != enumLock && (null == before || null == after)) {
      ctx.lockForMembershipChange()
    }

    if (null != after) {
      underlying.put(key, after)
    } else {
      underlying.remove(key)
    }

    ctx.undo(underlying, key, before)

    return true
  }
}


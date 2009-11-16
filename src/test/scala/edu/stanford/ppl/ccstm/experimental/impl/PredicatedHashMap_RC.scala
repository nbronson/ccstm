/* CCSTM - (c) 2009 Stanford University - PPL */

// PredicatedHashMap_RC

package edu.stanford.ppl.ccstm.experimental.impl

import edu.stanford.ppl.ccstm.experimental.TMap
import edu.stanford.ppl.ccstm.collection.{TOptionRef, LazyConflictIntRef}
import java.util.concurrent.ConcurrentHashMap
import edu.stanford.ppl.ccstm.experimental.TMap.Bound
import edu.stanford.ppl.ccstm.{STM, Txn}
import java.util.concurrent.atomic.AtomicIntegerFieldUpdater


object PredicatedHashMap_RC {
  private val refCountUpdater = (new Predicate[Int](0)).newUpdater

  class Predicate[B](initialRC: Int) extends TOptionRef[B](None) {
    def newUpdater = AtomicIntegerFieldUpdater.newUpdater(classOf[Predicate[_]], "_refCount")
    @volatile private var _refCount = initialRC

    def refCount = _refCount
    def refCountCAS(before: Int, after: Int) = refCountUpdater.compareAndSet(this, before, after)
  }
}

class PredicatedHashMap_RC[A,B] extends TMap[A,B] {
  import PredicatedHashMap_RC._

  val predicates = new ConcurrentHashMap[A,Predicate[B]]

  def nonTxn: Bound[A,B] = new TMap.AbstractNonTxnBound[A,B,PredicatedHashMap_RC[A,B]](this) {

    def get(key: A): Option[B] = {
      // if no predicate exists, or the one we get is stale, we can still read None
      val p = existingPred(key)
      if (null == p) None else p.nonTxn.get
    }

    override def put(key: A, value: B): Option[B] = {
      val p = pred(key, 1)
      val prev = p.nonTxn.getAndSet(Some(value))
      if (!prev.isEmpty) {
        exit(key, p, 1)
      }
      prev
    }

    override def removeKey(key: A): Option[B] = {
      // if no predicate exists, then we don't need to create one
      val p = existingPred(key)
      if (null == p) {
        None
      } else {
        val prev = p.nonTxn.getAndSet(None)
        if (!prev.isEmpty) {
          exit(key, p, 1)
        }
        prev
      }
    }

    override def transform(key: A, f: (Option[B]) => Option[B]) {
      // TODO: implement directly
      STM.atomic(unbind.transform(key, f)(_))
    }

    override def transformIfDefined(key: A, pf: PartialFunction[Option[B],Option[B]]): Boolean = {
      // TODO: implement directly
      STM.atomic(unbind.transformIfDefined(key, pf)(_))
    }

    protected def transformIfDefined(key: A,
                                     pfOrNull: PartialFunction[Option[B],Option[B]],
                                     f: Option[B] => Option[B]): Boolean = {
      throw new Error
    }

    def elements: Iterator[(A,B)] = new Iterator[(A,B)] {
      val iter = predicates.keySet().iterator
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

  def bind(implicit txn0: Txn): Bound[A, B] = new TMap.AbstractTxnBound[A,B,PredicatedHashMap_RC[A,B]](txn0, this) {
    def elements: Iterator[(A,B)] = throw new UnsupportedOperationException
  }

  def isEmpty(implicit txn: Txn): Boolean = throw new UnsupportedOperationException

  def size(implicit txn: Txn): Int = throw new UnsupportedOperationException

  def get(key: A)(implicit txn: Txn): Option[B] = {
    val p = pred(key, 1)
    txn.afterCompletion(t => exit(key, p, 1))
    p.get
  }

  def put(key: A, value: B)(implicit txn: Txn): Option[B] = {
    val p = pred(key, 1)
    try {
      val prev = p.getAndSet(Some(value))
      if (prev.isEmpty) {
        // None -> Some.  On commit, we leave +1 on the reference count
        txn.afterRollback(t => exit(key, p, 1))
      } else {
        // Some -> Some
        txn.afterCompletion(t => exit(key, p, 1))
      }
      prev
    } catch {
      case x => {
        exit(key, p, 1)
        throw x
      }
    }
  }

  def removeKey(key: A)(implicit txn: Txn): Option[B] = {
    val p = pred(key, 1)
    try {
      val prev = p.getAndSet(None)
      if (!prev.isEmpty) {
        // Some -> None.  On commit, we erase the +1 that was left by the
        // None -> Some transition
        txn.afterCompletion(t => exit(key, p, (if (txn.status == Txn.Committed) 2 else 1)))
      } else {
        // None -> None
        txn.afterCompletion(t => exit(key, p, 1))
      }
      prev
    } catch {
      case x => {
        exit(key, p, 1)
        throw x
      }
    }
  }


  override def transform(key: A, f: (Option[B]) => Option[B])(implicit txn: Txn) {
    val p = pred(key, 1)
    var sizeDelta = 0
    try {
      val before = p.get
      val after = f(before)
      p.set(after)
      sizeDelta = (if (after.isEmpty) 0 else 1) - (if (before.isEmpty) 0 else 1)
    } finally {
      txn.afterCompletion(t => exit(key, p, (if (txn.status == Txn.Committed) 1 - sizeDelta else 1)))
    }
  }

  override def transformIfDefined(key: A, pf: PartialFunction[Option[B],Option[B]])(implicit txn: Txn): Boolean = {
    val p = pred(key, 1)
    var sizeDelta = 0
    try {
      val before = p.get
      if (pf.isDefinedAt(before)) {
        val after = pf(before)
        p.set(after)
        sizeDelta = (if (after.isEmpty) 0 else 1) - (if (before.isEmpty) 0 else 1)
        true
      } else {
        false
      }
    } finally {
      txn.afterCompletion(t => exit(key, p, (if (txn.status == Txn.Committed) 1 - sizeDelta else 1)))
    }
  }

  private def exit(key: A, p: Predicate[B], delta: Int) {
    while (true) {
      val rc = p.refCount
      if (rc == delta) {
        // attempt to clean
        if (p.refCountCAS(rc, -1)) {
          predicates.remove(key, p)
          return
        }
      } else {
        // attempt to decrement
        if (p.refCountCAS(rc, rc - delta)) {
          return
        }
      }
    }
  }

  protected def transformIfDefined(key: A,
                                   pfOrNull: PartialFunction[Option[B],Option[B]],
                                   f: Option[B] => Option[B])(implicit txn: Txn): Boolean = {
    throw new Error
  }

  private def existingPred(key: A): Predicate[B] = predicates.get(key)

  private def pred(key: A, refCountDelta: Int): Predicate[B] = {
    pred(key, refCountDelta, predicates.get(key))
  }

  private def pred(key: A, refCountDelta: Int, p: Predicate[B]): Predicate[B] = {
    if (null == p) {
      createPred(key, refCountDelta)
    } else {
      var rc = 0
      do {
        rc = p.refCount
        if (rc < 0) {
          // stale predicate, help out, then retry
          predicates.remove(key, p)
          return createPred(key, refCountDelta)
        }
      } while (!p.refCountCAS(rc, rc + refCountDelta))

      // success
      return p
    }
  }

  private def createPred(key: A, refCountDelta: Int): Predicate[B] = {
    val fresh = new Predicate[B](refCountDelta)
    val race = predicates.putIfAbsent(key, fresh)
    if (null == race) fresh else pred(key, refCountDelta, race)
  }
}
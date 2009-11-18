/* CCSTM - (c) 2009 Stanford University - PPL */

// PredicatedHashMap_RC_Enum

package edu.stanford.ppl.ccstm.experimental.impl

import edu.stanford.ppl.ccstm.experimental.TMap
import java.util.concurrent.ConcurrentHashMap
import edu.stanford.ppl.ccstm.experimental.TMap.Bound
import edu.stanford.ppl.ccstm.{STM, Txn}
import java.util.concurrent.atomic.AtomicIntegerFieldUpdater
import edu.stanford.ppl.ccstm.collection.{StripedIntRef, TOptionRef, LazyConflictIntRef}


object PredicatedHashMap_RC_Enum {
  private val refCountUpdater = (new Pred[Int](0)).newUpdater

  class Pred[B](initialRC: Int) extends TOptionRef[B](None) {
    def newUpdater = AtomicIntegerFieldUpdater.newUpdater(classOf[Pred[_]], "_refCount")
    @volatile private var _refCount = initialRC

    def refCount = _refCount
    def refCountCAS(before: Int, after: Int) = refCountUpdater.compareAndSet(this, before, after)
  }
}

class PredicatedHashMap_RC_Enum[A,B] extends TMap[A,B] {
  import PredicatedHashMap_RC_Enum._

  private val sizeRef = new StripedIntRef(0)
  val predicates = new ConcurrentHashMap[A,Pred[B]]

  def nonTxn: Bound[A,B] = new TMap.AbstractNonTxnBound[A,B,PredicatedHashMap_RC_Enum[A,B]](this) {

    override def size(): Int = {
      sizeRef.nonTxn.get
    }

    def get(key: A): Option[B] = {
      // if no predicate exists, or the one we get is stale, we can still read None
      val p = existingPred(key)
      if (null == p) None else p.nonTxn.get
    }

    override def put(key: A, value: B): Option[B] = {
      val p = enter(key)
      val pn = p.nonTxn
      val before = pn.get
      if (!before.isEmpty) {
        // try to update (no size change)
        if (pn.compareAndSet(before, Some(value))) {
          // success
          exit(key, p, 1)
          return before
        }
        // failure goes to the transform2 implementation
      }
      val prev = STM.transform2(p, sizeRef.aStripe, (vo: Option[B], s: Int) => {
        (Some(value), (if (vo.isEmpty) s + 1 else s), vo)
      })
      if (!prev.isEmpty) {
        exit(key, p, 1)
      }
      prev
    }

    override def removeKey(key: A): Option[B] = {
      val p = existingPred(key)
      if (null == p || p.nonTxn.get.isEmpty) {
        // no need to create a predicate
        return None
      }
      val prev = STM.transform2(p, sizeRef.aStripe, (vo: Option[B], s: Int) => {
        (None, (if (vo.isEmpty) s else s - 1), vo)
      })
      if (!prev.isEmpty) {
        exit(key, p, 1)
      }
      prev
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

  def bind(implicit txn0: Txn): Bound[A, B] = new TMap.AbstractTxnBound[A,B,PredicatedHashMap_RC_Enum[A,B]](txn0, this) {
    def elements: Iterator[(A,B)] = new Iterator[(A,B)] {
      private var apparentSize = 0
      private val iter = predicates.entrySet().iterator
      private var avail: (A,B) = null

      advance()
      if (txn.barging) {
        // auto-readForWrite will prevent the size from changing in another txn
        size
      }

      private def advance() {
        while (iter.hasNext) {
          val e = iter.next()
          e.getValue.get match {
            case Some(v) => {
              apparentSize += 1
              avail = (e.getKey, v)
              return
            }
            case None => // keep looking
          }
        }

        // end of iteration
        if (apparentSize != size) {
          txn.forceRollback(Txn.InvalidReadCause(unbind, "PredicatedHashMap_RC_Enum.Iterator missed elements"))
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

  def isEmpty(implicit txn: Txn): Boolean = {
    sizeRef > 0
  }

  def size(implicit txn: Txn): Int = {
    sizeRef.get
  }

  def get(key: A)(implicit txn: Txn) = {
    val pred = enter(key)
    txn.afterCompletion(t => exit(key, pred, 1))
    pred.get
  }

  def put(k: A, v: B)(implicit txn: Txn): Option[B] = {
    val p = enter(k)
    try {
      val prev = p.getAndSet(Some(v))
      if (prev.isEmpty) {
        // None -> Some.  On commit, we leave +1 on the reference count
        sizeRef += 1
        txn.afterRollback(t => exit(k, p, 1))
      } else {
        // Some -> Some
        txn.afterCompletion(t => exit(k, p, 1))
      }
      prev
    } catch {
      case x => {
        exit(k, p, 1)
        throw x
      }
    }
  }

  def removeKey(k: A)(implicit txn: Txn): Option[B] = {
    val p = enter(k)
    try {
      val prev = p.getAndSet(None)
      if (!prev.isEmpty) {
        // Some -> None.  On commit, we erase the +1 that was left by the
        // None -> Some transition
        sizeRef -= 1
        txn.afterCompletion(t => exit(k, p, (if (txn.status == Txn.Committed) 2 else 1)))
      } else {
        // None -> None
        txn.afterCompletion(t => exit(k, p, 1))
      }
      prev
    } catch {
      case x => {
        exit(k, p, 1)
        throw x
      }
    }
  }


  override def transform(key: A, f: (Option[B]) => Option[B])(implicit txn: Txn) {
    val p = enter(key)
    var sizeDelta = 0
    try {
      val before = p.get
      val after = f(before)
      p.set(after)
      sizeDelta = (if (after.isEmpty) 0 else 1) - (if (before.isEmpty) 0 else 1)
      sizeRef += sizeDelta
    } finally {
      txn.afterCompletion(t => exit(key, p, (if (txn.status == Txn.Committed) 1 - sizeDelta else 1)))
    }
  }

  override def transformIfDefined(key: A, pf: PartialFunction[Option[B],Option[B]])(implicit txn: Txn): Boolean = {
    val p = enter(key)
    var sizeDelta = 0
    try {
      val before = p.get
      if (pf.isDefinedAt(before)) {
        val after = pf(before)
        p.set(after)
        sizeDelta = (if (after.isEmpty) 0 else 1) - (if (before.isEmpty) 0 else 1)
        sizeRef += sizeDelta
        true
      } else {
        false
      }
    } finally {
      txn.afterCompletion(t => exit(key, p, (if (txn.status == Txn.Committed) 1 - sizeDelta else 1)))
    }
  }

  protected def transformIfDefined(key: A,
                                   pfOrNull: PartialFunction[Option[B],Option[B]],
                                   f: Option[B] => Option[B])(implicit txn: Txn): Boolean = {
    throw new Error
  }

  private def existingPred(key: A): Pred[B] = predicates.get(key)

  private def enter(k: A): Pred[B] = {
    var p = predicates.get(k)
    var fresh: Pred[B] = null
    do {
      if (null != p) {
        var rc = p.refCount
        while (rc > 0) {
          if (p.refCountCAS(rc, rc + 1))
            return p
          rc = p.refCount
        }
        predicates.remove(k, p)
      }
      fresh = new Pred[B](1)
      p = predicates.putIfAbsent(k, fresh)
    } while (null != p)
    fresh
  }

  private def exit(k: A, p: Pred[B], d: Int) {
    val rc = p.refCount
    if (p.refCountCAS(rc, rc - d)) {
      if (rc - d == 0) predicates.remove(k, p)
    } else {
      exit(k, p, d)
    }
  }
}
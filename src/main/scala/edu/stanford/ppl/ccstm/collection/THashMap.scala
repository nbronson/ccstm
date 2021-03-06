/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// THashMap

package edu.stanford.ppl.ccstm.collection

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicIntegerFieldUpdater
import edu.stanford.ppl.ccstm._
import impl.{StripedIntRef, TAnyRef}

object THashMap {
  private val refCountUpdater = (new Pred[Int]).newUpdater

  class Pred[B] extends TAnyRef[AnyRef](null) {
    def newUpdater = AtomicIntegerFieldUpdater.newUpdater(classOf[Pred[_]], "_refCount")
    @volatile private var _refCount = 1

    def refCount = _refCount
    def refCountCAS(before: Int, after: Int) = refCountUpdater.compareAndSet(this, before, after)
  }
}

class THashMap[A,B] extends TMap[A,B] {
  import THashMap.Pred

  private val sizeRef = new StripedIntRef(0)

  private val predicates = new ConcurrentHashMap[A,Pred[B]] {
    override def putIfAbsent(key: A, value: Pred[B]): Pred[B] = {
      STM.resurrect(key.hashCode, value)
      super.putIfAbsent(key, value)
    }

    override def replace(key: A, oldValue: Pred[B], newValue: Pred[B]): Boolean = {
      val h = key.hashCode
      STM.embalm(h, oldValue)
      STM.resurrect(h, newValue)
      super.replace(key, oldValue, newValue)
    }

    override def remove(key: Any, value: Any): Boolean = {
      STM.embalm(key.hashCode, value.asInstanceOf[Pred[B]])
      super.remove(key, value)
    }
  }


  def escaped: TMap.View[A,B] = new TMap.AbstractEscapedView[A,B,THashMap[A,B]](this) {

    override def size(): Int = {
      sizeRef.escaped.get
    }

    def get(key: A): Option[B] = {
      // if no predicate exists, or the one we get is stale, we can still read None
      val p = predicates.get(key)
      if (null == p) None else NullValue.decodeOption(p.escaped.get)
    }

    override def put(key: A, value: B): Option[B] = {
      val p = enter(key)
      val pn = p.escaped
      val before = pn.get
      if (null != before) {
        // try to update (no size change)
        if (pn.compareAndSet(before, NullValue.encode(value))) {
          // success
          exit(key, p, 1)
          return Some(NullValue.decode(before))
        }
        // failure goes to the transform2 implementation
      }
      val prev = STM.transform2(p, sizeRef.aStripe, (v: AnyRef, s: Int) => {
        (NullValue.encode(value), (if (null == v) s + 1 else s), v)
      })
      if (null != prev) {
        exit(key, p, 1)
      }
      NullValue.decodeOption(prev)
    }

    override def remove(key: A): Option[B] = {
      val p = predicates.get(key)
      if (null == p || null == p.escaped.get) {
        // no need to create a predicate
        return None
      }
      val prev = STM.transform2(p, sizeRef.aStripe, (v: AnyRef, s: Int) => {
        (null, (if (null == v) s else s - 1), v)
      })
      if (null != prev) {
        exit(key, p, 1)
      }
      NullValue.decodeOption(prev)
    }

    def iterator: Iterator[(A,B)] = new Iterator[(A,B)] {
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

  def bind(implicit txn0: Txn): TMap.View[A, B] = new TMap.AbstractTxnView[A,B,THashMap[A,B]](txn0, this) {
    def iterator: Iterator[(A,B)] = new Iterator[(A,B)] {
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
          val v = e.getValue.get
          if (null != v) {
            apparentSize += 1
            avail = (e.getKey, NullValue.decode(v))
            return
          }
        }

        // end of iteration
        if (apparentSize != size) {
          txn.forceRollback(Txn.InvalidReadCause(unbind, "THashMap.Iterator missed elements"))
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
    sizeRef getWith { _ > 0 }
  }

  def size(implicit txn: Txn): Int = {
    sizeRef.get
  }

  def get(key: A)(implicit txn: Txn) = {
    val pred = enter(key)
    txn.afterCompletion(t => exit(key, pred, 1))
    NullValue.decodeOption(pred.get)
  }

  def put(k: A, v: B)(implicit txn: Txn): Option[B] = {
    val p = enter(k)
    try {
      val prev = p.swap(NullValue.encode(v))
      if (null == prev) {
        // None -> Some.  On commit, we leave +1 on the reference count
        sizeRef += 1
        txn.afterRollback(t => exit(k, p, 1))
      } else {
        // Some -> Some
        txn.afterCompletion(t => exit(k, p, 1))
      }
      NullValue.decodeOption(prev)
    } catch {
      case x => {
        exit(k, p, 1)
        throw x
      }
    }
  }

  def remove(k: A)(implicit txn: Txn): Option[B] = {
    val p = enter(k)
    try {
      val prev = p.swap(null)
      if (null != prev) {
        // Some -> None.  On commit, we erase the +1 that was left by the
        // None -> Some transition
        sizeRef -= 1
        txn.afterCompletion(t => exit(k, p, (if (txn.status == Txn.Committed) 2 else 1)))
      } else {
        // None -> None
        txn.afterCompletion(t => exit(k, p, 1))
      }
      NullValue.decodeOption(prev)
    } catch {
      case x => {
        exit(k, p, 1)
        throw x
      }
    }
  }

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
      fresh = new Pred[B]
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

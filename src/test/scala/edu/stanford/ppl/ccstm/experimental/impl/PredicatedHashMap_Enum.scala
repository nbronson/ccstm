/* CCSTM - (c) 2009 Stanford University - PPL */

// PredicatedHashMap_Basic

package edu.stanford.ppl.ccstm.experimental.impl

import edu.stanford.ppl.ccstm.experimental.TMap
import java.util.concurrent.ConcurrentHashMap
import edu.stanford.ppl.ccstm.experimental.TMap.Bound
import edu.stanford.ppl.ccstm.{STM, Txn}
import edu.stanford.ppl.ccstm.collection.{TIntRef, TOptionRef, LazyConflictIntRef}

class PredicatedHashMap_Enum[A,B] extends TMap[A,B] {
  def NumSizeStripes = 16

  private val sizeStripes = Array.fromFunction(i => new TIntRef(0))(NumSizeStripes)
  private val predicates = new ConcurrentHashMap[A,TOptionRef[B]]

  def nonTxn: Bound[A,B] = new TMap.AbstractNonTxnBound[A,B,PredicatedHashMap_Enum[A,B]](this) {

    override def size(): Int = {
      STM.atomic(unbind.size(_))
    }

    def get(key: A): Option[B] = {
      pred(key).nonTxn.get
    }

    private def myStripe: TIntRef = {
      sizeStripes(System.identityHashCode(Thread.currentThread) & (NumSizeStripes - 1))
    }

    override def put(key: A, value: B): Option[B] = {
      val p = pred(key)
      val pn = p.nonTxn
      val before = pn.get
      if (!before.isEmpty) {
        // try to update (no size change)
        if (pn.compareAndSet(before, Some(value))) {
          // success
          return before
        }
        // failure goes to the transform2 implementation
      }
      return STM.transform2(p, myStripe, (vo: Option[B], s: Int) => {
        (Some(value), (if (vo.isEmpty) s + 1 else s), vo)
      })
    }

    override def removeKey(key: A): Option[B] = {
      val p = pred(key)
      val before = p.nonTxn.get
      if (before.isEmpty) {
        // let's linearize right here!
        return None
      }
      return STM.transform2(p, myStripe, (vo: Option[B], s: Int) => {
        (None, (if (vo.isEmpty) s else s - 1), vo)
      })
    }

    protected def transformIfDefined(key: A,
                                     pfOrNull: PartialFunction[Option[B],Option[B]],
                                     f: Option[B] => Option[B]): Boolean = {
      val p = pred(key)
      val pn = p.nonTxn
      val before = pn.get
      if (null != pfOrNull && !pfOrNull.isDefinedAt(before)) {
        // no change
        return false
      }
      // make a CAS attempt
      val after = f(before)
      if (!before.isEmpty) {
        if (!after.isEmpty && pn.compareAndSet(before, after)) {
          // CAS success, and no size change
          return true
        }
        // failure goes to the transform2 implementation
      }
      return STM.transform2(p, myStripe, (vo: Option[B], s: Int) => {
        if (null != pfOrNull && (vo ne before) && !pfOrNull.isDefinedAt(vo)) {
          // do nothing and return false
          (vo, s, false)
        } else {
          // can we use the precomputed after?
          val a = if (vo eq before) after else f(vo)
          (a, s + (if (a.isEmpty) 0 else 1) - (if (vo.isEmpty) 0 else 1), true)
        }
      })
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

  def bind(implicit txn0: Txn): Bound[A, B] = new TMap.AbstractTxnBound[A,B,PredicatedHashMap_Enum[A,B]](txn0, this) {
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
          txn.forceRollback(Txn.InvalidReadCause(unbind, "PredicatedHashMap_Basic.Iterator missed elements"))
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
    // TODO: something better
    size > 0
  }

  def size(implicit txn: Txn): Int = {
    var i = 0
    var s = 0
    while (i < NumSizeStripes) {
      s += sizeStripes(i).get
      i += 1
    }
    s
  }

  def get(key: A)(implicit txn: Txn): Option[B] = {
    pred(key).get
  }

  private def myStripe(implicit txn: Txn): TIntRef = {
    sizeStripes(System.identityHashCode(txn) & (NumSizeStripes - 1))
  }

  def put(key: A, value: B)(implicit txn: Txn): Option[B] = {
    val prev = pred(key).getAndSet(Some(value))
    if (prev.isEmpty) myStripe.transform(_ + 1)
    prev
  }

  def removeKey(key: A)(implicit txn: Txn): Option[B] = {
    val prev = pred(key).getAndSet(None)
    if (!prev.isEmpty) myStripe.transform(_ - 1)
    prev
  }

  protected def transformIfDefined(key: A,
                                   pfOrNull: PartialFunction[Option[B],Option[B]],
                                   f: Option[B] => Option[B])(implicit txn: Txn): Boolean = {
    val p = pred(key)
    val prev = p.get
    if (null != pfOrNull && !pfOrNull.isDefinedAt(prev)) {
      false
    } else {
      val after = f(prev)
      p.set(after)
      val delta = (if (after.isEmpty) 0 else 1) - (if (prev.isEmpty) 0 else 1)
      if (delta != 0) myStripe.transform(_ + delta)
      true
    }
  }

  private def pred(key: A): TOptionRef[B] = {
    val pred = predicates.get(key)
    if (null != pred) pred else createPred(key)
  }

  private def createPred(key: A): TOptionRef[B] = {
    val fresh = new TOptionRef[B](None)
    val race = predicates.putIfAbsent(key, fresh)
    if (null != race) race else fresh
  }
}
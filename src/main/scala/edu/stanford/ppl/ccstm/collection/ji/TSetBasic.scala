/* CCSTM - (c) 2009 Stanford University - PPL */

// TSetBasic

package edu.stanford.ppl.ccstm.collection.ji

import java.util.concurrent.ConcurrentHashMap
import java.util._
import edu.stanford.ppl.ccstm._
import collection.TBooleanRef


object TSetBasic {
  trait Bound[A] extends Set[A] {
    def unbind: TSetBasic[A]
    def context: Option[Txn]
  }

  private class TxnBound[A](val unbind: TSetBasic[A], txn: Txn) extends AbstractSet[A] with Bound[A] {
    def context = Some(txn)

    def size: Int = unbind.size(txn)
    override def contains(key: Any): Boolean = unbind.contains(key)(txn)
    override def add(key: A): Boolean = unbind.add(key)(txn)
    override def remove(key: Any): Boolean = unbind.remove(key)(txn)

    def iterator: Iterator[A] = new Iterator[A] {
      private var apparentSize = 0
      private val iter = unbind._predicates.entrySet().iterator
      private var prev: Option[A] = None
      private var avail: Option[A] = null

      advance()
      if (txn.barging) {
        // auto-readForWrite will prevent the size from changing in another txn
        size
      }

      private def advance() {
        while (iter.hasNext) {
          val e = iter.next
          if (e.getValue.get(txn)) {
            apparentSize += 1
            avail = Some(e.getKey.asInstanceOf[A])
            return
          }
        }
        if (apparentSize != size) {
          txn.forceRollback(Txn.InvalidReadCause(unbind, "TSetBasic.Iterator missed elements"))
        }
        avail = None
        return
      }

      def hasNext: Boolean = !avail.isEmpty

      def next(): A = {
        prev = avail
        advance()
        prev.get
      }

      def remove() {
        if (TxnBound.this.remove(prev.get)) {
          apparentSize -= 1
        }
        prev = None
      }
    }
  }

  private class NonTxnBound[A](val unbind: TSetBasic[A]) extends AbstractSet[A] with Bound[A] {
    def context = None

    def size: Int = unbind._size.nonTxn.get

    override def contains(key: Any): Boolean = {
      unbind.ref(txn).nonTxn.get
    }

    override def add(key: A): Boolean = {
      // need to atomically update the size and the pred
      val r = unbind.ref(txn)
      !r.nonTxn.get && STM.atomic(t => unbind.addImpl(r)(t))
    }

    override def remove(key: Any): Boolean = {
      // need to atomically update the size and the pred
      val r = unbind.ref(txn)
      r.nonTxn.get && STM.atomic(t => unbind.removeImpl(r)(t))
    }

    def iterator: Iterator[A] = new Iterator[A] {
      private val iter = unbind._predicates.entrySet().iterator
      private var prev: Option[A] = None
      private var avail: Option[A] = null

      advance()

      private def advance() {
        while (iter.hasNext) {
          val e = iter.next
          if (e.getValue.nonTxn.get) {
            avail = Some(e.getKey.asInstanceOf[A])
            return
          }
        }
        avail = None
        return
      }

      def hasNext: Boolean = !avail.isEmpty

      def next(): A = {
        prev = avail
        advance()
        prev.get
      }

      def remove() {
        NonTxnBound.this.remove(prev.get)
        prev = None
      }
    }
  }
}

/** A transactional set that implements a <code>java.util.Set</code>-like
 *  interface using Transactional Predication, but that performs no garbage
 *  collection of entries that have been accessed but that are empty.
 */
class TSetBasic[A] {
  import TSetBasic._

  private val _size = new collection.LazyConflictIntRef(0) // replace with striped version
  private val _predicates = new ConcurrentHashMap[Any,TBooleanRef]

  def size(implicit txn: Txn): Int = _size.get

  def bind(implicit txn: Txn): Bound[A] = new TxnBound(this, txn)
  val nonTxn: Bound[A] = new NonTxnBound(this)

  def contains(key: Any)(implicit txn: Txn): Boolean = {
    ref(key).get
  }

  private def ref(key: Any): TBooleanRef = {
    if (null != ref) {
      ref
    } else {
      // we must create a ref so that we detect conflicting write-after-read
      val fresh = new TBooleanRef(false)
      val race = _predicates.putIfAbsent(key, fresh)
      if (null != race) race else fresh
    }
  }

  def add(key: A)(implicit txn: Txn): Boolean = {
    addImpl(ref(key))
  }

  private def addImpl(r: TBooleanRef)(implicit txn: Txn): Boolean = {
    if (!r.get) {
      r := true
      _size += 1
      true
    } else {
      false
    }
  }

  def remove(key: Any)(implicit txn: Txn): Boolean = {
    removeImpl(ref(key))
  }

  private def removeImpl(r: TBooleanRef)(implicit txn: Txn): Boolean = {
    if (r.get) {
      r := false
      _size -= 1
      true
    } else {
      false
    }
  }
}
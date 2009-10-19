/* CCSTM - (c) 2009 Stanford University - PPL */

// TSetGC

package edu.stanford.ppl.ccstm.collection.ji

import java.util.concurrent.ConcurrentHashMap
import java.util._
import edu.stanford.ppl.ccstm._
import collection.TAnyRef


object TSetGC {
  trait Bound[A] extends Set[A] {
    def unbind: TSetGC[A]
    def context: Option[Txn]
  }

  private class TxnBound[A](val unbind: TSetGC[A], txn: Txn) extends AbstractSet[A] with Bound[A] {
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
          if (e.getValue.get(txn) == Present) {
            apparentSize += 1
            avail = Some(e.getKey.asInstanceOf[A])
            return
          }
        }
        if (apparentSize != size) {
          txn.forceRollback(Txn.InvalidReadCause(unbind, "TSetGC.Iterator missed elements"))
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

  private class NonTxnBound[A](val unbind: TSetGC[A]) extends AbstractSet[A] with Bound[A] {
    def context = None

    def size: Int = unbind._size.nonTxn.get

    override def contains(key: Any): Boolean = containsImpl(unbind._predicates.get(key))

    private def containsImpl(pred: Predicate): Boolean = {
      null != pred && null != pred.nonTxn.get
    }

    override def add(key: A): Boolean = {
      val pred = unbind._predicates.get(key)
      !containsImpl(pred) && STM.atomic(t => unbind.addImpl(pred)(t))
    }

    override def remove(key: Any): Boolean = {
      val pred = unbind._predicates.get(key)
      containsImpl(pred) && STM.atomic(t => unbind.removeImpl(pred)(t))
    }

    def iterator: Iterator[A] = new Iterator[A] {
      private val iter = unbind._predicates.entrySet().iterator
      private var prev: Option[A] = None
      private var avail: Option[A] = null

      advance()

      private def advance() {
        while (iter.hasNext) {
          val e = iter.next
          if (null != e.getValue.nonTxn.get) {
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

  class Token(set: TSetGC[_], key: Any) {
    val pred = new Predicate(set, key, this)
  }

  class Predicate(set: TSetGC[_], val key: Any, token: Token) extends TAnyRef[Token](null) {
    val weak = new CleanableRef[Token](token) { def cleanup() { set.cleanup(Predicate.this) } }
  }
}

/** A transactional set that implements a <code>java.util.Set</code>-like
 *  interface using Transactional Predication, with garbage collection of empty
 *  accessed entries performed incrementally by using weak references and a
 *  shared cleanup thread.  This implementation is serializable, but does not
 *  provide opacity. Transactional reads may return different values with no
 *  intervening writes inside a transaction that rolls back. 
 */
class TSetGC[A](removeViaGC: Boolean) {
  import TSetGC._

  private val _size = new collection.LazyConflictIntRef(0) // replace with striped version
  private val _predicates = new ConcurrentHashMap[Any,Predicate]

  def size(implicit txn: Txn): Int = _size.get

  def bind(implicit txn: Txn): Bound[A] = new TxnBound(this, txn)
  val nonTxn: Bound[A] = new NonTxnBound(this)

  private[ji] def getOrCreateToken(key: AnyRef): Token = {
    while (true) {
      val pred = _predicates.get(key)
      if (null == pred) {
        // new predicate needed
        val fresh = new Token(this, key)
        if (null != _predicates.putIfAbsent(key, fresh.pred)) {
          return fresh
        }
      } else {
        val token = pred.weak.get
        if (null == token) {
          // new predicate needed
          val fresh = new Token(this, key)
          if (_predicates.replace(key, pred, fresh.pred)) {
            return fresh
          }
        } else {
          return token
        }
      }
    }
    throw new Error
  }

  def contains(key: Any)(implicit txn: Txn): Boolean = {
    null != getOrCreateToken(key).pred.get
  }

  def add(key: Any)(implicit txn: Txn): Boolean = addImpl(getOrCreateToken(key))

  private[ji] def addImpl(pred: Predicate)(implicit txn: Txn): Boolean = {
    val token = pred.weak.get
    addImpl(if (null != token) token else getOrCreateToken(pred.key))
  }

  private[ji] def addImpl(token: Token)(implicit txn: Txn): Boolean = {
    if (null != token.pred.get) {
      // already present
      false
    } else {
      _size += 1
      token.pred.set(token)
      true
    }
  }

  def remove(key: Any)(implicit txn: Txn): Boolean = removeImpl(getOrCreateToken(key))

  private[ji] def removeImpl(pred: Predicate)(implicit txn: Txn): Boolean = {
    val token = pred.weak.get
    removeImpl(if (null != token) token else getOrCreateToken(pred.key))
  }

  private[ji] def removeImpl(token: Token)(implicit txn: Txn): Boolean = {
    if (null == token.pred.get) {
      // already absent
      false
    } else {
      _size -= 1
      token.pred.set(null)
      true
    }
  }

  private def cleanup(pred: Predicate) {
    _predicates.remove(pred.key, pred)
    // no need to retry on failure
  }
}
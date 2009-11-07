/* CCSTM - (c) 2009 Stanford University - PPL */

// PredicatedSetGC

package edu.stanford.ppl.ccstm.collection.ji

import java.util.concurrent.ConcurrentHashMap
import java.util._
import edu.stanford.ppl.ccstm._
import collection.TAnyRef


object PredicatedSetGC {
  trait Bound[A] extends Set[A] {
    def unbind: PredicatedSetGC[A]
    def context: Option[Txn]
  }

  private class TxnBound[A](val unbind: PredicatedSetGC[A], txn: Txn) extends AbstractSet[A] with Bound[A] {
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
          if (null != e.getValue.get(txn)) {
            apparentSize += 1
            avail = Some(e.getKey.asInstanceOf[A])
            return
          }
        }
        if (apparentSize != size) {
          txn.forceRollback(Txn.InvalidReadCause(unbind, "PredicatedSetGC.Iterator missed elements"))
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

  private class NonTxnBound[A](val unbind: PredicatedSetGC[A]) extends AbstractSet[A] with Bound[A] {
    def context = None

    def size: Int = unbind._size.nonTxn.get

    override def contains(key: Any): Boolean = {
      val r = unbind._predicates.get(key)
      null != r && null != r.nonTxn.get
    }

    override def add(key: A): Boolean = {
      //!containsImpl(pred) && STM.atomic(t => unbind.addImpl(pred)(t))
      val r = unbind.ref(key)
      null == r.nonTxn.get && STM.transform2(r, unbind._size, (p: Boolean, s: Int) => {
        (true, (if (p) s else s + 1), !p)
      })
    }

    override def remove(key: Any): Boolean = {
      //containsImpl(pred) && STM.atomic(t => unbind.removeImpl(pred)(t))
      val r = unbind._predicates.get(key)
      null != r && null != r.nonTxn.get && STM.transform2(r, unbind._size, (p: Boolean, s: Int) => {
        (false, (if (p) s - 1 else s), p)
      })
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

  class Token(set: PredicatedSetGC[_], key: Any) {
    val pred = new Predicate(set, key, this)
  }

  class Predicate(set: PredicatedSetGC[_], val key: Any, token: Token) extends TAnyRef[Token](null) {
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
class PredicatedSetGC[A] {
  import PredicatedSetGC._

  private val _size = new collection.LazyConflictIntRef(0) // replace with striped version
  private val _predicates = new ConcurrentHashMap[Any,Predicate]

  def size(implicit txn: Txn): Int = _size.get

  def bind(implicit txn: Txn): Bound[A] = new TxnBound(this, txn)
  val nonTxn: Bound[A] = new NonTxnBound(this)

  private[ji] def getOrCreateToken(key: Any): Token = {
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
    val token = getOrCreateToken(key)
    if (null != token.pred.get) {
      // ref will survive until a change, so any read by this txn of a new ref
      // can only happen if the current ref read is invalid, and this txn can't
      // commit
      true
    } else {
      // we must pin this ref until we commit
      txn.addReference(token)
      false
    }
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
      // the strong reference in the write buffer will pin the token
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
      // already absent, make sure that we can't commit unless it stays that
      // way, by making sure that the token is alive until completion
      txn.addReference(token)
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
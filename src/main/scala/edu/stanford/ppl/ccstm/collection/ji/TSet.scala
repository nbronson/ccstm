/* CCSTM - (c) 2009 Stanford University - PPL */

// TSet

package edu.stanford.ppl.ccstm.collection.ji

import java.lang.ref._
import java.util.concurrent.ConcurrentHashMap
import java.util._


object TSet {
  trait Bound[A] extends Set[A] {
    def unbind: TSet[A]
    def context: Option[Txn]
  }

  private class TxnBound[A](val unbind: TSet[A], txn: Txn) extends AbstractSet[A] with Bound[A] {
    def context = Some(txn)

    def size: Int = unbind.size(txn)
    override def contains(key: Any): Boolean = unbind.contains(key)(txn)
    override def add(key: A): Boolean = unbind.add(key)(txn)
    override def remove(key: Any): Boolean = unbind.remove(key)(txn)

    def iterator: Iterator[A] = new Iterator[A] {
      private var apparentSize = 0
      private val preds = unbind._predicates.values.iterator
      private var prev: Option[A] = None
      private var avail: Option[A] = null

      advance()
      if (txn.barging) {
        // auto-readForWrite will prevent the size from changing in another txn
        size
      }

      private def advance() {
        while (preds.hasNext) {
          val p = preds.next
          if (null != p.strong.get(txn)) {
            apparentSize += 1
            avail = Some(p.key.asInstanceOf[A])
            return
          }
        }
        if (apparentSize != size) {
          txn.forceRollback(Txn.InvalidReadCause(unbind, "TSet.Iterator missed elements"))
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

  private class NonTxnBound[A](val unbind: TSet[A]) extends AbstractSet[A] with Bound[A] {
    def context = None

    def size: Int = unbind._size.nonTxn.get

    override def contains(key: Any): Boolean = {
      val pred = unbind._predicates.get(key)
      null != pred && null != pred.strong.nonTxn.get
    }

    override def add(key: A): Boolean = {
      // need to atomically update the size and the pred
      !contains(key) && STM.atomic(t => unbind.add(key)(t))
    }

    override def remove(key: Any): Boolean = {
      // need to atomically update the size and the pred
      contains(key) && STM.atomic(t => unbind.remove(key)(t))
    }

    def iterator: Iterator[A] = new Iterator[A] {
      private val preds = unbind._predicates.values.iterator
      private var prev: Option[A] = None
      private var avail: Option[A] = null

      advance()

      private def advance() {
        while (preds.hasNext) {
          val p = preds.next
          if (null != p.strong.nonTxn.get) {
            avail = Some(p.key.asInstanceOf[A])
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

  private class Token(predicates: ConcurrentHashMap[Any,Pred], key: Any) {
    val pred = new Pred(predicates, key, this)
  }

  private class Pred(predicates: ConcurrentHashMap[Any,Pred], val key: Any, tok: Token) extends CleanableRef(tok) {
    val strong = Ref[Token](null)

    def cleanup() {
      predicates.remove(key, this)
    }
  }
}

/** A transactional set that implements a <code>java.util.Set</code>-like
 *  interface.
 */
class TSet[A] {
  import TSet._

  /*private*/ val _size = new TIntRef(0) // replace with striped version
  /*private*/ val _predicates = new ConcurrentHashMap[Any,Pred]

  def size(implicit txn: Txn): Int = _size.get

  def bind(implicit txn: Txn): Bound[A] = new TxnBound(this, txn)
  val nonTxn: Bound[A] = new NonTxnBound(this)

  def contains(key: Any)(implicit txn: Txn): Boolean = {
    val tok = token(key)
    if (null == tok.pred.strong.get) {
      // The lifetime of this predicate is defined by the Token, so we need to
      // make sure that the Token cannot be garbage collected before this Txn
      // is completed.  That wil ensure that any conflicting access must agree
      // on the predicate, so we will correctly detect conflict.
      txn.addReference(tok)
      false
    } else {
      // Present, there is a strong reference to the token in pred.strong.  Any
      // transaction that changes this must affect this predicate, so even if
      // the Token is subsequently garbage collected the predicate's version
      // wil have changed.
      true
    }
  }

  def add(key: A)(implicit txn: Txn): Boolean = {
    val tok = token(key)
    if (null == tok.pred.strong.get) {
      // not previously present
      tok.pred.strong := tok
      _size.transform(_ + 1)
      true
    } else {
      // already present
      false
    }
  }

  def remove(key: Any)(implicit txn: Txn): Boolean = {
    val tok = token(key)
    if (null != tok.pred.strong.get) {
      // previously present
      tok.pred.strong := null
      _size.transform(_ - 1)
      true
    } else {
      // Already absent.  The return value is equivalent to a contains()
      // observation.
      txn.addReference(tok)
      false
    }
  }

  private def token(key: Any): Token = {
    var tok: Token = null
    while (null == tok) {
      val pred = _predicates.get(key)
      if (null == pred) {
        val freshTok = new Token(_predicates, key)
        if (null == _predicates.putIfAbsent(key, freshTok.pred)) {
          // successful
          tok = freshTok
        }
      } else {
        tok = pred.get
        if (null == tok) {
          // stale predicate has not yet been cleaned
          val freshTok = new Token(_predicates, key)
          if (_predicates.replace(key, pred, freshTok.pred)) {
            // successful
            tok = freshTok
          }
        }
      }
    }
    tok
  }
}
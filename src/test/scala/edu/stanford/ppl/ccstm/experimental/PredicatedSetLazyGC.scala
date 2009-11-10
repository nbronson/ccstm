/* CCSTM - (c) 2009 Stanford University - PPL */

// PredicatedSetLazyGC

package edu.stanford.ppl.ccstm.experimental

import java.util.concurrent.ConcurrentHashMap
import java.util._
import edu.stanford.ppl.ccstm._
import collection.TAnyRef


//object PredicatedSetLazyGC {
//  trait Bound[A] extends Set[A] {
//    def unbind: PredicatedSetLazyGC[A]
//    def context: Option[Txn]
//  }
//
//  private class TxnBound[A](val unbind: PredicatedSetLazyGC[A], txn: Txn) extends AbstractSet[A] with Bound[A] {
//    def context = Some(txn)
//
//    def size: Int = unbind.size(txn)
//    override def contains(key: Any): Boolean = unbind.contains(key)(txn)
//    override def add(key: A): Boolean = unbind.add(key)(txn)
//    override def remove(key: Any): Boolean = unbind.remove(key)(txn)
//
//    def iterator: Iterator[A] = new Iterator[A] {
//      private var apparentSize = 0
//      private val iter = unbind._predicates.entrySet().iterator
//      private var prev: Option[A] = None
//      private var avail: Option[A] = null
//
//      advance()
//      if (txn.barging) {
//        // auto-readForWrite will prevent the size from changing in another txn
//        size
//      }
//
//      private def advance() {
//        while (iter.hasNext) {
//          val e = iter.next
//          if (null != e.getValue.get(txn)) {
//            apparentSize += 1
//            avail = Some(e.getKey.asInstanceOf[A])
//            return
//          }
//        }
//        if (apparentSize != size) {
//          txn.forceRollback(Txn.InvalidReadCause(unbind, "PredicatedSetLazyGC.Iterator missed elements"))
//        }
//        avail = None
//        return
//      }
//
//      def hasNext: Boolean = !avail.isEmpty
//
//      def next(): A = {
//        prev = avail
//        advance()
//        prev.get
//      }
//
//      def remove() {
//        if (TxnBound.this.remove(prev.get)) {
//          apparentSize -= 1
//        }
//        prev = None
//      }
//    }
//  }
//
//  private class NonTxnBound[A](val unbind: PredicatedSetLazyGC[A]) extends AbstractSet[A] with Bound[A] {
//    def context = None
//
//    def size: Int = unbind._size.nonTxn.get
//
//    override def contains(key: Any): Boolean = containsImpl(unbind._predicates.get(key))
//
//    private def containsImpl(pred: Predicate): Boolean = {
//      null != pred && null != pred.nonTxn.get
//    }
//
//    override def add(key: A): Boolean = {
//      val pred = unbind._predicates.get(key)
//      !containsImpl(pred) && STM.atomic(t => unbind.addImpl(pred)(t))
//    }
//
//    override def remove(key: Any): Boolean = {
//      val pred = unbind._predicates.get(key)
//      containsImpl(pred) && STM.atomic(t => unbind.removeImpl(pred)(t))
//    }
//
//    def iterator: Iterator[A] = new Iterator[A] {
//      private val iter = unbind._predicates.entrySet().iterator
//      private var prev: Option[A] = None
//      private var avail: Option[A] = null
//
//      advance()
//
//      private def advance() {
//        while (iter.hasNext) {
//          val e = iter.next
//          if (null != e.getValue.nonTxn.get) {
//            avail = Some(e.getKey.asInstanceOf[A])
//            return
//          }
//        }
//        avail = None
//        return
//      }
//
//      def hasNext: Boolean = !avail.isEmpty
//
//      def next(): A = {
//        prev = avail
//        advance()
//        prev.get
//      }
//
//      def remove() {
//        NonTxnBound.this.remove(prev.get)
//        prev = None
//      }
//    }
//  }
//
//  class Token(set: PredicatedSetLazyGC[_], key: Any, inserting: Boolean) {
//    val pred = new Predicate(set, key, this, inserting)
//  }
//
//  class TokenRef(token: Token, set: PredicatedSetLazyGC[_], pred: Predicate) extends CleanableRef[Token](token) {
//    def cleanup() {
//      set.weakCleanup(pred)
//    }
//  }
//
//  class Predicate(set: PredicatedSetLazyGC[_], val key: Any, token: Token, inserting: Boolean) extends TAnyRef[Token](null) {
//
//    @volatile var ref: AnyRef = { if (inserting) token else new TokenRef(token, set, this) }
//
//    def token(inserting: Boolean): Token = {
//      // TODO: use inserting
//      ref match {
//        case null => {
//          // predicate is pending removal, can't be reused
//          null
//        }
//        case t: Token => {
//          if (inserting) {
//            t
//          } else {
//            // token has so far only been used by observers that think that the
//            // predicate is true
//            synchronized {
//              ref match {
//                case null => {
//                  null
//                }
//                case t: Token => {
//                  ref = new TokenRef(t, set, pred)
//                  t
//                }
//                case r: TokenRef => {
//                  r.get
//                }
//              }
//            }
//          }
//        }
//        case r: TokenRef => {
//          // already weakened
//          r.get
//        }
//      }
//    }
//  }
//}
//
///** A transactional set that implements a <code>java.util.Set</code>-like
// *  interface using Transactional Predication, with garbage collection of empty
// *  accessed entries performed incrementally by using weak references and a
// *  shared cleanup thread.  This implementation is serializable, but does not
// *  provide opacity. Transactional reads may return different values with no
// *  intervening writes inside a transaction that rolls back.
// */
//class PredicatedSetLazyGC[A] {
//  import PredicatedSetLazyGC._
//
//  private val _size = new collection.LazyConflictIntRef(0) // replace with striped version
//  private val _predicates = new ConcurrentHashMap[Any,Predicate]
//
//  def size(implicit txn: Txn): Int = _size.get
//
//  def bind(implicit txn: Txn): Bound[A] = new TxnBound(this, txn)
//  val nonTxn: Bound[A] = new NonTxnBound(this)
//
//  private[experimental] def getOrCreateToken(key: Any, inserting: Boolean): Token = {
//    while (true) {
//      val pred = _predicates.get(key)
//      if (null == pred) {
//        // new predicate needed
//        val fresh = new Token(this, key, inserting)
//        if (null != _predicates.putIfAbsent(key, fresh.pred)) {
//          return fresh
//        }
//      } else {
//        val token = pred.token(inserting)
//        if (null == token) {
//          // new predicate needed
//          val fresh = new Token(this, key, inserting)
//          if (_predicates.replace(key, pred, fresh.pred)) {
//            return fresh
//          }
//        } else {
//          return token
//        }
//      }
//    }
//    throw new Error
//  }
//
//  def contains(key: Any)(implicit txn: Txn): Boolean = {
//    val token = getOrCreateToken(key, false)
//    if (null != token.pred.get) {
//      // ref will survive until a change, so any read by this txn of a new ref
//      // can only happen if the current ref read is invalid, and this txn can't
//      // commit
//      true
//    } else {
//      // we must pin this ref until we commit
//      txn.addReference(token)
//      false
//    }
//  }
//
//  def add(key: Any)(implicit txn: Txn): Boolean = addImpl(getOrCreateToken(key, true))
//
//  private[experimental] def addImpl(pred: Predicate)(implicit txn: Txn): Boolean = {
//    val token = pred.token(true)
//    addImpl(if (null != token) token else getOrCreateToken(pred.key, true))
//  }
//
//  private[experimental] def addImpl(token: Token)(implicit txn: Txn): Boolean = {
//    val pred = token.pred
//    if (null != pred.get) {
//      // already present
//      false
//    } else {
//      // the strong reference in the write buffer will pin the token
//      _size += 1
//      pred.set(token)
//
//      // if we have avoided making a weak reference to the token, then we are
//      // responsible for removing the key if this txn rolls back
//      if (pred.ref eq token) {
//        txn.afterRollback((t: Txn) => {
//          tryStrongCleanup(pred)
//        })
//      }
//      true
//    }
//  }
//
//  def remove(key: Any)(implicit txn: Txn): Boolean = removeImpl(getOrCreateToken(key))
//
//  private[experimental] def removeImpl(pred: Predicate)(implicit txn: Txn): Boolean = {
//    val token = pred.weak.get
//    removeImpl(if (null != token) token else getOrCreateToken(pred.key))
//  }
//
//  private[experimental] def removeImpl(token: Token)(implicit txn: Txn): Boolean = {
//    if (null == token.pred.get) {
//      // already absent, make sure it stays that way
//      txn.addReference(token)
//      false
//    } else {
//      _size -= 1
//      token.pred.set(null)
//      true
//    }
//  }
//
//  private def tryStrongCleanup(pred: Predicate) {
//    if (!pred.ref.isInstanceOf[Token]) return
//    pred.synchronized {
//      if (!pred.ref.isInstanceOf[Token]) return
//      pred.ref = null
//    }
//    _predicates.remove(pred.key, pred)
//  }
//
//  private def weakCleanup(pred: Predicate) {
//    _predicates.remove(pred.key, pred)
//    // no need to retry on failure
//  }
//}

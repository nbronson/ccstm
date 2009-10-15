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
      private val iter = unbind._predicates.entrySet.iterator
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
      val ref = unbind._predicates.get(key)
      null != ref && ref.nonTxn.get == Present
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
      private val iter = unbind._predicates.entrySet.iterator
      private var prev: Option[A] = None
      private var avail: Option[A] = null

      advance()

      private def advance() {
        while (iter.hasNext) {
          val e = iter.next
          if (e.getValue.nonTxn.get == Present) {
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

  trait State

  object Present extends State
  
  class Absent(set: TSet[_], key: Any) {
    val observer = new ObservedAbsent(set, key, this)
  }

  case class ObservedAbsent(set: TSet[_], key: Any, absent: Absent) extends CleanableRef[Absent](absent) with State {
    def cleanup() { set.cleanup(this) }
  }

  object Invalid extends State

  // unobserved-absent is the implicit state
}

/** A transactional set that implements a <code>java.util.Set</code>-like
 *  interface.
 */
class TSet[A] {
  import TSet._

  private val _size = new LazyConflictIntRef(0) // replace with striped version
  private val _predicates = new ConcurrentHashMap[Any,Ref[State]]

  def size(implicit txn: Txn): Int = _size.get

  def bind(implicit txn: Txn): Bound[A] = new TxnBound(this, txn)
  val nonTxn: Bound[A] = new NonTxnBound(this)

  def contains(key: Any)(implicit txn: Txn): Boolean = {
    while (true) {
      val ref = _predicates.get(key)
      if (null == ref) {
        // unobserved-absent, make it observed
        val absent = new Absent(this, key)
        if (null == _predicates.putIfAbsent(key, Ref(absent.observer))) {
          txn.addReference(absent)
          return false
        }
      } else {
        ref.get match {
          case Present => {
            // easy
            return true
          }
          case obs: ObservedAbsent => {
            val absent = obs.get
            if (null != absent) {
              // active observed-absent
              txn.addReference(absent)
              return false
            } else {
              // We must replace the observer, but we can reuse the Ref
              val fresh = new Absent(this, key)
              ref.set(fresh.observer)
              txn.addReference(fresh)
              return false
            }
          }
          case Invalid => {
            // try to transition directly to unobserved-absent, with a new Ref
            val absent = new Absent(this, key)
            if (_predicates.replace(key, ref, Ref(absent.observer))) {
              txn.addReference(absent)
              return false
            }
          }
        }
      }
    }
    throw new Error("unreachable")
  }

  def add(key: A)(implicit txn: Txn): Boolean = {
    while (true) {
      val ref = _predicates.get(key)
      if (null == ref) {
        // unobserved-absent
        if (null == _predicates.putIfAbsent(key, Ref(Present))) {
          // success, and not previously present
          _size += 1
          return true
        }
      } else {
        ref.get match {
          case Present => {
            // success, no change
            return false
          }
          case obs: ObservedAbsent => {
            // doesn't matter whether or not it is reclaimed
            ref.set(Present)
            _size += 1
            return true
          }
          case Invalid => {
            // try to transition directly to present, with a new Ref
            if (_predicates.replace(key, ref, Ref(Present))) {
              _size += 1
              return true
            }
          }
        }
      }
    }
    throw new Error("unreachable")
  }

  def remove(key: Any)(implicit txn: Txn): Boolean = {
    if (contains(key)) {
      _predicates.remove(key)
      _size -= 1
      true
    } else {
      false
    }
  }

  private def cleanup(obs: ObservedAbsent) {
    val ref = _predicates.get(obs.key)
    if (null != ref && ref.nonTxn.compareAndSet(obs, Invalid)) {
      // we now have permission to remove the Ref, if it is still there
      _predicates.remove(obs.key, ref)
    }
    // no need to retry on failure
  }
}
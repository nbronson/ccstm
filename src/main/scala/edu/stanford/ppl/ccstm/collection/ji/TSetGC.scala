/* CCSTM - (c) 2009 Stanford University - PPL */

// TSetGC

package edu.stanford.ppl.ccstm.collection.ji

import java.util.concurrent.ConcurrentHashMap
import java.util._
import edu.stanford.ppl.ccstm._


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

    override def contains(key: Any): Boolean = {
      containsImpl(unbind._predicates.get(key))
    }

    private def containsImpl(ref: Ref[State]): Boolean = {
      null != ref && (ref.nonTxn.get eq Present)
    }

    override def add(key: A): Boolean = {
      val ref = unbind._predicates.get(key)
      !containsImpl(ref) && STM.atomic(t => unbind.addImpl(key, ref)(t))
    }

    override def remove(key: Any): Boolean = {
      val ref = unbind._predicates.get(key)
      containsImpl(ref) && STM.atomic(t => unbind.removeImpl(key, ref)(t))
    }

    def iterator: Iterator[A] = new Iterator[A] {
      private val iter = unbind._predicates.entrySet().iterator
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
  
  class Absent(set: TSetGC[_], key: Any) {
    val observer = new ObservedAbsent(set, key, this)
  }

  case class ObservedAbsent(set: TSetGC[_], key: Any, absent: Absent) extends CleanableRef[Absent](absent) with State {
    def cleanup() { set.cleanup(this) }
  }

  object Invalid extends State

  // unobserved-absent is the implicit state
}

/** A transactional set that implements a <code>java.util.Set</code>-like
 *  interface using Transactional Predication, with garbage collection of empty
 *  accessed entries performed incrementally by using weak references and a
 *  shared cleanup thread.  This implementation is serializable, but does not
 *  provide opacity. Transactional reads may return different values with no
 *  intervening writes inside a transaction that rolls back. 
 */
class TSetGC[A] {
  import TSetGC._

  private val _size = new collection.LazyConflictIntRef(0) // replace with striped version
  private val _predicates = new ConcurrentHashMap[Any,Ref[State]]

  def size(implicit txn: Txn): Int = _size.get

  def bind(implicit txn: Txn): Bound[A] = new TxnBound(this, txn)
  val nonTxn: Bound[A] = new NonTxnBound(this)

  def contains(key: Any)(implicit txn: Txn): Boolean = {
    null != containsImpl(key, _predicates.get(key))
  }

  private[ji] def createRef(key: Any, initialValue: State)(implicit txn: Txn): Ref[State] = {
    val fresh = Ref(initialValue)
    fresh.get
    fresh
  }

  /** Returns Ref[Present] or null. */
  private def containsImpl(key: Any, ref0: Ref[State])(implicit txn: Txn): Ref[State] = {
    var ref = ref0
    while (true) {
      if (null == ref) {
        // unobserved-absent, make it observed
        val absent = new Absent(this, key)
        if (null == _predicates.putIfAbsent(key, createRef(absent.observer))) {
          txn.addReference(absent)
          return null
        }
      } else {
        ref.get match {
          case Present => {
            // easy
            return ref
          }
          case obs: ObservedAbsent => {
            val absent = obs.get
            if (null != absent) {
              // active observed-absent
              txn.addReference(absent)
              return null
            } else {
              // We must replace the observer, but we can reuse the Ref
              val absent = new Absent(this, key)
              ref.set(absent.observer)
              txn.addReference(absent)
              return null
            }
          }
          case Invalid => {
            // try to transition directly to unobserved-absent, with a new Ref
            val absent = new Absent(this, key)
            if (_predicates.replace(key, ref, createRef(absent.observer))) {
              txn.addReference(absent)
              return null
            }
          }
        }
      }
      ref = _predicates.get(key)
    }
    throw new Error("unreachable")
  }

  def add(key: A)(implicit txn: Txn): Boolean = {
    addImpl(key, _predicates.get(key))
  }

  private def addImpl(key: A, ref0: Ref[State])(implicit txn: Txn): Boolean = {
    var ref = ref0
    while (true) {
      if (null == ref) {
        // unobserved-absent
        if (null == _predicates.putIfAbsent(key, createRef(Present))) {
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
            if (_predicates.replace(key, ref, createRef(Present))) {
              _size += 1
              return true
            }
          }
        }
      }
      ref = _predicates.get(key)
    }
    throw new Error("unreachable")
  }

  def remove(key: Any)(implicit txn: Txn): Boolean = {
    removeImpl(key, _predicates(key))
  }

  private def removeImpl(key: Any, ref0: Ref[State])(implicit txn: Txn): Boolean = {
    var ref = containsImpl(key, ref0)
    if (null != ref) {
      // TODO: we can't remove immediately
      // TODO:  option 1: store UnobservedAbsent, defer removal until commit
      // TODO:  option 2: store ObservedAbsent, let GC remove it later
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
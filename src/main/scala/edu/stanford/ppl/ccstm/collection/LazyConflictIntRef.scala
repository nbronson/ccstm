/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// LazyConflictIntRef

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._


object LazyConflictIntRef {
  
  private abstract class HistoryNode extends PartialFunction[Int,Int] {
    var next: HistoryNode = null
  }
  private abstract class Update extends HistoryNode {
    def isDefinedAt(v: Int) = true
  }
  private abstract class Test extends HistoryNode {
    def apply(v: Int) = v
  }

  private class Head extends HistoryNode {
    def isDefinedAt(v: Int): Boolean = throw new UnsupportedOperationException
    def apply(v: Int): Int = throw new UnsupportedOperationException
  }

  private case class Op_<(rhs: Int)                 extends Test { def isDefinedAt(v: Int) = (v < rhs) }
  private case class Op_>(rhs: Int)                 extends Test { def isDefinedAt(v: Int) = (v > rhs) }
  private case class Op_<=(rhs: Int)                extends Test { def isDefinedAt(v: Int) = (v <= rhs) }
  private case class Op_>=(rhs: Int)                extends Test { def isDefinedAt(v: Int) = (v >= rhs) }
  private case class Op_==(rhs: Int)                extends Test { def isDefinedAt(v: Int) = (v == rhs) }
  private case class Op_!=(rhs: Int)                extends Test { def isDefinedAt(v: Int) = (v != rhs) }

  private case class Map[Z](f: Int => Z, result: Z) extends Test { def isDefinedAt(v: Int) = (f(v) == result) }

  private case class CheckIsDefined(pf: PartialFunction[Int,Int], applicable: Boolean) extends Test {
    def isDefinedAt(v: Int) = (pf.isDefinedAt(v) == applicable)
  }

  private class ReleasableGet(value: Int)           extends Test {
    var active = true
    def isDefinedAt(v: Int) = !active || (value == v)
  }

  private class Set(value: Int)                extends Update { def apply(v: Int) = value }
  private class Incr(var delta: Int)           extends Update { def apply(v: Int) = (v + delta) }
  private class Transform(f: Int => Int)       extends Update { def apply(v: Int) = f(v) }


  class TxnBound(txn: Txn, val unbind: LazyConflictIntRef) extends IntRef.Bound with Txn.ReadResource {
    private val _ubound = unbind.underlying.bind(txn)
    private var _read: UnrecordedRead[Int] = _ubound.unrecordedRead
    private var _value: Int = _read.value
    private val _head = new Head
    private var _tail: HistoryNode = _head

    private var _updaterRegistered = false

    def context: Option[Txn] = Some(txn)

    //////////////// Source

    def get: Int = {
      record(new Op_==(_value))
      _value
    }

    def map[Z](f: Int => Z): Z = {
      val z = f(_value)
      record(new Map(f, z))
      z
    }

    def await(pred: Int => Boolean) {
      if (!map(pred)) {
        // record underlying to read set, then retry
        _ubound.get
        txn.retry()
      }
    }

    def releasableRead: ReleasableRead[Int] = {
      val h = new ReleasableGet(_value)
      record(h)
      new ReleasableRead[Int] {
        def context: Option[Txn] = Some(txn)
        val value: Int = _value
        def release() { h.active = false }
      }
    }

    def unrecordedRead: UnrecordedRead[Int] = new UnrecordedRead[Int] {
      def context: Option[Txn] = Some(txn)
      val value = _value
      def stillValid = _read.stillValid
      def recorded = _read.recorded
    }

    //////////////// Sink

    def set(v: Int) {
      _value = v
      record(new Set(v))
    }

    def tryWrite(v: Int): Boolean = {
      set(v)
      true
    }

    //////////////// read+write

    def readForWrite: Int = get

    def getAndSet(v: Int): Int = {
      val z = get
      set(v)
      z
    }

    def compareAndSet(before: Int, after: Int): Boolean = {
      if (this ==! before) {
        set(after)
        true
      } else {
        false
      }
    }

    def weakCompareAndSet(before: Int, after: Int): Boolean = compareAndSet(before, after)

    def compareAndSetIdentity[A <: Int with AnyRef](before: A, after: Int): Boolean = {
      throw new UnsupportedOperationException("identity comparisons are not valid on Int")
    }
    def weakCompareAndSetIdentity[A <: Int with AnyRef](before: A, after: Int): Boolean = {
      throw new UnsupportedOperationException("identity comparisons are not valid on Int")
    }

    def transform(f: (Int) => Int) {
      _value = f(_value)
      record(new Transform(f))
    }

    def getAndTransform(f: (Int) => Int): Int = {
      val z = get
      set(f(z))
      z
    }

    def tryTransform(f: (Int) => Int): Boolean = {
      transform(f)
      true
    }

    def transformIfDefined(pf: PartialFunction[Int, Int]): Boolean = {
      val a = pf.isDefinedAt(_value)
      record(new CheckIsDefined(pf, a))
      if (a) {
        transform(pf)
      }
      a
    }

    //////////////// convenience functions for int

    override def += (delta: Int) {
      if (delta != 0) {
        _value += delta
        _tail match {
          case incr: Incr => incr.delta += delta
          case _ => record(new Incr(delta))
        }
      }
    }

    override def compare(rhs: Int): Int = map((_ compare rhs))

    override def < (rhs: Int): Boolean = {
      if (_value < rhs) {
        record(new Op_<(rhs))
        true
      } else {
        record(new Op_>=(rhs))
        false
      }
    }
    
    override def > (rhs: Int): Boolean = {
      if (_value > rhs) {
        record(new Op_>(rhs))
        true
      } else {
        record(new Op_<=(rhs))
        false
      }
    }

    override def ==! (rhs: Int): Boolean = {
      if (_value == rhs) {
        record(new Op_==(rhs))
        true
      } else {
        record(new Op_!=(rhs))
        false
      }
    }

    //////////////// history replay implementation

    private def validateIfRequired() {
      if (!_read.stillValid) {
        // Since we are reusing the old unrecordedRead, we need to manually
        // trigger a revalidation.  This provides opacity.
        txn.explicitlyValidateReads()
      }
    }

    private def append(h: HistoryNode) {
      _tail.next = h
      _tail = h
    }

    private def record(h: Test) {
      if (h != _tail) append(h)
      validateIfRequired()
    }

    private def record(h: Update) {
      if (!_updaterRegistered) {
        // we can either start doing direct writes or register a deferred
        // update callback
        registerUpdater()
      }
      append(h)
      validateIfRequired()
    }

    private def registerUpdater() {
      _updaterRegistered = true
      txn.beforeCommit(t => {
        if (_value != _read.value) {
          _ubound := _value
        }
        // clearing the flag here means that if a later beforeCommit callback
        // does another store on this ref, we will register a new handler and
        // do the underlying update again
        _updaterRegistered = false
      }, Int.MaxValue  / 2)      
    }

    def valid(txn: Txn): Boolean = {
      if (!_read.stillValid) {
        _read = _ubound.unrecordedRead
        _value = _read.value
        var node: HistoryNode = _head.next
        while (null != node) {
          if (!node.isDefinedAt(_value)) return false
          _value = node(_value)
          node = node.next
        }
      }
      true
    }
  }
}

/** An IntRef implementation that uses Abstract Nested Transactions to
 *  minimize the need to roll back a transaction due to a conflict.  All
 *  operations and their result values are recorded.  If another transaction
 *  changes the value of this ref, a conflict will be avoided if the history
 *  can be replayed starting with the new value without an observable
 *  difference.  Writes to the underlying value will be delayed until just
 *  prior to commit.
 *
 *  As an example, if the current value of a LazyConflictIntRef x is 10, and
 *  the caller tests the value using <code>x > 4</code>, a thread-local history
 *  will be created that records that the transaction has observed that x is
 *  greater than 4.  If another transaction then changes x to 9, the current
 *  transaction can avoid rollback, because replaying the operations that were
 *  used to obtain information about the value does not result in an observable
 *  difference.  If a call is made to <code>x.get</code>, then clearly no
 *  conflicts can be avoided (unless there was an earlier call to
 *  <code>x.set</code> in the same transaction). 
 */
class LazyConflictIntRef(initialValue: Int) extends IntRef {

  private val underlying = new TIntRef(initialValue)

  private val bound = new TxnLocal[LazyConflictIntRef.TxnBound] {
    override def initialValue(txn: Txn) = {
      val b = new LazyConflictIntRef.TxnBound(txn, LazyConflictIntRef.this)
      txn.addReadResource(b, Int.MinValue / 2)
      b
    }
  }

  protected def handle: impl.Handle[Int] = throw new UnsupportedOperationException
  override private[ccstm] def nonTxnHandle = underlying.nonTxnHandle

  override def get(implicit txn: Txn): Int = bind.get
  override def map[Z](f: (Int) => Z)(implicit txn: Txn): Z = bind.map(f)
  override def set(v: Int)(implicit txn: Txn) { bind.set(v) }
  override def getAndSet(v: Int)(implicit txn: Txn): Int = bind.getAndSet(v)
  override def transform(f: (Int) => Int)(implicit txn: Txn) { bind.transform(f) }
  override def transformIfDefined(pf: PartialFunction[Int, Int])(implicit txn: Txn): Boolean = {
    bind.transformIfDefined(pf)
  }

  override def bind(implicit txn: Txn): IntRef.Bound = bound.get

  //////////////// txn int functions forward to bound versions

  override def +=  (delta: Int)(implicit txn: Txn) { bind += delta }
  override def compare(rhs: Int)(implicit txn: Txn): Int = { bind compare rhs }
  override def <   (rhs: Int)(implicit txn: Txn): Boolean = { bind < rhs }
  override def >   (rhs: Int)(implicit txn: Txn): Boolean = { bind > rhs }
  override def ==! (rhs: Int)(implicit txn: Txn): Boolean = { bind ==! rhs }

  //////////////// equality stuff in Ref uses handles, must be overriden:

  override def hashCode: Int = {
    System.identityHashCode(this)
  }

  override def equals(rhs: Any): Boolean = {
    this eq rhs.asInstanceOf[AnyRef]
  }
}

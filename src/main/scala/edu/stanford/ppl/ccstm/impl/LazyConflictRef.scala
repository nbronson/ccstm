/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// LazyConflictRef[T]

package edu.stanford.ppl.ccstm
package impl

import math.Numeric

private object LazyConflictRef {
  
  private abstract class HistoryNode[T] extends PartialFunction[T,T] {
    var next: HistoryNode[T] = null
  }

  private abstract class Update[T] extends HistoryNode[T] {
    def isDefinedAt(v: T) = true
  }

  private abstract class Test[T] extends HistoryNode[T] {
    def apply(v: T) = v
  }

  private class Head[T] extends HistoryNode[T] {
    def isDefinedAt(v: T): Boolean = throw new UnsupportedOperationException
    def apply(v: T): T = throw new UnsupportedOperationException
  }

  private case class Op_==[T](rhs: T) extends Test[T] { def isDefinedAt(v: T) = (v == rhs) }

  private case class GetWith[T,Z](f: T => Z, result: Z) extends Test[T] { def isDefinedAt(v: T) = (f(v) == result) }

  private case class CheckIsDefined[T](pf: PartialFunction[T,T], applicable: Boolean) extends Test[T] {
    def isDefinedAt(v: T) = (pf.isDefinedAt(v) == applicable)
  }

  private class ReleasableGet[T](value: T) extends Test[T] {
    var active = true
    def isDefinedAt(v: T) = !active || (value == v)
  }

  private class Set[T](value: T) extends Update[T] { def apply(v: T) = value }

  private class Incr[T](var delta: T)(implicit num: Numeric[T]) extends Update[T] {
    def apply(v: T) = num.plus(v, delta)
  }

  private class Transform[T](f: T => T) extends Update[T] { def apply(v: T) = f(v) }


  class TxnView[T](txn: Txn, val unbind: LazyConflictRef[T]) extends Ref.View[T] with Txn.ReadResource {
    private val _uview = unbind.underlying.bind(txn)
    private var _read: UnrecordedRead[T] = _uview.unrecordedRead
    private var _value: T = _read.value
    private val _head = new Head[T]
    private var _tail: HistoryNode[T] = _head

    private var _updaterRegistered = false

    def mode: AccessMode = txn

    //////////////// Source

    def get: T = {
      record(new Op_==(_value))
      _value
    }

    def getWith[Z](f: T => Z): Z = {
      val z = f(_value)
      record(new GetWith(f, z))
      z
    }

    def await(pred: T => Boolean) {
      if (!getWith(pred)) {
        // record underlying to read set, then retry
        _uview.get
        txn.retry()
      }
    }

    def releasableRead: ReleasableRead[T] = {
      val h = new ReleasableGet(_value)
      record(h)
      new ReleasableRead[T] {
        def context: Option[Txn] = Some(txn)
        val value: T = _value
        def release() { h.active = false }
      }
    }

    def unrecordedRead: UnrecordedRead[T] = new UnrecordedRead[T] {
      def context: Option[Txn] = Some(txn)
      val value = _value
      def stillValid = _read.stillValid
      def recorded = _read.recorded
    }

    //////////////// Sink

    def set(v: T) {
      _value = v
      record(new Set(v))
    }

    def trySet(v: T): Boolean = {
      set(v)
      true
    }

    //////////////// read+write

    def readForWrite: T = get

    def swap(v: T): T = {
      val z = get
      set(v)
      z
    }

    def compareAndSet(before: T, after: T): Boolean = {
      if (this getWith { _ == before }) {
        set(after)
        true
      } else {
        false
      }
    }

    def compareAndSetIdentity[A <: T with AnyRef](before: A, after: T): Boolean = {
      throw new UnsupportedOperationException("identity comparisons are not valid on T")
    }

    def transform(f: (T) => T) {
      _value = f(_value)
      record(new Transform(f))
    }

    def getAndTransform(f: (T) => T): T = {
      val z = get
      set(f(z))
      z
    }

    def tryTransform(f: (T) => T): Boolean = {
      transform(f)
      true
    }

    def transformIfDefined(pf: PartialFunction[T,T]): Boolean = {
      val a = pf.isDefinedAt(_value)
      record(new CheckIsDefined(pf, a))
      if (a) {
        transform(pf)
      }
      a
    }

    override def += (rhs: T)(implicit num: Numeric[T]) {
      if (num.zero != rhs) {
        _value = num.plus(_value, rhs)
        _tail match {
          case incr: Incr[T] => incr.delta = num.plus(incr.delta, rhs)
          case _ => record(new Incr(rhs))
        }
      }
    }

    override def -= (rhs: T)(implicit num: Numeric[T]) {
      this += num.negate(rhs)
    }

    //////////////// history replay implementation

    private def validateIfRequired() {
      if (!_read.stillValid) {
        // Since we are reusing the old unrecordedRead, we need to manually
        // trigger a revalidation.  This provides opacity.
        txn.explicitlyValidateReads()
      }
    }

    private def append(h: HistoryNode[T]) {
      _tail.next = h
      _tail = h
    }

    private def record(h: Test[T]) {
      if (h != _tail) append(h)
      validateIfRequired()
    }

    private def record(h: Update[T]) {
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
          _uview := _value
        }
        // clearing the flag here means that if a later beforeCommit callback
        // does another store on this ref, we will register a new handler and
        // do the underlying update again
        _updaterRegistered = false
      }, Int.MaxValue  / 2)      
    }

    def valid(txn: Txn): Boolean = {
      if (!_read.stillValid) {
        _read = _uview.unrecordedRead
        _value = _read.value
        var node: HistoryNode[T] = _head.next
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

private[ccstm] class LazyConflictRef[T](initialValue: T)(implicit cm: ClassManifest[T]) extends Ref[T] {

  private[LazyConflictRef] val underlying = Ref(initialValue)

  private val bound = new TxnLocal[LazyConflictRef.TxnView[T]] {
    override def initialValue(txn: Txn) = {
      val b = new LazyConflictRef.TxnView(txn, LazyConflictRef.this)
      txn.addReadResource(b, Int.MinValue / 2)
      b
    }
  }

  def get(implicit txn: Txn): T = bind.get
  def getWith[Z](f: (T) => Z)(implicit txn: Txn): Z = bind.getWith(f)
  def set(v: T)(implicit txn: Txn) { bind.set(v) }
  def swap(v: T)(implicit txn: Txn): T = bind.swap(v)
  def transform(f: (T) => T)(implicit txn: Txn) { bind.transform(f) }
  def transformIfDefined(pf: PartialFunction[T,T])(implicit txn: Txn): Boolean = bind.transformIfDefined(pf)

  def bind(implicit txn: Txn): Ref.View[T] = bound.get

  def single: Ref.View[T] = new SingleProxyView(this)

  def escaped: Ref.View[T] = new EscapedView(this, underlying.asInstanceOf[RefOps[T]].handle)

  private[ccstm] def embalm(identity: Int) { underlying.embalm(identity) }

  private[ccstm] def resurrect(identity: Int) { underlying.resurrect(identity) }

  // Object equals and hashCode are correct, since there can be no handle aliasing
}

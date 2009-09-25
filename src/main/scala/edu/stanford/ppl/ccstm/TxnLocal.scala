/* CCSTM - (c) 2009 Stanford University - PPL */

// TxnLocal

package edu.stanford.ppl.ccstm

import java.util.concurrent.atomic.AtomicLong


object TxnLocal {
  trait Bound[T] {
    def txn: Txn
    def get: T
    def set(v: T)
  }

  private[ccstm] class PerTxn {
    private var _size: Int = 0
    private var _data: Array[AnyRef] = new Array[AnyRef](16)

    def apply[T](owner: TxnLocal[T]): T = getOrSet(owner, true, null.asInstanceOf[T])
    def update[T](owner: TxnLocal[T], v: T) { getOrSet(owner, false, v) }

    private def getOrSet[T](owner: AnyRef, getting: Boolean, v: T): T = {
      val n = _data.length / 2
      var i = System.identityHashCode(owner) & (n - 1)
      while (true) {
        var k = _data(2 * i)
        if (k eq owner) {
          // hit
          if (getting) {
            return _data(2 * i + 1).asInstanceOf[T]
          } else {
            _data(2 * i + 1) = v.asInstanceOf[AnyRef]
            return v
          }
        }
        else if (k eq null) {
          // empty slot
          val z = (if (getting) owner.asInstanceOf[TxnLocal[T]].initialValue else v)
          _data(2 * i + 1) = z.asInstanceOf[AnyRef]
          _size += 1
          if (_size * 2 > _data.length) grow()
          return z
        }
        else {
          // probe
          i += 1
        }
      }
      throw new Error("unreachable")
    }

    private def grow() {
      val n = _data.length / 2
      val oldData = _data
      _size = 0
      _data = new Array[AnyRef](n * 4)
      var i = 0
      while (i < n) {
        val k = oldData(2 * i)
        if (k ne null) getOrSet(k, false, oldData(2 * i + 1))
        i += 1
      }
    }
  }
}

class TxnLocal[T] {
  import TxnLocal._

  protected def initialValue: T = null.asInstanceOf[T]
  def get(implicit txn: Txn) = getImpl(txn)
  def set(v: T)(implicit txn: Txn) = setImpl(txn, v)
  def bind(implicit txn0: Txn) = new Bound[T] {
    def txn = txn0
    def get: T = getImpl(txn0)
    def set(v: T) = setImpl(txn0, v)
  }

  private def getImpl(txn: Txn): T = {
    if (txn.locals == null) txn.locals = new PerTxn
    txn.locals(this)
  }

  private def setImpl(txn: Txn, v: T) {
    if (txn.locals == null) txn.locals = new PerTxn
    txn.locals(this) = v
  }
}
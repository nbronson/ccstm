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

  private[ccstm] class Entry[T](val key: Long, var value: T)

  private[ccstm] class PerTxn {
    var count: Int = 0
    var entries: Array[Entry[_]] = new Array[Entry[_]](8)

    def apply[T](owner: TxnLocal[T]): T = getOrSet(owner, true, null.asInstanceOf[T])
    def update[T](owner: TxnLocal[T], v: T) { getOrSet(owner, false, v) }

    private def getOrSet[T](owner: TxnLocal[T], getting: Boolean, v: T): T = {
      val key = owner.key
      var index = key.asInstanceOf[Int]
      while (true) {
        index &= (entries.length - 1)
        var e = entries(index)
        if (e != null) {
          if (e.key == key) {
            val entry = e.asInstanceOf[Entry[T]]
            if (!getting) entry.value = v
            return entry.value
          }
          // else probe
          index += 1
        } else {
          // fill in the empty slot
          val z = if (getting) owner.initialValue else v
          entries(index) = new Entry(key, z)
          count += 1
          if (count * 2 > entries.length) rehash()
          return z
        }
      }
      throw new Error("unreachable")
    }

    private def rehash() {
      val n = entries.length * 2
      val a = new Array[Entry[_]](n)
      for (e <- entries) {
        if (e != null) {
          var index = e.key.asInstanceOf[Int] & (n - 1)
          while (a(index) != null) {
            index = (index + 1) & (n - 1)
          }
          a(index) = e
        }
      }
      entries = a
    }
  }

  private[TxnLocal] val nextKey = new AtomicLong(1)
}

class TxnLocal[T] {
  import TxnLocal._

  private[TxnLocal] val key = nextKey.getAndIncrement

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
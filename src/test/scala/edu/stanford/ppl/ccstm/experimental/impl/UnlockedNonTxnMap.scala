/* SnapTree - (c) 2009 Stanford University - PPL */

// UnlockedNonTxnMap

package edu.stanford.ppl.ccstm.experimental.impl

import edu.stanford.ppl.ccstm.Txn
import edu.stanford.ppl.ccstm.experimental.TMap
import edu.stanford.ppl.ccstm.experimental.TMap.Bound


class UnlockedNonTxnMap[A,B](underlying: java.util.Map[A,AnyRef]) extends TMap[A,B] {

  def nonTxn = new TMap.Bound[A,B] {
    def unbind: TMap[A,B] = throw new UnsupportedOperationException
    def context: Option[Txn] = None

    override def isEmpty: Boolean = {
      underlying.isEmpty
    }

    override def size: Int = {
      underlying.size()
    }

    override def apply(key: A): B = {
      val v = underlying.get(key)
      if (null eq v) default(key) else NullValue.decode[B](v)
    }

    def get(key: A): Option[B] = {
      NullValue.decodeOption[B](underlying.get(key))
    }

    override def put(key: A, value: B): Option[B] = {
      NullValue.decodeOption[B](underlying.put(key, NullValue.encode(value)))
    }

    override def update(key: A, value: B) {
      underlying.put(key, NullValue.encode(value))
    }

    override def removeKey(key: A): Option[B] = {
      NullValue.decodeOption[B](underlying.remove(key))
    }

    def -= (key: A) = {
      underlying.remove(key)
      this
    }

    def transform(key: A, f: (Option[B]) => Option[B]) {
      {
        val before = get(key)
        f(before) match {
          case Some(v) => underlying.put(key, NullValue.encode(v))
          case None => if (!before.isEmpty) underlying.remove(key)
        }
      }
    }

    def transformIfDefined(key: A, pf: PartialFunction[Option[B], Option[B]]): Boolean = {
      val before = get(key)
      if (pf.isDefinedAt(before)) {
        pf(before) match {
          case Some(v) => underlying.put(key, NullValue.encode(v))
          case None => if (!before.isEmpty) underlying.remove(key)
        }
        true
      } else {
        false
      }
    }

    def iterator: Iterator[(A,B)] = {
      NullValue.decodeEntrySetSnapshot(underlying)
    }
  }

  def bind(implicit txn: Txn): Bound[A, B] = throw new UnsupportedOperationException

  def isEmpty(implicit txn: Txn): Boolean = throw new UnsupportedOperationException

  def size(implicit txn: Txn): Int = throw new UnsupportedOperationException

  def get(key: A)(implicit txn: Txn): Option[B] = throw new UnsupportedOperationException

  def put(key: A, value: B)(implicit txn: Txn): Option[B] = throw new UnsupportedOperationException

  def removeKey(key: A)(implicit txn: Txn): Option[B] = throw new UnsupportedOperationException

  protected def transformIfDefined(key: A,
                                   pfOrNull: PartialFunction[Option[B],Option[B]],
                                   f: Option[B] => Option[B])(implicit txn: Txn): Boolean = {
    throw new UnsupportedOperationException
  }
}
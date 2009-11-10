/* SnapTree - (c) 2009 Stanford University - PPL */

// TMap

package edu.stanford.ppl.ccstm.experimental

import edu.stanford.ppl.ccstm._


object TMap {
  trait Source[A,+B] {
    def nonTxn: TMap.BoundSource[A,B]

    def bind(implicit txn: Txn): TMap.BoundSource[A,B]

    def isEmpty(implicit txn: Txn): Boolean
    def size(implicit txn: Txn): Int

    def apply(key: A)(implicit txn: Txn): B
    def get(key: A)(implicit txn: Txn): Option[B]

    def default(key: A): B = {
      throw new NoSuchElementException(key + ": is not present")
    }

    def mkString(implicit txn: Txn): String = bind(txn).mkString
    def mkString(sep: String)(implicit txn: Txn): String = bind(txn).mkString(sep)
    def mkString(start: String, sep: String, end: String)(implicit txn: Txn): String = bind(txn).mkString(start, sep, end)
  }

  trait Sink[A,-B] {
    def nonTxn: TMap.BoundSink[A,B]

    def bind(implicit txn: Txn): TMap.BoundSink[A,B]

    def update(key: A, value: B)(implicit txn: Txn)
    def -=(key: A)(implicit txn: Txn)
  }

  trait BoundSource[A,+B] extends scala.collection.Map[A,B] {
    def unbind: TMap.Source[A,B]
    def context: Option[Txn]

    override def default(key: A): B = unbind.default(key)
  }

  trait BoundSink[A,-B] {
    def unbind: TMap.Sink[A,B]
    def context: Option[Txn]

    def update(key: A, value: B)
    def -= (key: A)
  }

  trait Bound[A,B] extends BoundSource[A,B] with BoundSink[A,B] with scala.collection.mutable.Map[A,B] {
    def unbind: TMap[A,B]

    def transform(key: A, f: Option[B] => Option[B])
    def transformIfDefined(key: A, pf: PartialFunction[Option[B],Option[B]]): Boolean
  }

  private[experimental] abstract class AbstractTxnBound[A,B,M <: TMap[A,B]](val txn: Txn, val unbind: M) extends Bound[A,B] {
    def context = Some(txn)

    override def isEmpty: Boolean = unbind.isEmpty(txn)
    def size: Int = unbind.size(txn)

    override def apply(key: A): B = unbind.apply(key)(txn)
    def get(key: A): Option[B] = unbind.get(key)(txn)

    override def put(key: A, value: B): Option[B] = unbind.put(key, value)(txn)
    def update(key: A, value: B) { unbind.update(key, value)(txn) }

    override def removeKey(key: A): Option[B] = unbind.removeKey(key)(txn)
    def -=(key: A) { unbind.-=(key)(txn) }
  }
}

/** An interface for transactional maps. */
trait TMap[A,B] extends TMap.Source[A,B] with TMap.Sink[A,B] {

  def nonTxn: TMap.Bound[A,B]

  def bind(implicit txn: Txn): TMap.Bound[A,B]

  def put(key: A, value: B)(implicit txn: Txn): Option[B]
  def removeKey(key: A)(implicit txn: Txn): Option[B]
}
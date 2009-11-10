/* SnapTree - (c) 2009 Stanford University - PPL */

// TMap

package edu.stanford.ppl.ccstm.experimental

import edu.stanford.ppl.ccstm._


object TMap {
  trait Bound[A,B] extends scala.collection.mutable.Map[A,B] {
    def unbind: TMap[A,B]
    def context: Option[Txn]

    def transform(key: A, f: Option[B] => Option[B])
    def transformIfDefined(key: A, pf: PartialFunction[Option[B],Option[B]]): Boolean

    override def default(key: A): B = unbind.default(key)
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
trait TMap[A,B] {

  def nonTxn: TMap.Bound[A,B]

  def bind(implicit txn: Txn): TMap.Bound[A,B]

  def isEmpty(implicit txn: Txn): Boolean
  def size(implicit txn: Txn): Int

  def apply(key: A)(implicit txn: Txn): B
  def get(key: A)(implicit txn: Txn): Option[B]

  def put(key: A, value: B)(implicit txn: Txn): Option[B]
  def update(key: A, value: B)(implicit txn: Txn)

  def removeKey(key: A)(implicit txn: Txn): Option[B]
  def -=(key: A)(implicit txn: Txn)

  def default(key: A): B = {
    throw new NoSuchElementException(key + ": is not present")
  }

  def mkString(implicit txn: Txn): String = bind(txn).mkString
  def mkString(sep: String)(implicit txn: Txn): String = bind(txn).mkString(sep)
  def mkString(start: String, sep: String, end: String)(implicit txn: Txn): String = bind(txn).mkString(start, sep, end)
}
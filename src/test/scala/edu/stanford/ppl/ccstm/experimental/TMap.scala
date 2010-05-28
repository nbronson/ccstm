/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TMap

package edu.stanford.ppl.ccstm.experimental

import edu.stanford.ppl.ccstm._


object TMap {
  trait Source[A,+B] {
    def escaped: TMap.BoundSource[A,B]

    def bind(implicit txn: Txn): TMap.BoundSource[A,B]

    def isEmpty(implicit txn: Txn): Boolean
    def size(implicit txn: Txn): Int

    def apply(key: A)(implicit txn: Txn): B = {
      get(key) match {
        case Some(v) => v
        case None => default(key)
      }
    }

    def get(key: A)(implicit txn: Txn): Option[B]

    def default(key: A): B = {
      throw new NoSuchElementException(key + ": is not present")
    }

    def mkString(implicit txn: Txn): String = bind(txn).mkString
    def mkString(sep: String)(implicit txn: Txn): String = bind(txn).mkString(sep)
    def mkString(start: String, sep: String, end: String)(implicit txn: Txn): String = bind(txn).mkString(start, sep, end)
  }

  trait Sink[A,-B] {
    def escaped: TMap.BoundSink[A,B]

    def bind(implicit txn: Txn): TMap.BoundSink[A,B]

    def update(key: A, value: B)(implicit txn: Txn)
    def -=(key: A)(implicit txn: Txn)
  }

  trait BoundSource[A,+B] extends scala.collection.Map[A,B] {
    def unbind: TMap.Source[A,B]
    def context: Option[Txn]

    // TODO: these should be in a TSortedMap
    def subRange(begin: A, end: A): Iterator[(A,B)] = throw new UnsupportedOperationException
    def higher(key: A): Option[(A,B)] = throw new UnsupportedOperationException

    override def default(key: A): B = unbind.default(key)
  }

  trait BoundSink[A,-B] {
    def unbind: TMap.Sink[A,B]
    def context: Option[Txn]

    def update(key: A, value: B)
    def -= (key: A): this.type
  }

  trait Bound[A,B] extends BoundSource[A,B] with BoundSink[A,B] with scala.collection.mutable.Map[A,B] {
    def unbind: TMap[A,B]

    def += (kv: (A, B)) = { update(kv._1, kv._2); this }

//    def transform(key: A, f: Option[B] => Option[B])
//    def transformIfDefined(key: A, pf: PartialFunction[Option[B],Option[B]]): Boolean
  }

  private[experimental] abstract class AbstractNonTxnBound[A,B,M <: TMap[A,B]](val unbind: M) extends Bound[A,B] {
    def context = None

    override def isEmpty: Boolean = !iterator.hasNext

    // we implement in terms of put, instead of in terms of +=
    override def update(key: A, value: B) { put(key, value) }
    def -= (key: A) = { remove(key); this }

//    def transform(key: A, f: Option[B] => Option[B]) { transformIfDefined(key, null, f) }
//    def transformIfDefined(key: A, pf: PartialFunction[Option[B],Option[B]]): Boolean = transformIfDefined(key, pf, pf)
//
//    protected def transformIfDefined(key: A, pfOrNull: PartialFunction[Option[B],Option[B]], f: Option[B] => Option[B]): Boolean
  }

  private[experimental] abstract class AbstractTxnBound[A,B,M <: TMap[A,B]](val txn: Txn, val unbind: M) extends Bound[A,B] {
    def context = Some(txn)

    override def isEmpty: Boolean = unbind.isEmpty(txn)
    override def size: Int = unbind.size(txn)

    override def apply(key: A): B = unbind.apply(key)(txn)
    def get(key: A): Option[B] = unbind.get(key)(txn)

    override def put(key: A, value: B): Option[B] = unbind.put(key, value)(txn)
    override def update(key: A, value: B) { unbind.update(key, value)(txn) }

    override def removeKey(key: A): Option[B] = unbind.removeKey(key)(txn)
    def -=(key: A) = { unbind.-=(key)(txn); this }

//    def transform(key: A, f: (Option[B]) => Option[B]) {
//      unbind.transform(key, f)(txn)
//    }
//    def transformIfDefined(key: A, pf: PartialFunction[Option[B], Option[B]]): Boolean = {
//      unbind.transformIfDefined(key, pf)(txn)
//    }
  }
}

/** An interface for transactional maps. */
trait TMap[A,B] extends TMap.Source[A,B] with TMap.Sink[A,B] {

  def escaped: TMap.Bound[A,B]

  def bind(implicit txn: Txn): TMap.Bound[A,B]

  def put(key: A, value: B)(implicit txn: Txn): Option[B]
  def removeKey(key: A)(implicit txn: Txn): Option[B]

//  def transform(key: A, f: Option[B] => Option[B])(implicit txn: Txn) {
//    transformIfDefined(key, null, f)
//  }
//
//  def transformIfDefined(key: A, pf: PartialFunction[Option[B],Option[B]])(implicit txn: Txn): Boolean = {
//    transformIfDefined(key, pf, pf)
//  }
//
//  protected def transformIfDefined(key: A,
//                                   pfOrNull: PartialFunction[Option[B],Option[B]],
//                                   f: Option[B] => Option[B])(implicit txn: Txn): Boolean

  //////////////// default implementations

  def update(key: A, value: B)(implicit txn: Txn) { put(key, value) }
  def -= (key: A)(implicit txn: Txn) { removeKey(key) }
}

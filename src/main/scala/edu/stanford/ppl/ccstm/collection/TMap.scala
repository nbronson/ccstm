/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TMap

package edu.stanford.ppl.ccstm.collection


import edu.stanford.ppl.ccstm._

object TMap {
  trait Source[A,+B] {
    def single: TMap.SourceView[A,B]
    def escaped: TMap.SourceView[A,B]
    def bind(implicit txn: Txn): TMap.SourceView[A,B]

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
  }

  trait Sink[A,-B] {
    def single: TMap.SinkView[A,B]
    def escaped: TMap.SinkView[A,B]
    def bind(implicit txn: Txn): TMap.SinkView[A,B]

    def update(key: A, value: B)(implicit txn: Txn)
    def -=(key: A)(implicit txn: Txn)
  }

  trait SourceView[A,+B] extends scala.collection.Map[A,B] {
    def unbind: TMap.Source[A,B]
    def mode: BindingMode

    override def default(key: A): B = unbind.default(key)
  }

  trait SinkView[A,-B] {
    def unbind: TMap.Sink[A,B]
    def mode: BindingMode

    def update(key: A, value: B)
    def -= (key: A): this.type
  }

  trait View[A,B] extends SourceView[A,B] with SinkView[A,B] with scala.collection.mutable.Map[A,B] {
    def unbind: TMap[A,B]

    def += (kv: (A, B)) = { update(kv._1, kv._2); this }
  }

  private[collection] class SingleView[A,B,M <: TMap[A,B]](val unbind: M) extends View[A,B] {
    def mode = Single

    private def dynView = Txn.dynCurrentOrNull match {
      case null => unbind.escaped
      case txn => unbind.bind(txn)
    }

    override def isEmpty: Boolean = dynView.isEmpty
    override def size: Int = dynView.size

    override def apply(key: A): B = dynView.apply(key)
    def get(key: A): Option[B] = dynView.get(key)

    override def put(key: A, value: B): Option[B] = dynView.put(key, value)
    override def update(key: A, value: B) { dynView.update(key, value) }

    override def remove(key: A): Option[B] = dynView.remove(key)
    def -=(key: A) = { dynView.-=(key); this }

    def iterator = dynView.iterator
  }

  private[collection] abstract class AbstractEscapedView[A,B,M <: TMap[A,B]](val unbind: M) extends View[A,B] {
    def mode = Escaped

    override def isEmpty: Boolean = !iterator.hasNext

    // we implement in terms of put and remove, instead of in terms of += and -=
    override def update(key: A, value: B) { put(key, value) }
    def -= (key: A) = { remove(key); this }
  }

  private[collection] abstract class AbstractTxnView[A,B,M <: TMap[A,B]](val txn: Txn, val unbind: M) extends View[A,B] {
    def mode = txn

    override def isEmpty: Boolean = unbind.isEmpty(txn)
    override def size: Int = unbind.size(txn)

    override def apply(key: A): B = unbind.apply(key)(txn)
    def get(key: A): Option[B] = unbind.get(key)(txn)

    override def put(key: A, value: B): Option[B] = unbind.put(key, value)(txn)
    override def update(key: A, value: B) { unbind.update(key, value)(txn) }

    override def remove(key: A): Option[B] = unbind.remove(key)(txn)
    def -=(key: A) = { unbind.-=(key)(txn); this }
  }
}

/** An interface for transactional maps. */
trait TMap[A,B] extends TMap.Source[A,B] with TMap.Sink[A,B] {

  def escaped: TMap.View[A,B]
  def single: TMap.View[A,B] = new TMap.SingleView(this)
  def bind(implicit txn: Txn): TMap.View[A,B]

  def put(key: A, value: B)(implicit txn: Txn): Option[B]
  def remove(key: A)(implicit txn: Txn): Option[B]

  //////////////// default implementations

  def update(key: A, value: B)(implicit txn: Txn) { put(key, value) }
  def -= (key: A)(implicit txn: Txn) { remove(key) }
}

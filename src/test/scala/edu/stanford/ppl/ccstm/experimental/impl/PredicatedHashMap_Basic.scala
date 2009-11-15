/* CCSTM - (c) 2009 Stanford University - PPL */

// PredicatedHashMap_Basic

package edu.stanford.ppl.ccstm.experimental.impl

import edu.stanford.ppl.ccstm.experimental.TMap
import edu.stanford.ppl.ccstm.collection.{TOptionRef, LazyConflictIntRef}
import java.util.concurrent.ConcurrentHashMap
import edu.stanford.ppl.ccstm.experimental.TMap.Bound
import edu.stanford.ppl.ccstm.{STM, Txn}


class PredicatedHashMap_Basic[A,B] extends TMap[A,B] {
  private val predicates = new ConcurrentHashMap[A,TOptionRef[B]]

  def nonTxn: Bound[A,B] = new TMap.AbstractNonTxnBound[A,B,PredicatedHashMap_Basic[A,B]](this) {

    def get(key: A): Option[B] = {
      // if no predicate exists, then we don't need to create one
      val p = existingPred(key)
      if (null == p) None else p.nonTxn.get
    }

    override def put(key: A, value: B): Option[B] = {
      pred(key).nonTxn.getAndSet(Some(value))
    }

    override def removeKey(key: A): Option[B] = {
      // if no predicate exists, then we don't need to create one
      val p = existingPred(key)
      if (null == p) None else p.nonTxn.getAndSet(None)
    }

    override def transform(key: A, f: (Option[B]) => Option[B]) {
      pred(key).nonTxn.transform(f)
    }

    override def transformIfDefined(key: A, pf: PartialFunction[Option[B],Option[B]]): Boolean = {
      pred(key).nonTxn.transformIfDefined(pf)
    }

    protected def transformIfDefined(key: A,
                                     pfOrNull: PartialFunction[Option[B],Option[B]],
                                     f: Option[B] => Option[B]): Boolean = {
      throw new Error
    }

    def elements: Iterator[(A,B)] = new Iterator[(A,B)] {
      val iter = predicates.keySet().iterator
      var avail: (A,B) = null
      advance()

      private def advance() {
        while (iter.hasNext) {
          val k = iter.next()
          get(k) match {
            case Some(v) => {
              avail = (k,v)
              return
            }
            case None => // keep looking
          }
        }
        avail = null
      }

      def hasNext: Boolean = null != avail
      def next(): (A,B) = {
        val z = avail
        advance()
        z
      }
    }
  }

  def bind(implicit txn0: Txn): Bound[A, B] = new TMap.AbstractTxnBound[A,B,PredicatedHashMap_Basic[A,B]](txn0, this) {
    def elements: Iterator[(A,B)] = throw new UnsupportedOperationException
  }

  def isEmpty(implicit txn: Txn): Boolean = throw new UnsupportedOperationException

  def size(implicit txn: Txn): Int = throw new UnsupportedOperationException

  def get(key: A)(implicit txn: Txn): Option[B] = {
    pred(key).get
  }

  def put(key: A, value: B)(implicit txn: Txn): Option[B] = {
    pred(key).getAndSet(Some(value))
  }

  def removeKey(key: A)(implicit txn: Txn): Option[B] = {
    pred(key).getAndSet(None)
  }


  override def transform(key: A, f: (Option[B]) => Option[B])(implicit txn: Txn) {
    pred(key).transform(f)
  }

  override def transformIfDefined(key: A, pf: PartialFunction[Option[B],Option[B]])(implicit txn: Txn): Boolean = {
    pred(key).transformIfDefined(pf)
  }

  protected def transformIfDefined(key: A,
                                   pfOrNull: PartialFunction[Option[B],Option[B]],
                                   f: Option[B] => Option[B])(implicit txn: Txn): Boolean = {
    throw new Error
  }

  private def existingPred(key: A): TOptionRef[B] = predicates.get(key)

  private def pred(key: A): TOptionRef[B] = {
    val pred = predicates.get(key)
    if (null != pred) pred else createPred(key)
  }

  private def createPred(key: A): TOptionRef[B] = {
    val fresh = new TOptionRef[B](None)
    val race = predicates.putIfAbsent(key, fresh)
    if (null != race) race else fresh
  }
}
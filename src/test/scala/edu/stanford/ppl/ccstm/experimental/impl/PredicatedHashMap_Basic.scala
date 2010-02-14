/* CCSTM - (c) 2009 Stanford University - PPL */

// PredicatedHashMap_Basic

package edu.stanford.ppl.ccstm.experimental.impl

import edu.stanford.ppl.ccstm.experimental.TMap
import java.util.concurrent.ConcurrentHashMap
import edu.stanford.ppl.ccstm.experimental.TMap.Bound
import edu.stanford.ppl.ccstm.{STM, Txn}
import edu.stanford.ppl.ccstm.collection.TAnyRef


class PredicatedHashMap_Basic[A,B] extends TMap[A,B] {
  private val predicates = new ConcurrentHashMap[A,TAnyRef[AnyRef]]

  def nonTxn: Bound[A,B] = new TMap.AbstractNonTxnBound[A,B,PredicatedHashMap_Basic[A,B]](this) {

    def get(key: A): Option[B] = {
      // if no predicate exists, then we don't need to create one
      val p = predicates.get(key)
      if (null == p) None else NullValue.decodeOption(p.nonTxn.get)
    }

    override def put(key: A, value: B): Option[B] = {
      val p = predicates.get(key)
      if (null == p) {
        putNew(key, value)
      } else {
        NullValue.decodeOption(p.nonTxn.getAndSet(NullValue.encode(value)))
      }
    }

    private def putNew(key: A, value: B): Option[B] = {
      // if no predicate exists, then we can put the value in the
      // predicate during construction and linearize at the putIfAbsent
      val fresh = new TAnyRef[AnyRef](NullValue.encode(value))
      val race = predicates.putIfAbsent(key, fresh)
      if (null == race) {
        // our predicate won
        None
      } else {
        // normal read from the existing predicate
        NullValue.decodeOption(race.nonTxn.getAndSet(NullValue.encode(value)))
      }
    }

    override def removeKey(key: A): Option[B] = {
      // if no predicate exists, then we don't need to create one
      val p = predicates.get(key)
      if (null == p) None else NullValue.decodeOption(p.nonTxn.getAndSet(null))
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
    NullValue.decodeOption(pred(key).get)
  }

  def put(key: A, value: B)(implicit txn: Txn): Option[B] = {
    NullValue.decodeOption(pred(key).getAndSet(NullValue.encode(value)))
  }

  def removeKey(key: A)(implicit txn: Txn): Option[B] = {
    NullValue.decodeOption(pred(key).getAndSet(null))
  }


//  override def transform(key: A, f: (Option[B]) => Option[B])(implicit txn: Txn) {
//    pred(key).transform(f)
//  }
//
//  override def transformIfDefined(key: A, pf: PartialFunction[Option[B],Option[B]])(implicit txn: Txn): Boolean = {
//    pred(key).transformIfDefined(pf)
//  }
//
//  protected def transformIfDefined(key: A,
//                                   pfOrNull: PartialFunction[Option[B],Option[B]],
//                                   f: Option[B] => Option[B])(implicit txn: Txn): Boolean = {
//    throw new Error
//  }

  private def pred(key: A): TAnyRef[AnyRef] = {
    val pred = predicates.get(key)
    if (null != pred) pred else createPred(key)
  }

  private def createPred(key: A): TAnyRef[AnyRef] = {
    val fresh = new TAnyRef[AnyRef](null)
    val race = predicates.putIfAbsent(key, fresh)
    if (null != race) race else fresh
  }
}
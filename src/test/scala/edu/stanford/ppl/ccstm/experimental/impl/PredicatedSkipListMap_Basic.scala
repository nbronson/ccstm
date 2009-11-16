/* CCSTM - (c) 2009 Stanford University - PPL */

// PredicatedSkipListMap_Basic

package edu.stanford.ppl.ccstm.experimental.impl

import edu.stanford.ppl.ccstm.experimental.TMap
import edu.stanford.ppl.ccstm.experimental.TMap.Bound
import edu.stanford.ppl.ccstm.{STM, Txn}
import java.util.concurrent.ConcurrentSkipListMap
import edu.stanford.ppl.ccstm.collection.{TIntRef, TOptionRef}


object PredicatedSkipListMap_Basic {
  class Predicate[B] {
    @volatile var ready = false
    val vOpt = new TOptionRef[B](None)
    val predInsCount = new TIntRef(0)
    val succInsCount = new TIntRef(0)
  }

  val entryIterNextValueField = {
    val f = Class.forName("java.util.concurrent.ConcurrentSkipListMap$Iter").getDeclaredField("nextValue")
    f.setAccessible(true)
    f
  }

  def entryIterPeek[A,B](iter: java.util.Iterator[java.util.Map.Entry[A,B]]): B = {
    // look for a field named "nextValue"
    entryIterNextValueField.get(iter).asInstanceOf[B]
  }
}

class PredicatedSkipListMap_Basic[A,B] extends TMap[A,B] {
  import PredicatedSkipListMap_Basic._

  private val predicates = new ConcurrentSkipListMap[A,Predicate[B]]
  private val firstInsCount = new TIntRef(0)
  private val lastInsCount = new TIntRef(0)

  def nonTxn: Bound[A,B] = new TMap.AbstractNonTxnBound[A,B,PredicatedSkipListMap_Basic[A,B]](this) {

    def get(key: A): Option[B] = {
      // if no predicate exists, then we don't need to create one
      val p = existingPred(key)
      if (null == p) None else p.vOpt.nonTxn.get
    }

    override def put(key: A, value: B): Option[B] = {
      predicateForPut(key).vOpt.nonTxn.getAndSet(Some(value))
    }

    override def removeKey(key: A): Option[B] = {
      // if no predicate exists, then we don't need to create one
      val p = existingPred(key)
      if (null == p) None else p.vOpt.nonTxn.getAndSet(None)
    }

    override def transform(key: A, f: (Option[B]) => Option[B]) {
      predicateForPut(key).vOpt.nonTxn.transform(f)
    }

    override def transformIfDefined(key: A, pf: PartialFunction[Option[B],Option[B]]): Boolean = {
      predicateForPut(key).vOpt.nonTxn.transformIfDefined(pf)
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

  def bind(implicit txn0: Txn): Bound[A, B] = new TMap.AbstractTxnBound[A,B,PredicatedSkipListMap_Basic[A,B]](txn0, this) {
    def elements: Iterator[(A,B)] = {
      // this will cause the txn to be invalidated if we missed a key
      firstInsCount.get

      // Iteration is tricky, because the underlying iterator will already
      // have advanced to the _next_ element when returning the existing one,
      // but we can't read the current element's succInsCount until after it
      // has been returned to us.  We have three options:
      //  1) don't use an iterator, but just repeatedly apply higherEntry
      //  2) use two iterators, with one running just ahead of the second
      //  3) hack in a peek() operator using reflection
      // Option (1) will have bad performance.  Option (2) seems very
      // complicated, because the iterators may not see the same set of
      // entries.  Therefore, we go with option (3).

      val iter = predicates.entrySet().iterator()

      return new Iterator[(A,B)] {
        var avail: (A,B) = null
        advance()

        private def advance() {
          while (iter.hasNext) {
            // Before iter.next() returns X, it will first find X's successor.
            // We need to perform the read of X.succInsCount before it does
            // that.
            val succPred = entryIterPeek(iter)
            succPred.succInsCount.get
            val succEntry = iter.next
            assert (succPred eq succEntry.getValue)
            succPred.vOpt.get match {
              case Some(v) => {
                avail = (succEntry.getKey, v)
                return
              }
              case None => {}
            }
          }
          avail = null
        }

        def hasNext = null != avail
        def next = {
          val z = avail
          advance()
          z
        }
      }
    }
  }

  def isEmpty(implicit txn: Txn): Boolean = bind.elements.hasNext

  def size(implicit txn: Txn): Int = {
    var n = 0    
    var iter = bind.elements
    while (iter.hasNext) { n += 1; iter.next }
    n
  }

  def get(key: A)(implicit txn: Txn): Option[B] = {
    predicateForRead(key).vOpt.get
  }

  def put(key: A, value: B)(implicit txn: Txn): Option[B] = {
    predicateForPut(key).vOpt.getAndSet(Some(value))
  }

  def removeKey(key: A)(implicit txn: Txn): Option[B] = {
    predicateForRead(key).vOpt.getAndSet(None)
  }


  override def transform(key: A, f: (Option[B]) => Option[B])(implicit txn: Txn) {
    predicateForPut(key).vOpt.transform(f)
  }

  override def transformIfDefined(key: A, pf: PartialFunction[Option[B],Option[B]])(implicit txn: Txn): Boolean = {
    predicateForPut(key).vOpt.transformIfDefined(pf)
  }

  protected def transformIfDefined(key: A,
                                   pfOrNull: PartialFunction[Option[B],Option[B]],
                                   f: Option[B] => Option[B])(implicit txn: Txn): Boolean = {
    throw new Error
  }

  private def existingPred(key: A): Predicate[B] = predicates.get(key)

  private def predicateForRead(key: A): Predicate[B] = {
    val p = predicates.get(key)
    if (null != p) {
      p
    } else {
      val fresh = new Predicate[B]
      val race = predicates.putIfAbsent(key, fresh)
      if (null != race) race else fresh
    }
  }

  private def predicateForPut(key: A): Predicate[B] = {
    // vOptRef can't be non-None until after the pred and succ mod counts are
    // incremented.  This is true whether we created it or found it.
    val p = predicateForRead(key)
    if (!p.ready) {
      val before = predicates.lowerEntry(key)
      (if (null != before) before.getValue.succInsCount else firstInsCount).nonTxn += 1
      val after = predicates.higherEntry(key)
      (if (null != after) after.getValue.predInsCount else lastInsCount).nonTxn += 1
      p.ready = true
    }
    p
  }
}
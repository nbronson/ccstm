/* CCSTM - (c) 2009 Stanford University - PPL */

// PredicatedSkipListMap_Basic

package edu.stanford.ppl.ccstm.experimental.impl

import edu.stanford.ppl.ccstm.experimental.TMap
import edu.stanford.ppl.ccstm.experimental.TMap.Bound
import edu.stanford.ppl.ccstm.{STM, Txn}
import java.util.concurrent.ConcurrentSkipListMap
import edu.stanford.ppl.ccstm.collection.{TIntRef, TOptionRef}

// TODO: use Ordered or Ordering

object PredicatedSkipListMap_Basic {
  class Predicate[B] extends TOptionRef[B](None) {
    @volatile var ready = false
    val predInsCount = new TIntRef(0)
    val succInsCount = new TIntRef(0)
  }

  val iterNextValueField = {
    val f = Class.forName("java.util.concurrent.ConcurrentSkipListMap$Iter").getDeclaredField("nextValue")
    f.setAccessible(true)
    f
  }

  val subMapIterNextValueField = {
    val f = Class.forName("java.util.concurrent.ConcurrentSkipListMap$SubMap$SubMapIter").getDeclaredField("nextValue")
    f.setAccessible(true)
    f
  }

  def iterPeek[A,B](iter: java.util.Iterator[java.util.Map.Entry[A,B]]): B = {
    // look for a field named "nextValue"
    iterNextValueField.get(iter).asInstanceOf[B]
  }

  def subMapIterPeek[A,B](iter: java.util.Iterator[java.util.Map.Entry[A,B]]): B = {
    // look for a field named "nextValue"
    subMapIterNextValueField.get(iter).asInstanceOf[B]
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
      if (null == p) None else p.nonTxn.get
    }

    override def put(key: A, value: B): Option[B] = {
      predicateForPut(key).nonTxn.getAndSet(Some(value))
    }

    override def removeKey(key: A): Option[B] = {
      // if no predicate exists, then we don't need to create one
      val p = existingPred(key)
      if (null == p) None else p.nonTxn.getAndSet(None)
    }

    def higher(key: A): Option[(A,B)] = {
      // we use a txn, because both the value read must be consistent with the
      // protecting insCount reads
      STM.atomic(unbind.higher(key)(_))
    }

    override def transform(key: A, f: (Option[B]) => Option[B]) {
      predicateForPut(key).nonTxn.transform(f)
    }

    override def transformIfDefined(key: A, pf: PartialFunction[Option[B],Option[B]]): Boolean = {
      predicateForPut(key).nonTxn.transformIfDefined(pf)
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
            val succPred = iterPeek(iter)
            succPred.succInsCount.get
            val succEntry = iter.next
            assert (succPred eq succEntry.getValue)
            succPred.get match {
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
    predicateForRead(key).get
  }

  def put(key: A, value: B)(implicit txn: Txn): Option[B] = {
    predicateForPut(key).getAndSet(Some(value))
  }

  def removeKey(key: A)(implicit txn: Txn): Option[B] = {
    predicateForRead(key).getAndSet(None)
  }

  def higher(key: A)(implicit txn: Txn): Option[(A,B)] = {
    // There's a bit of a catch-22, because we can't find the insCount that
    // protects our range access until we've performed the access.  Also, we
    // may have to skip one or more predicates until we find one that actually
    // contains a value.
    val tail = predicates.tailMap(key, false)

    val first = tail.firstEntry
    if (null == first) {
      // changes to the tail must adjust lastInsCount
      lastInsCount.get
    } else {
      // changes to the head of the tail must change the predInsCount of first
      first.getValue.predInsCount.get
    }

    // for the second (protected) read we will use an iterator, so that we can
    // keep going if we see absent entries
    val iter = tail.entrySet.iterator
    var availValue = subMapIterPeek(iter)
    if (availValue ne (if (null == first) null else first.getValue)) {
      // the insCount we read was not the right one, try again
      return higher(key)
    }

    // we can now iterate as in bind.elements
    while (null != availValue) {
      availValue.get match {
        case Some(v) => {
          // no protection for iter.next needed, because we just want the key
          return Some((iter.next.getKey, v))
        }
        case None => {} // keep searching
      }
      availValue.succInsCount.get
      iter.next
    }
    return None
  }

  override def transform(key: A, f: (Option[B]) => Option[B])(implicit txn: Txn) {
    predicateForPut(key).transform(f)
  }

  override def transformIfDefined(key: A, pf: PartialFunction[Option[B],Option[B]])(implicit txn: Txn): Boolean = {
    predicateForPut(key).transformIfDefined(pf)
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
    if (p.ready) return p

    val before = predicates.lowerEntry(key)
    if (p.ready) return p
    (if (null != before) before.getValue.succInsCount else firstInsCount).nonTxn += 1

    val after = predicates.higherEntry(key)
    if (p.ready) return p
    (if (null != after) after.getValue.predInsCount else lastInsCount).nonTxn += 1

    p.ready = true
    return p
  }
}
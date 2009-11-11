/* CCSTM - (c) 2009 Stanford University - PPL */

// STMIndexedMap

package edu.stanford.ppl.ccstm.experimental.bench

import edu.stanford.ppl.ccstm.experimental.TMap
import edu.stanford.ppl.ccstm.{Txn, STM}
import reflect.Manifest

object STMIndexedMap {

  trait TMapFactory {
    def newInstance[A,B](implicit am: Manifest[A], bm: Manifest[B]): TMap[A,B]
  }

  
  private val ImmSet = scala.collection.immutable.Set
  private type ImmSet[A] = scala.collection.immutable.Set[A]
  private type ROMap[A,B] = scala.collection.Map[A,B]
  private type MutMap[A,B] = scala.collection.mutable.Map[A,B]

  private class Index[K,V,C](lock: AnyRef, val func: V => Option[C], fact: TMapFactory)(implicit cm: Manifest[C], kvm: Manifest[ImmSet[(K,V)]]) extends ROMap[C,ImmSet[(K,V)]] {
    private val data = fact.newInstance(cm, kvm)

    //////////// index maintenance

    def indexAdd(key: K, after: V)(implicit txn: Txn) {
      func(after) match {
        case Some(c) => {
          val e = (key,after)
          data.put(c, (data.get(c) match {
            case Some(s) => s + e
            case None => ImmSet(e)
          }))
        }
        case None => {}
      }
    }

    def indexUpdate(key: K, before: V, after: V)(implicit txn: Txn) {
      indexRemove(key, before)
      indexAdd(key, after)
    }

    def indexRemove(key: K, before: V)(implicit txn: Txn) {
      func(before) match {
        case Some(c) => {
          val e = (key,before)
          val s = data(c) - e
          if (s.isEmpty) data.removeKey(c) else data.put(c, s)
        }
        case None => throw new IllegalStateException
      }
    }

    //////////// map implementation

    override def isEmpty: Boolean = data.nonTxn.isEmpty

    def size: Int = data.nonTxn.size

    def get(c: C): Option[ImmSet[(K,V)]] = data.nonTxn.get(c)

    def elements: Iterator[(C,ImmSet[(K,V)])] = STM.atomic(elementsImpl(_))

    private def elementsImpl(implicit txn: Txn): Iterator[(C,ImmSet[(K,V)])] = {
      data.bind.clone.elements
    }
  }
}

/** A simple implementation of <code>IndexedMap</code> that uses a single lock
 *  to protect access to normal hash tables.
 */
class STMIndexedMap[K,V](fact: STMIndexedMap.TMapFactory)(implicit km: Manifest[K], vm: Manifest[V]) extends IndexedMap[K,V] {
  import STMIndexedMap._

  private val main: TMap[K,V] = fact.newInstance(km, vm)
  private var indices: List[Index[K,V,_]] = Nil


  override def isEmpty: Boolean = main.nonTxn.isEmpty

  def size: Int = main.nonTxn.size

  def get(key: K): Option[V] = {
    main.nonTxn.get(key)
  }

  def update(key: K, value: V) { put(key, value) }

  override def put(key: K, value: V): Option[V] = STM.atomic(putImpl(key,value)(_))

  private def putImpl(key: K, value: V)(implicit txn: Txn): Option[V] = {
    val prev = main.put(key, value)
    prev match {
      case Some(p) => for (i <- indices) i.indexUpdate(key, p, value)
      case None => for (i <- indices) i.indexAdd(key, value)
    }
    prev
  }

  def -=(key: K) { removeKey(key) }

  override def removeKey(key: K): Option[V] = STM.atomic(removeKeyImpl(key)(_))

  private def removeKeyImpl(key: K)(implicit txn: Txn): Option[V] = {
    val prev = main.removeKey(key)
    prev match {
      case Some(p) => for (i <- indices) i.indexRemove(key, p)
      case None => {}
    }
    prev
  }

  def elements: Iterator[(K,V)] = STM.atomic(elementsImpl(_))

  private def elementsImpl(implicit txn: Txn): Iterator[(K,V)] = {
    main.bind.clone.elements
  }

  def addIndex[C](f: V => Option[C])(implicit cm: Manifest[C]): scala.collection.Map[C,scala.collection.immutable.Set[(K,V)]] = {
    val idx = new Index[K,V,C](this, f, fact)
    indices ::= idx
    idx
  }
}
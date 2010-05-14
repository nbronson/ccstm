/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// StripedHashMap

package edu.stanford.ppl.ccstm.experimental.impl

import reflect.Manifest
import edu.stanford.ppl.ccstm.experimental.TMap
import edu.stanford.ppl.ccstm.experimental.TMap.{Sink, BoundSink}
import scala.collection.mutable.ArrayBuffer
import edu.stanford.ppl.ccstm.{AtomicFunc, Atomic, Txn}


object StripedHashMap {
  def NumStripes = 16
}

class StripedHashMap[K,V](implicit km: Manifest[K], vm: Manifest[V]) extends TMap[K,V] {
  import StripedHashMap._

  private val underlying = Array.fromFunction({ _ => new ChainingHashMap[K,V] })(NumStripes)

  private def mapFor(k: K) = underlying(k.hashCode & (NumStripes - 1))


  def bind(implicit txn: Txn): TMap.Bound[K,V] = new TMap.AbstractTxnBound[K,V,StripedHashMap[K,V]](txn, this) {
    def elements: Iterator[(K,V)] = {
      underlying.flatMap({ u => u.bind(txn) }).elements
    }
  }

  def nonTxn: TMap.Bound[K,V] = new TMap.AbstractNonTxnBound[K,V,StripedHashMap[K,V]](this) {
    override def get(key: K): Option[V] = mapFor(key).nonTxn.get(key)

    override def put(key: K, value: V): Option[V] = mapFor(key).nonTxn.put(key, value)

    override def removeKey(key: K): Option[V] = mapFor(key).nonTxn.removeKey(key)
    
    def elements: Iterator[(K,V)] = {
      new AtomicFunc[Iterator[(K,V)]] { def body = {
        val buf = new ArrayBuffer[(K,V)]
        for (u <- underlying) buf ++= u.nonTxn
        buf.elements
      }}.run
    }
  }

  
  def isEmpty(implicit txn: Txn): Boolean = {
    !underlying.exists({ u => !u.isEmpty })
  }

  def size(implicit txn: Txn): Int = {
    var s = 0
    for (u <- underlying) s += u.size
    s
  }

  def get(key: K)(implicit txn: Txn): Option[V] = mapFor(key).get(key)

  def removeKey(key: K)(implicit txn: Txn): Option[V] = mapFor(key).removeKey(key)

  def put(key: K, value: V)(implicit txn: Txn): Option[V] = mapFor(key).put(key, value)
}

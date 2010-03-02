/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// BasicHashMap

package edu.stanford.ppl.ccstm.experimental.impl

import reflect.Manifest
import edu.stanford.ppl.ccstm.experimental.TMap
import edu.stanford.ppl.ccstm.experimental.TMap.Bound
import edu.stanford.ppl.ccstm._
import collection.{LazyConflictIntRef, TArray}
import impl.MetaHolder

class BasicHashMap[K,V](implicit km: Manifest[K], vm: Manifest[V]) extends TMap[K,V] {
  private val bucketsRef = Ref(new TArray[BHMBucket[K,V]](16, TArray.MaximizeParallelism))
  private val sizeRef = new LazyConflictIntRef(0)

  private def hash(key: K) = {
    // this is the bit mixing code from java.util.HashMap
    var h = key.hashCode
    h ^= (h >>> 20) ^ (h >>> 12)
    h ^= (h >>> 7) ^ (h >>> 4)
    h
  }


  def nonTxn: Bound[K,V] = new TMap.AbstractNonTxnBound[K,V,BasicHashMap[K,V]](BasicHashMap.this) {

    override def size: Int = sizeRef.nonTxn.get

    def get(key: K): Option[V] = {
      STM.atomic(unbind.get(key)(_))
    }

    override def put(key: K, value: V): Option[V] = {
      STM.atomic(unbind.put(key, value)(_))
    }

    override def removeKey(key: K): Option[V] = {
      STM.atomic(unbind.removeKey(key)(_))
    }

    protected def transformIfDefined(key: K,
                                     pfOrNull: PartialFunction[Option[V],Option[V]],
                                     f: Option[V] => Option[V]): Boolean = {
      STM.atomic(unbind.transformIfDefined(key, pfOrNull, f)(_))
    }

    def iterator: Iterator[Tuple2[K,V]] = {
      throw new UnsupportedOperationException
    }
  }


  def bind(implicit txn0: Txn): Bound[K,V] = new TMap.AbstractTxnBound[K,V,BasicHashMap[K,V]](txn0, BasicHashMap.this) {

    def iterator: Iterator[Tuple2[K,V]] = {
      throw new UnsupportedOperationException
    }
  }

  def isEmpty(implicit txn: Txn): Boolean = sizeRef > 0

  def size(implicit txn: Txn): Int = sizeRef.get

  def get(key: K)(implicit txn: Txn): Option[V] = {
    val h = hash(key)
    val buckets = bucketsRef.get
    return get(key, buckets(h & (buckets.length - 1)))
  }

  private def get(key: K, bucket: BHMBucket[K,V])(implicit txn: Txn): Option[V] = {
    if (null == bucket) {
      None
    } else if (key == bucket.key) {
      Some(bucket.value)
    } else {
      get(key, bucket.next)
    }
  }

  def put(key: K, value: V)(implicit txn: Txn): Option[V] = {
    val h = hash(key)
    val buckets = bucketsRef.get
    val i = h & (buckets.length - 1)
    val head = buckets(i)
    val z = putExisting(key, value, head)
    if (!z.isEmpty) {
      // update
      z
    } else {
      // insert change
      buckets(i) = new BHMBucket(key, value, head)
      sizeRef += 1
      val n = buckets.length
      if (sizeRef > n - n/4) {
        grow()
      }
      None
    }
  }

  private def putExisting(key: K, value: V, bucket: BHMBucket[K,V])(implicit txn: Txn): Option[V] = {
    if (null == bucket) {
      None
    } else if (key == bucket.key) {
      Some(bucket.valueRef.getAndSet(value))
    } else {
      putExisting(key, value, bucket.next)
    }
  }


  def removeKey(key: K)(implicit txn: Txn): Option[V] = {
    val h = hash(key)
    val buckets = bucketsRef.get
    val i = h & (buckets.length - 1)
    var prev: BHMBucket[K,V] = null
    var node = buckets(i)
    while (null != node) {
      val next = node.next
      if (node.key == key) {
        // hit
        if (null == prev) {
          buckets(i) = next
        } else {
          prev.next = next
        }
        sizeRef -= 1
        return Some(node.value)
      }
      prev = node
      node = next
    }
    return None
  }

  protected def transformIfDefined(key: K,
                                   pfOrNull: PartialFunction[Option[V],Option[V]],
                                   f: Option[V] => Option[V])(implicit txn: Txn): Boolean = {
    throw new UnsupportedOperationException
  }

  private def grow()(implicit txn: Txn) {
    val before = bucketsRef.bind.readForWrite

    val after = new TArray[BHMBucket[K,V]](before.length * 2, TArray.MaximizeParallelism)
    val aMask = after.length - 1
    for (bi <- 0 until before.length) {
      var bucket = before(bi)
      while (null != bucket) {
        val next = bucket.next
        val ai = hash(bucket.key) & aMask
        bucket.next = after(ai)
        after(ai) = bucket
        bucket = next
      }
    }

    bucketsRef := after
  }
}

private class BHMBucket[A,B](val key: A, value0: B, next0: BHMBucket[A,B]) extends MetaHolder {
  import BHMBucket._

  @volatile private var _value: B = value0
  @volatile private var _next: BHMBucket[A,B] = next0

  def value(implicit txn: Txn): B = valueRef.get
  def value_=(v: B)(implicit txn: Txn) { valueRef.set(v) }
  def valueRef = Value[B](this)

  def next(implicit txn: Txn): BHMBucket[A,B] = nextRef.get
  def next_=(v: BHMBucket[A,B])(implicit txn: Txn) { nextRef.set(v) }
  def nextRef = Next[A,B](this)
}

private object BHMBucket {
//  def Value[B] = new TxnFieldUpdater[RBNode[_,B],B](classOf[RBNode[_,B]], "value") {
//    protected def getField(instance: BHMBucket[_,B]): B = instance._value
//    protected def setField(instance: BHMBucket[_,B], v: B) { instance._value = v }
//  }
  private val UntypedValue = new TxnFieldUpdater[BHMBucket[_,Any],Any](classOf[BHMBucket[_,Any]], "value") {
    protected def getField(instance: BHMBucket[_,Any]): Any = instance._value
    protected def setField(instance: BHMBucket[_,Any], v: Any) { instance._value = v }
  }
  def Value[B] = UntypedValue.asInstanceOf[TxnFieldUpdater[BHMBucket[_,B],B]]
  
//  def Next[A,B] = new TxnFieldUpdater[BHMBucket[A,B],BHMBucket[A,B]](classOf[BHMBucket[A,B]], "next") {
//    protected def getField(instance: BHMBucket[A,B]): BHMBucket[A,B] = instance._next
//    protected def setField(instance: BHMBucket[A,B], v: BHMBucket[A,B]) { instance._next = v }
//  }
    private val UntypedNext = new TxnFieldUpdater[BHMBucket[Any,Any],BHMBucket[Any,Any]](classOf[BHMBucket[Any,Any]], "next") {
      protected def getField(instance: BHMBucket[Any,Any]): BHMBucket[Any,Any] = instance._next
      protected def setField(instance: BHMBucket[Any,Any], v: BHMBucket[Any,Any]) { instance._next = v }
    }
    def Next[A,B] = UntypedNext.asInstanceOf[TxnFieldUpdater[BHMBucket[A,B],BHMBucket[A,B]]]
}

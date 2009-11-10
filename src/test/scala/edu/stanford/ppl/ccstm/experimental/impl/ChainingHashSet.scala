/* SnapTree - (c) 2009 Stanford University - PPL */

// ChainingHashSet

package edu.stanford.ppl.ccstm.experimental.impl

import edu.stanford.ppl.ccstm.collection.{TAnyRef, TArray}
import edu.stanford.ppl.ccstm.{Ref, Txn}
import reflect.Manifest

object ChainingHashSet {
  private class Bucket[K,V](val hash: Int, val key: K, val value: V, val next: Bucket[K,V]) {
    def find(h: Int, k: K): Bucket[K,V] = {
      if (hash == h && key == k) {
        this
      } else if (null == next) {
        null
      } else {
        next.find(h, k)
      }
    }

    def remove(b: Bucket[K,V]): Bucket[K,V] = {
      if (this eq b) {
        next
      } else {
        new Bucket(hash, key, value, next.remove(b))
      }
    }
  }
}

class ChainingHashSet[K,V](implicit km: Manifest[K], vm: Manifest[V]) {
  import ChainingHashSet._

  private val buckets = Ref(new TArray[Bucket[K,V]](16))

  private def hash(key: K) = {
    // this is the bit mixing code from java.util.HashMap
    var h = key.hashCode
    h ^= (h >>> 20) ^ (h >>> 12)
    h ^= (h >>> 7) ^ (h >>> 4)
    h
  }

  def get(key: K)(implicit txn: Txn): V = {
    val h = hash(key)
    val array = buckets.get
    val i = h & (array.length - 1)
    val head = array(i)
    if (null == head) {
      null.asInstanceOf[V]
    } else {
      val bucket = head.find(h, key)
      if (null == bucket) {
        null.asInstanceOf[V]
      } else {
        bucket.value
      }
    }
  }

  def put(key: K, value: V)(implicit txn: Txn): V = {
    val h = hash(key)
    val array = buckets.get
    val i = h & (array.length - 1)
    var head = array(i)
    val prev = (if (null == head) {
      null.asInstanceOf[V]
    } else {
      val bucket = head.find(h, key)
      if (null == bucket) {
        null.asInstanceOf[V]
      } else {
        head = head.remove(bucket)
        bucket.value
      }
    })
    array(i) = new Bucket(h, key, value, head)
    prev
  }

  def remove(key: K, value: V)(implicit txn: Txn): V = {
    val h = hash(key)
    val array = buckets.get
    val i = h & (array.length - 1)
    var head = array(i)
    if (null == head) {
      null.asInstanceOf[V]
    } else {
      val bucket = head.find(h, key)
      if (null == bucket) {
        null.asInstanceOf[V]
      } else {
        head = head.remove(bucket)
        bucket.value
      }
    }
  }
}

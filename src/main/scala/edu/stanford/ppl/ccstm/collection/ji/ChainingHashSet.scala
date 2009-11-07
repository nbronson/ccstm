/* SnapTree - (c) 2009 Stanford University - PPL */

// ChainingHashSet

package edu.stanford.ppl.ccstm.collection.ji

import edu.stanford.ppl.ccstm.collection.{TAnyRef, TArray}
import edu.stanford.ppl.ccstm.Txn

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

class ChainingHashSet[K,V] {
  import ChainingHashSet._

  private val buckets: TAnyRef[TArray[Bucket[K,V]]]

  private def hash(key: K) {
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
      null
    } else {
      val bucket = head.find(h, key)
      if (null == bucket) {
        null
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
      null
    } else {
      val bucket = head.find(h, key)
      if (null == bucket) {
        null
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
      null
    } else {
      val bucket = head.find(h, key)
      if (null == bucket) {
        null
      } else {
        head = head.remove(bucket)
        bucket.value
      }
    }
  }
}
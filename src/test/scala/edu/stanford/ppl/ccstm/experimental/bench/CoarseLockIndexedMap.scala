/* CCSTM - (c) 2009 Stanford University - PPL */

// CoarseLockIndexedMap

package edu.stanford.ppl.ccstm.experimental.bench

import reflect.Manifest


private object CoarseLockIndexedMap {
  val ImmSet = scala.collection.immutable.Set
  type ImmSet[A] = scala.collection.immutable.Set[A]
  type ROMap[A,B] = scala.collection.Map[A,B]
  type MutMap[A,B] = scala.collection.mutable.Map[A,B]

  
  def entrySetSnapshot[A,B](map: java.util.Map[A,_]): Iterator[(A,B)] = {
    val a = new Array[(A,B)](map.size())
    var i = 0
    val iter = map.entrySet().iterator()
    while (iter.hasNext()) {
      val e = iter.next()
      a(i) = (e.getKey(), e.getValue().asInstanceOf[B])
      i += 1
    }
    a
    return new Iterator[(A,B)] {
      private var pos = 0

      def hasNext: Boolean = pos < a.length

      def next: (A,B) = {
        val z = a(pos)
        pos += 1
        z
      }
    }
  }


  class Index[K,V,C](lock: AnyRef, val func: V => Option[C]) extends ROMap[C,ImmSet[(K,V)]] {
    private val data = new java.util.HashMap[C,ImmSet[(K,V)]]

    //////////// index maintenance

    def indexAdd(key: K, after: V) {
      func(after) match {
        case Some(c) => {
          val e = (key,after)
          val s = data.get(c)
          data.put(c, if (null == s) ImmSet(e) else s + e)
        }
        case None => {}
      }
    }

    def indexUpdate(key: K, before: V, after: V) {
      indexRemove(key, before)
      indexAdd(key, after)
    }

    def indexRemove(key: K, before: V) {
      func(before) match {
        case Some(c) => {
          val s = data.get(c) - ((key,before))
          if (s.isEmpty) data.remove(c) else data.put(c, s)
        }
        case None => {}
      }
    }

    //////////// map implementation

    def size: Int = lock.synchronized {
      data.size()
    }

    def get(c: C): Option[ImmSet[(K,V)]] = lock.synchronized {
      val s = data.get(c)
      if (null == s) None else Some(s)
    }

    def elements: Iterator[(C,ImmSet[(K,V)])] = lock.synchronized {
      entrySetSnapshot(data)
    }
  }
}

/** A simple implementation of <code>IndexedMap</code> that uses a single lock
 *  to protect access to normal hash tables.
 */
class CoarseLockIndexedMap[K,V] extends IndexedMap[K,V] {
  import CoarseLockIndexedMap._

  private val main = new java.util.HashMap[K,AnyRef]
  private var indices: List[Index[K,V,_]] = Nil

  def size: Int = synchronized {
    main.size()
  }

  def get(key: K): Option[V] = synchronized {
    val v = main.get(key)
    if (null == v) None else Some(v.asInstanceOf[V])
  }

  def update(key: K, value: V) { put(key, value) }

  override def put(key: K, value: V): Option[V] = synchronized {
    val prev = main.put(key, value.asInstanceOf[AnyRef])
    if (null == prev) {
      for (i <- indices) i.indexAdd(key, value)
      None
    } else {
      for (i <- indices) i.indexUpdate(key, prev.asInstanceOf[V], value)
      Some(prev.asInstanceOf[V])
    }
  }

  def -=(key: K) { removeKey(key) }

  override def removeKey(key: K): Option[V] = synchronized {
    val prev = main.remove(key)
    if (null != prev) {
      for (i <- indices) i.indexRemove(key, prev.asInstanceOf[V])
      Some(prev.asInstanceOf[V])
    } else {
      None
    }
  }

  def elements: Iterator[(K,V)] = synchronized {
    entrySetSnapshot(main)
  }

  def addIndex[C](f: V => Option[C])(implicit cm: Manifest[C]): scala.collection.Map[C,scala.collection.immutable.Set[(K,V)]] = {
    val idx = new Index[K,V,C](this, f)
    indices ::= idx
    idx
  }
}
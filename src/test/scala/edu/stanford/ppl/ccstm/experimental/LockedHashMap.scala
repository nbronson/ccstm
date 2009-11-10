/* SnapTree - (c) 2009 Stanford University - PPL */

// LockedHashMap

package edu.stanford.ppl.ccstm.experimental

import edu.stanford.ppl.ccstm.Txn


class LockedHashMap[A,B] extends TMap.Bound[A,B] {
  private val underlying = new java.util.HashMap[A,AnyRef]

  def unbind: TMap[A,B] = throw new UnsupportedOperationException
  def context: Option[Txn] = None

  override def isEmpty: Boolean = underlying.synchronized {
    underlying.isEmpty
  }

  def size: Int = underlying.synchronized {
    underlying.size()
  }

  override def apply(key: A): B = underlying.synchronized {
    val v = underlying.get(key)
    if (null eq v) default(key) else NullValue.decode[B](v)
  }

  def get(key: A): Option[B] = underlying.synchronized {
    NullValue.decodeOption[B](underlying.get(key))
  }

  override def put(key: A, value: B): Option[B] = underlying.synchronized {
    NullValue.decodeOption[B](underlying.put(key, NullValue.encode(value)))
  }

  def update(key: A, value: B) {
    underlying.synchronized {
      underlying.put(key, NullValue.encode(value))
    }
  }

  override def removeKey(key: A): Option[B] = underlying.synchronized {
    NullValue.decodeOption[B](underlying.remove(key))
  }

  def -= (key: A) {
    underlying.synchronized {
      underlying.remove(key)
    }
  }

  def transform(key: A, f: (Option[B]) => Option[B]) {
    underlying.synchronized {
      val before = get(key)
      f(before) match {
        case Some(v) => underlying.put(key, NullValue.encode(v))
        case None => if (!before.isEmpty) underlying.remove(key)
      }
    }
  }

  def transformIfDefined(key: A, pf: PartialFunction[Option[B], Option[B]]): Boolean = {
    underlying.synchronized {
      val before = get(key)
      if (pf.isDefinedAt(before)) {
        pf(before) match {
          case Some(v) => underlying.put(key, NullValue.encode(v))
          case None => if (!before.isEmpty) underlying.remove(key)
        }
        true
      } else {
        false
      }
    }
  }

  def elements: Iterator[(A,B)] = {
    // we must copy to get a thread-safe iterator
    val entries = underlying.synchronized {
      underlying.entrySet().toArray()
    }
    return new Iterator[(A,B)] {
      private var pos = 0

      def hasNext: Boolean = pos < entries.length

      def next: (A,B) = {
        val e = entries(pos).asInstanceOf[java.util.Map.Entry[A,AnyRef]]
        pos += 1
        (e.getKey(), NullValue.decode[B](e.getValue()))
      }
    }
  }
}
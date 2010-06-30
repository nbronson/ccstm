package edu.stanford.ppl.stm.stmbench

import stmbench7.backend._
import edu.stanford.ppl.ccstm._
import scala.collection.immutable.TreeMap
import java.lang.Iterable
import java.util.Iterator
import scala.collection.JavaConversions
// IndexImpl

object IndexImpl {
  class BoxedImmutable[A <: IndexKey, B] extends Index[A,B] {
    implicit val cmp = new Ordering[A] {
      def compare(x: A, y: A): Int = x compareTo y
    }
    val underlying = Ref(TreeMap.empty[A,B]).single

    def get(key: A) = underlying().getOrElse(key, null.asInstanceOf[B])

    def put(key: A, value: B) { underlying.transform(_ + (key -> value)) }

    def putIfAbsent(key: A, value: B): B = {
      underlying.getAndTransform({ m => if (m.contains(key)) m else m + (key -> value) }).getOrElse(key, null.asInstanceOf[B])
    }

    def remove(key: A) = underlying transformIfDefined {
      case m if m.contains(key) => m - key
    }

    def iterator = JavaConversions.asIterator(underlying().valuesIterator)

    def getRange(minKey: A, maxKey: A) = JavaConversions.asIterable(underlying().range(minKey, maxKey).values)
  }
}
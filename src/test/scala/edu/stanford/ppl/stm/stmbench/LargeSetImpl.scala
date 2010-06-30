package edu.stanford.ppl.stm.stmbench

import stmbench7.backend.LargeSet
import edu.stanford.ppl.ccstm._
import edu.stanford.ppl.ccstm.collection._
import scala.collection.JavaConversions

// LargeSetImpl

object LargeSetImpl {
  class PredicatedHash[A] extends LargeSet[A] {
    val underlying = (new THashMap2[A,Unit]).single

    def add(e: A) = underlying.put(e, ()).isEmpty
    def remove(e: A) = !underlying.remove(e).isEmpty
    def contains(e: A) = underlying.contains(e)
    def size = underlying.size
    def iterator = JavaConversions.asIterator(underlying.keysIterator)
  }

  class BoxedImmutable[A] extends LargeSet[A] {
    val underlying = Ref(Set.empty[A]).single

    def add(e: A) = underlying transformIfDefined {
      case s if !s.contains(e) => s + e
    }
    def remove(e: A) = underlying transformIfDefined {
      case s if s.contains(e) => s - e
    }
    def contains(e: A) = underlying().contains(e)
    def size = underlying().size
    def iterator = JavaConversions.asIterator(underlying().iterator)
  }
}
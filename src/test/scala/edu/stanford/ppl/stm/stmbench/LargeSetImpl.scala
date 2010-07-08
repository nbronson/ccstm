package edu.stanford.ppl.stm.stmbench

import stmbench7.backend.LargeSet
import edu.stanford.ppl.ccstm._
import edu.stanford.ppl.ccstm.experimental.impl._
import scala.collection.JavaConversions

// LargeSetImpl

object LargeSetImpl {
  class PredicatedHash[A] extends LargeSet[A] {
    val underlying = (new THashMap3[A,Unit]).single

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

  class TwoStage[A] extends LargeSet[A] {
    // this should really be .dynamic, which finds the existing txn but doesn't
    // work outside a txn
    val underlying = Ref[LargeSet[A]](new BoxedImmutable[A]).single

    def add(e: A) = {
      val u = underlying()
      val z = u.add(e)
      if (u.isInstanceOf[BoxedImmutable[_]] && u.size >= 2) {
        val repl = new PredicatedHash[A]
        val iter = u.iterator
        while (iter.hasNext) repl.add(iter.next())
        underlying() = repl
      }
      z
    }
    def remove(e: A) = underlying().remove(e)
    def contains(e: A) = underlying().contains(e)
    def size = underlying().size
    def iterator = underlying().iterator
  }

}

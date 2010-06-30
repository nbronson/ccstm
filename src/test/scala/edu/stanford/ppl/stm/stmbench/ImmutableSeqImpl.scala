package edu.stanford.ppl.stm.stmbench

import stmbench7.backend.ImmutableCollection
import collection.JavaConversions

class ImmutableSeqImpl[A](contents: Seq[A]) extends ImmutableCollection[A] {
  def contains(element: A) = contents.contains(element)
  def size = contents.size
  def iterator = JavaConversions.asIterator(contents.iterator)
}
package edu.stanford.ppl.stm.stmbench

import stmbench7.backend.ImmutableCollection
import collection.JavaConversions

class ImmutableSetImpl[A](contents: Set[A]) extends ImmutableCollection[A] {
  def contains(element: A) = contents.contains(element)
  def size = contents.size
  def iterator = JavaConversions.asIterator(contents.iterator)
}
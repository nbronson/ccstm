package edu.stanford.ppl.stm.stmbench

import stmbench7.backend._
import edu.stanford.ppl.ccstm._
import stmbench7.core.OperationFailedException

object IdPoolImpl {
  class BoxedList(n: Int) extends IdPool {
    val underlying = Ref(List.range(1, n + 1)).single
    
    def putUnusedId(id: Int) { underlying.transform(id :: _) }

    def getId = {
      underlying.getAndTransform(_.drop(1)) match {
        case head :: _ => head
        case _ => throw new OperationFailedException
      }
    }
  }
}
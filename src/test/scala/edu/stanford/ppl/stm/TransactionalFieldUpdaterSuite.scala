/* CCSTM - (c) 2009 Stanford University - PPL */

// TransactionalFieldUpdaterSuite

package edu.stanford.ppl.stm

import edu.stanford.ppl.ccstm._
import org.scalatest.FunSuite


class TransactionalFieldUpdaterSuite extends FunSuite {
  class Obj extends STMImpl.MetadataHolder {
    var field: STMImpl.Data[Int] = STMImpl.initialData(0)
  }

  // currently this class tests only the compile-time visibility of STMImpl traits
}
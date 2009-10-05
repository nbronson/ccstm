/* CCSTM - (c) 2009 Stanford University - PPL */

// TransactionalFieldUpdaterSuite

package edu.stanford.ppl.stm

import edu.stanford.ppl.ccstm._


class TransactionalFieldUpdaterSuite extends STMFunSuite {
  class Obj extends impl.MetaHolder {
    var field: Int = 0
  }

  // currently this class tests only the compile-time visibility of STM impl traits
}
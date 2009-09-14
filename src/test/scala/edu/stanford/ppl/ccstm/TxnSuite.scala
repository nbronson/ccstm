/* CCSTM - (c) 2009 Stanford University - PPL */

// TxnSuite

package edu.stanford.ppl.ccstm


import org.scalatest.FunSuite

class TxnSuite extends FunSuite {

  test("Empty transaction") {
    new Atomic { def body {
      // do nothing
    }}.run
  }

  test("Duplicate binding with old access") {
    val x = TVar(1)
    new Atomic { def body {
      val b1 = x.bind
      assert(b1.elem == 1)
      val b2 = x.bind
      assert(b2.elem == 1)
      b1.elem = 2
      assert(b1.elem == 2)
      assert(b2.elem == 2)
      b2.elem = 3
      assert(b1.elem == 3)
      assert(b2.elem == 3)
    }}.run
    assert(x.nonTxn.elem == 3)
  }
}
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

  test("Counter increment") {
    val x = TVar(1)
    new Atomic { def body {
      x := !x + 1
    }}.run
    assert(x.nonTxn.elem == 2)
  }
}
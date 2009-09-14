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

  test("Counter increment should be fast") {
    val x = TVar(1)
    var best = java.lang.Long.MAX_VALUE
    for (pass <- 0 until 10000) {
      val begin = System.nanoTime
      for (i <- 0 until 10) {
        new Atomic { def body {
          x := !x + 1
        }}.run
      }
      val elapsed = System.nanoTime - begin
      best = best min elapsed
    }
    assert(x.nonTxn.elem == 100001)    
    println("best was " + (best / 10.0) + " nanos/call")

    // we should be able to get less than 2500 nanos, even on a Niagara
    assert(best / 10 < 2500)
  }
}
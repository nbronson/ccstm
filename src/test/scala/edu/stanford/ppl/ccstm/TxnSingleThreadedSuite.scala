/* CCSTM - (c) 2009 Stanford University - PPL */

// TxnSuite

package edu.stanford.ppl.ccstm


import org.scalatest.FunSuite

class TxnSingleThreadedSuite extends FunSuite {

  test("Empty transaction") {
    new Atomic { def body {
      // do nothing
    }}.run
  }

  test("Counter increment") {
    val x = TVar(1)
    new Atomic { def body {
      assert(!x == 1)
      x := !x + 1
      assert(!x == 2)
    }}.run
    assert(x.nonTxn.elem == 2)
  }

  test("Counter increment should be fast") {
    val x = TVar(1)
    var best = java.lang.Long.MAX_VALUE
    for (pass <- 0 until 100000) {
      val begin = System.nanoTime
      for (i <- 0 until 10) {
        new Atomic { def body {
          x := !x + 1
        }}.run
      }
      val elapsed = System.nanoTime - begin
      best = best min elapsed
    }
    assert(x.nonTxn.elem == 1000001)
    println("best was " + (best / 10.0) + " nanos/call")

    // We should be able to get less than 5000 nanos, even on a Niagara.
    // On most platforms we should be able to do much better than this.
    assert(best / 10 < 5000)
  }

  class CustomException extends Exception

  test("Failure atomicity") {
    val x = new TVar(1)
    intercept[CustomException] {
      new Atomic { def body {
        x := 2
        throw new CustomException
      }}.run
    }
    assert(x.nonTxn.elem == 1)
  }
}
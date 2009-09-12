/* CCSTM - (c) 2009 Stanford University - PPL */

// FastPoorRandomSuite

package edu.stanford.ppl.ccstm.impls


import org.scalatest.FunSuite

class FastPoorRandomSuite extends FunSuite {

  test("FastPoorRandom should return a value") {
    val rand = new FastPoorRandom
    rand.nextInt
  }

  test("FastPoorRandom should eventually get reasonable coverage") {
    val rand = new FastPoorRandom
    val bigEndBuckets = new Array[Boolean](64)
    var bigEndEmpty = 64
    val smallEndBuckets = new Array[Boolean](64)
    var smallEndEmpty = 64
    
    while (bigEndEmpty > 0 || smallEndEmpty > 0) {
      val z = rand.nextInt
      val b = (z >> (32 - 6)) & 63
      val s = z & 63
      if (!bigEndBuckets(b)) {
        bigEndBuckets(b) = true
        bigEndEmpty -= 1
      }
      if (!smallEndBuckets(s)) {
        smallEndBuckets(s) = true
        smallEndEmpty -= 1
      }
    }
  }

  test("FastPoorRandom should be fast") {
    val rand = new FastPoorRandom
    var best = java.lang.Long.MAX_VALUE
    for (pass <- 0 until 1000) {
      val begin = System.nanoTime
      for (i <- 0 until 100) rand.nextInt
      val elapsed = System.nanoTime - begin
      best = best min elapsed
    }
    println("best was " + (best / 100.0) + " nanos/call")

    // we should be able to get less than 250 nanos, even on a Niagara
    assert(best / 100 < 250)
  }
}
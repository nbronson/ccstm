/* CCSTM - (c) 2009 Stanford University - PPL */

// FastPoorRandomSuite

package edu.stanford.ppl.ccstm.impl


import org.scalatest.FunSuite

class FastPoorRandomSuite extends FunSuite {

  test("FastPoorRandom should return a value") {
    val rand = FastPoorRandom
    rand.nextInt
  }

  test("FastPoorRandom should eventually get reasonable coverage") {
    val rand = FastPoorRandom
    val bigEndBuckets = new Array[Boolean](64)
    var bigEndEmpty = 64
    val smallEndBuckets = new Array[Boolean](64)
    var smallEndEmpty = 64

    var tries = 0
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
      tries += 1
      if (tries == 10000000) {
        println(smallEndBuckets.map(if (_) "1" else "0").mkString)
        println(bigEndBuckets.map(if (_) "1" else "0").mkString)
        fail
      }
    }
  }

  test("FastPoorRandom should be fast") {
    val rand = FastPoorRandom
    var best = Math.MAX_LONG
    for (pass <- 0 until 1000) {
      val begin = System.nanoTime
      var i = 0
      while (i < 10000) {
        rand.nextInt
        i += 1
      }
      val elapsed = System.nanoTime - begin
      best = best min elapsed
    }
    println("best was " + (best / 10000.0) + " nanos/call")

    // we should be able to get less than 250 nanos, even on a Niagara
    assert(best / 10000 < 250)
  }
}
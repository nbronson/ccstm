/* CCSTM - (c) 2009 Stanford University - PPL */

// ImmutableSetTest

package edu.stanford.ppl.scala


object ImmutableSetTest {

  def parallel(numThreads: Int)(block: => Unit) {
    var failure: Throwable = null
    val threads = Array.fromFunction(i => new Thread {
      override def run {
        try {
          block
        } catch {
          case x => failure = x
        }
      }
    })(numThreads)
    for (t <- threads) t.start
    for (t <- threads) t.join
    if (failure != null) throw failure
  }

  def runTest(initialSize: Int, numThreads: Int, passes: Int) {
    val orig = Set.empty ++ (1 to initialSize)
    parallel(numThreads) {
      for (pass <- 0 until passes) {
        var s = orig
        for (e <- (initialSize to 1 by -1)) {
          s -= e
          val obs = s.size
          if (obs != e - 1) {
            throw new Exception("removed e=" + e + ", size was " + obs + ", s=" + s)
          }
        }
      }
    }
  }

  def main(args: Array[String]) {
    println("testing small Set that doesn't promote to HashSet...")
    runTest(4, 2, 1000000)
    println("okay\n")

    println("testing single-threaded HashSet use...")
    runTest(5, 1, 1000000)
    println("okay\n")

    println("testing HashSet.size from multiple threads...")
    runTest(5, 2, 1000000)
    println("okay")
  }
}
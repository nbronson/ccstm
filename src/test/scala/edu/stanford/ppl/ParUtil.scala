/* CCSTM - (c) 2009 Stanford University - PPL */

// ParUtil

package edu.stanford.ppl


object ParUtil {
  case class Precursor(from: Int) {
    def par_until(to: Int) = ParRange(from, to)
  }

  case class ParRange(from: Int, to: Int) {
    def foreach(block: Int => Unit) {
      var failure: Throwable = null
      val threads = Array.fromFunction(i => new Thread {
        override def run {
          try {
            block(from + i)
          } catch {
            case x => failure = x
          }
        }
      })(to - from)
      for (t <- threads) t.start
      for (t <- threads) t.join
      if (failure != null) throw failure
    }
  }

  implicit def intToPrecursor(from: Int) = Precursor(from)

  def parallel(numThreads: Int)(block: => Unit) {
    for (i <- 0 par_until numThreads) {
      block
    }
  }
}
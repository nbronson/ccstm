/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// ParUtil

package edu.stanford.ppl

import java.util.concurrent.CyclicBarrier


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
      if (null != failure) throw failure
    }
  }

  implicit def intToPrecursor(from: Int) = Precursor(from)

  def parallel(numThreads: Int)(block: => Unit) {
    for (i <- 0 par_until numThreads) {
      block
    }
  }

  /** Returns the elapsed milliseconds. */
  def timeParallel(numThreads: Int)(block: Int => Unit): Long = {
    var t0 = 0L
    var t1 = 0L
    val barrier = new CyclicBarrier(numThreads, new Runnable {
      def run() {
        t0 = t1
        t1 = System.currentTimeMillis
      }
    })
    for (i <- 0 par_until numThreads) {
      barrier.await
      block(i)
      barrier.await
    }
    t1 - t0
  }

}

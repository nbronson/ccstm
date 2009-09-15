/* CCSTM - (c) 2009 Stanford University - PPL */

// TokenRingSuite

package edu.stanford.ppl.ccstm


import java.util.concurrent.CyclicBarrier
import org.scalatest.FunSuite

/** This test uses the transactional retry mechanism to pass a token around a
 *  ring of threads.  When there are two threads this is a ping-pong test.  A
 *  separate <code>TVar</code> is used for each handoff.
 */
class TokenRingSuite extends FunSuite {
  test("ping-pong") { tokenRing(2, 1000000) }
  test("three-some") { tokenRing(3, 1000000) }
  test("large ring") { tokenRing(32, 100000) }

  def tokenRing(ringSize: Int, handoffsPerThread: Int) {
    val ready = Array.fromFunction(i => TVar(i == 0))(ringSize)
    val threads = new Array[Thread](ringSize - 1)
    val barrier = new CyclicBarrier(ringSize, new Runnable {
      var start = 0L
      def run {
        val now = System.currentTimeMillis
        if (start == 0) {
          start = now
        } else {
          val elapsed = now - start
          val handoffs = handoffsPerThread * ringSize
          println("tokenRing(" + ringSize + "," + handoffs +
            ")  total_elapsed=" + elapsed + " msec,  throughput=" +
            (handoffs * 1000L) / elapsed + " handoffs/sec,  latency=" +
            (elapsed * 1000000L) / handoffs + " nanos/handoff")
        }
      }
    })

    for (index <- 0 until ringSize) {
      val work = new Runnable {
        def run {
          val next = (index + 1) % ringSize
          barrier.await
          for (h <- 0 until handoffsPerThread) {
            new Atomic { def body {
              if (ready(index).elem == false) retry
              ready(index) := false
              ready(next) := true
            }}.run
          }
          barrier.await
        }
      }
      if (index < ringSize - 1) {
        threads(index) = new Thread(work, "worker " + index)
        threads(index).start
      } else {
        work.run
      }
    }

    for (t <- threads) t.join
  }
}
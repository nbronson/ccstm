/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TokenRingSuite

package edu.stanford.ppl.stm

import edu.stanford.ppl._
import edu.stanford.ppl.ccstm._
import java.util.concurrent.CyclicBarrier


/** This test uses the transactional retry mechanism to pass a token around a
 *  ring of threads.  When there are two threads this is a ping-pong test.  A
 *  separate <code>Ref</code> is used for each handoff.
 */
class TokenRingSuite extends STMFunSuite {
  test("small non-txn threesome") { tokenRing(3, 10000, false) }
  test("small txn threesome") { tokenRing(3, 1000, true) }

  test("non-txn ping-pong", ExhaustiveTest) { tokenRing(2, 1000000, false) }
  test("non-txn threesome", ExhaustiveTest) { tokenRing(3, 1000000, false) }
  test("non-txn large ring", ExhaustiveTest) { tokenRing(32, 10000, false) }
  test("txn ping-pong", ExhaustiveTest) { tokenRing(2, 100000, true) }
  test("txn threesome", ExhaustiveTest) { tokenRing(3, 100000, true) }
  test("txn large ring", ExhaustiveTest) { tokenRing(32, 10000, true) }

  def tokenRing(ringSize: Int, handoffsPerThread: Int, useTxns: Boolean) {
    val ready = Array.tabulate(ringSize)(i => Ref(i == 0))
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
          println("tokenRing(" + ringSize + "," + handoffsPerThread + "," + useTxns +
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
            if (!useTxns) {
              ready(index).single await { _ == true }
              ready(index).single() = false
              ready(next).single() = true
            } else {
              atomic { implicit t =>
                if (ready(index).get == false) retry
                ready(index)() = false
                ready(next)() = true
              }
            }
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

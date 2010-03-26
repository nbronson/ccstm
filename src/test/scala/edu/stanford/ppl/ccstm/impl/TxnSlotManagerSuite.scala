/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TxnSlotManagerSuite

package edu.stanford.ppl.ccstm.impl


import org.scalatest.FunSuite
import edu.stanford.ppl.ParUtil._
import edu.stanford.ppl.ExhaustiveTest

class TxnSlotManagerSuite extends FunSuite {

  test("simple") {
    val mgr = new TxnSlotManager[String](32, 2)
    val s0 = mgr.assign("0", 0)
    val s1 = mgr.assign("1", 0)
    assert(s0 >= 2 && s0 < 32)
    assert(s1 >= 2 && s1 < 32)
    assert(s0 != s1)
    assert(mgr.lookup(s0) === "0")
    assert(mgr.lookup(s1) === "1")
    mgr.release(s0)
    mgr.release(s1)
  }

  test("request repeat") {
    val mgr = new TxnSlotManager[String](32, 2)
    val s0 = mgr.assign("0", 0)
    mgr.release(s0)
    val s1 = mgr.assign("1", s0)
    mgr.release(s1)
    assert(s0 >= 2 && s0 < 32)
    assert(s0 === s1)
  }

  test("sequential full") {
    val mgr = new TxnSlotManager[String](32, 4)
    var slots = Set.empty[Int]
    for (i <- 4 until 32) slots += mgr.assign("t" + i, 0)
    assert(slots === (Set.empty[Int] ++ (4 until 32)))
    var txns = Set.empty[String]
    for (i <- 4 until 32) txns += mgr.lookup(i)
    assert(txns === (Set.empty[String] ++ ((4 until 32).map("t" + _))))
  }

  test("contended", ExhaustiveTest) {
    val mgr = new TxnSlotManager[String](32, 4)
    val t0 = System.currentTimeMillis
    for (i <- 0 par_until (28*4 + 1)) {
      val s = mgr.assign(Thread.currentThread.getName, i)
      assert(s >= 4 && s < 32)
      Thread.sleep(200)
      mgr.release(s)
    }
    val elapsed = System.currentTimeMillis

    // the pigeonhole principle says that one slot has at least 5 occupants
    assert(elapsed >= 1000)
  }

  test("parallel uncontended 1K") {
    runParallelUncontended(1000)
  }

  test("parallel uncontended 1M", ExhaustiveTest) {
    runParallelUncontended(1000000)
  }

  def runParallelUncontended(count: Int) {
    val mgr = new TxnSlotManager[String](128, 0)
    val bests = Array(Long.MaxValue, Long.MaxValue)
    for (t <- 0 par_until 2) {
      val txn = Thread.currentThread.getName
      for (p <- 0 until 10) {
        var i = 0
        val t0 = System.nanoTime
        var s = 0
        while (i < count) {
          s = mgr.assign(txn, s)
          i += 1
          mgr.release(s)
        }
        val elapsed = System.nanoTime - t0
        bests(t) = bests(t) min elapsed
      }
    }
    val best = bests(0) min bests(1)
    println("best latency for " + count + " assign+release pairs was " + (best / count) + " nanos/pair")
  }
}

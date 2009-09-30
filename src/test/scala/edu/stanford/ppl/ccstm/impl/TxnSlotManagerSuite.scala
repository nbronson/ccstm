/* CCSTM - (c) 2009 Stanford University - PPL */

// TxnSlotManagerSuite

package edu.stanford.ppl.ccstm.impl


import org.scalatest.FunSuite
import edu.stanford.ppl.ParUtil._

class TxnSlotManagerSuite extends FunSuite {

  test("simple") {
    val mgr = new TxnSlotManager[String](32, 2)
    val s0 = mgr.assign("0")
    val s1 = mgr.assign("1")
    assert(s0 >= 2 && s0 < 32)
    assert(s1 >= 2 && s1 < 32)
    assert(s0 != s1)
    assert(mgr(s0) === "0")
    assert(mgr(s1) === "1")
    mgr.release(s0)
    mgr.release(s1)
  }

  test("thread-local repeat") {
    val mgr = new TxnSlotManager[String](32, 2)
    val s0 = mgr.assign("0")
    mgr.release(s0)
    val s1 = mgr.assign("1")
    mgr.release(s1)
    assert(s0 >= 2 && s0 < 32)
    assert(s0 === s1)
  }

  test("sequential full") {
    val mgr = new TxnSlotManager[String](32, 4)
    var slots = Set.empty[Int]
    for (i <- 4 until 32) slots += mgr.assign("t" + i)
    assert(slots === (Set.empty[Int] ++ (4 until 32)))
    var txns = Set.empty[String]
    for (i <- 4 until 32) txns += mgr(i)
    assert(txns === (Set.empty[String] ++ ((4 until 32).map("t" + _))))
  }

  test("contended") {
    val mgr = new TxnSlotManager[String](32, 4)
    val t0 = System.currentTimeMillis
    parallel(28*4 + 1) {
      val s = mgr.assign(Thread.currentThread.getName)
      assert(s >= 4 && s < 32)
      Thread.sleep(200)
      mgr.release(s)
    }
    val elapsed = System.currentTimeMillis

    // the pigeonhole principle says that one slot has at least 5 occupants
    assert(elapsed >= 1000)
  }

  test("parallel uncontended") {
    val mgr = new TxnSlotManager[String](128, 0)
    val bests = Array(Math.MAX_LONG, Math.MAX_LONG)
    for (t <- 0 par_until 2) {
      val txn = Thread.currentThread.getName
      for (p <- 0 until 10) {
        var i = 0
        val t0 = System.currentTimeMillis
        while (i < 1000000) {
          val s = mgr.assign(txn)
          i += 1
          mgr.release(s)
        }
        val elapsed = System.currentTimeMillis - t0
        bests(t) = bests(t) min elapsed
      }
    }
    val best = bests(0) min bests(1)
    println("best latency for 1M assign+release was " + best + " millis")
  }
}
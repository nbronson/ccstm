/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// WakeupManagerSuite

package edu.stanford.ppl.ccstm.impl


import java.util.concurrent.CyclicBarrier
import org.scalatest.FunSuite
import edu.stanford.ppl._

class WakeupManagerSuite extends FunSuite {
  import ParUtil._

  test("single non-blocking") {
    val mgr = new WakeupManager(1, 1)
    val event = mgr.subscribe
    event.addSource("ref", 0)
    mgr.trigger(mgr.prepareToTrigger("ref", 0))
    event.await
  }

  test("duplicate wakeup") {
    val mgr = new WakeupManager(1, 1)
    val event = mgr.subscribe
    event.addSource("ref", 0)
    mgr.trigger(mgr.prepareToTrigger("ref", 0))
    mgr.trigger(mgr.prepareToTrigger("ref", 0))
    event.await
  }

  test("single blocking", ExhaustiveTest) {
    val mgr = new WakeupManager(1, 1)
    val event = mgr.subscribe
    event.addSource("ref", 0)
    val thread = new Thread {
      override def run {
        Thread.sleep(500)
        mgr.trigger(mgr.prepareToTrigger("ref", 0))
      }
    }
    val t0 = System.currentTimeMillis
    thread.start
    event.await
    val elapsed = System.currentTimeMillis - t0
    assert(elapsed >= 500)
    thread.join
  }

  test("multiple wakeups, early trigger") {
    runMultipleWakeupTest(0, 500)
  }

  test("multiple wakeups, late trigger") {
    runMultipleWakeupTest(0, 500)
  }

  test("multiple wakeups, racing trigger") {
    runMultipleWakeupTest(0, 0)
  }

  private def runMultipleWakeupTest(triggerDelay: Int, awaitDelay: Int) {
    val mgr = new WakeupManager
    val barrier = new CyclicBarrier(100)
    for (id <- 0 par_until 100) {
      if (id == 0) {
        barrier.await()
        Thread.sleep(triggerDelay)
        var w = 0L
        for (i <- 1 until 100) {
          w |= mgr.prepareToTrigger("ref", i)
        }
        mgr.trigger(w)
      }
      else {
        val e = mgr.subscribe
        e.addSource("ref", id)
        barrier.await()
        Thread.sleep(awaitDelay)
        e.await()
      }
    }
  }

  test("single ticket handoff, 2 threads, 100K handoffs/thread", ExhaustiveTest) {
    runTicketTest(1, 2, 100000)
  }

  test("single ticket handoff, 3 threads, 100K handoffs/thread", ExhaustiveTest) {
    runTicketTest(1, 3, 100000)
  }

  test("single ticket handoff, 100 threads, 1000 handoffs/thread", ExhaustiveTest) {
    runTicketTest(1, 100, 1000)
  }

  test("separate ticket handoff, 100 threads, 1000 handoffs/thread", ExhaustiveTest) {
    runTicketTest(50, 2, 1000)
  }

  test("overlapped ticket handoff, 10 threads, 10K handoffs/thread", ExhaustiveTest) {
    runTicketTest(2, 5, 10000)
  }

  private def runTicketTest(numTickets: Int, threadsPerTicket: Int, handoffsPerThread: Int) {
    class TicketHolder {
      @volatile var turn = 0
    }
    val tickets = Array.fromFunction(i => new TicketHolder)(numTickets)
    var mgr = new WakeupManager
    val t0 = System.currentTimeMillis
    for (id <- 0 par_until (numTickets * threadsPerTicket)) {
      val ticket = tickets(id / threadsPerTicket)
      val self = id % threadsPerTicket
      val next = (self + 1) % threadsPerTicket
      for (h <- 0 until handoffsPerThread) {
        while (ticket.turn != self) {
          val e = mgr.subscribe
          e.addSource(ticket, self)
          if (ticket.turn != self) e.await
        }
        ticket.turn = next
        mgr.trigger(mgr.prepareToTrigger(ticket, next))
      }
    }
    println((System.currentTimeMillis - t0) + " elapsed (including thread creation/shutdown)")
  }
}

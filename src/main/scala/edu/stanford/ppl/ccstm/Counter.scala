/* CCSTM - (c) 2009 Stanford University - PPL */

// Counter

package edu.stanford.ppl.ccstm

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}


/** A linearizable counter class with adaptive striping. */
class Counter {
  private val SuccessMask = 127
  private val GrowThreshold = 8
  private val MaxSlots = 4 * Runtime.getRuntime.availableProcessors

  private val state = new AtomicReference(Array.fromFunction(i => new AtomicLong)(4), 0)

  /** Returns the current value of the counter. */
  def get: Long = {
    (0L /: state.get._1.map(_.get))(_+_)
  }

  /** Adjusts the current value of the counter by <code>delta</code>. */
  def +=(delta: Long) {
    if (delta == 0) return
    
    val h = System.identityHashCode(Thread.currentThread)
    while (true) {
      val s = state.get
      val i = h & (s._1.length - 1)
      val slot = s._1(i)
      val prev = slot.get
      if (slot.compareAndSet(prev, prev + delta)) {
        if ((prev & SuccessMask) == 0) recordSuccesses(s)
        return
      } else {
        recordFailure(s)
      }
    }
  }

  private def recordSuccesses(s: (Array[AtomicLong],Int)) {
    if (s._1.length < MaxSlots && s._2 > 0) {
      state.compareAndSet(s, (s._1, s._2 / 2))
    }
  }

  private def recordFailure(s: (Array[AtomicLong],Int)) {
    if (s._1.length < MaxSlots) {
      if (s._2 >= GrowThreshold) {
        attemptGrow
      } else {
        state.compareAndSet(s, (s._1, s._2 + 1))
      }
    }
  }

  private def attemptGrow {
    val s = state.get
    val prev = s._1
    val next = Array.fromFunction(i => if (i < prev.length) prev(i) else new AtomicLong)(prev.length * 2)
    state.compareAndSet(s, (next,0))
  }

  override def toString = get.toString 
}
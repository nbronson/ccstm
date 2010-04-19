/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// Counter

package edu.stanford.ppl.ccstm

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}


/** A linearizable counter class with adaptive striping. */
private[ccstm] class Counter {
  private val SuccessMask = 127
  private val GrowThreshold = 8
  private val MaxSlots = 4 * Runtime.getRuntime.availableProcessors

  private val state = new AtomicReference(allocateSlots(4), 0)

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
    val next = allocateSlots(prev.length * 2)
    for (i <- 0 until prev.length) next(i).set(prev(i).get)
    state.compareAndSet(s, (next,0))
  }

  private def allocateSlots(size: Int): Array[AtomicLong] = {
    val a = new Array[AtomicLong](size)
    for (i <- 0 until size) a(i) = new AtomicLong
    a
  }

  override def toString = get.toString
}

/* CCSTM - (c) 2009 Stanford University - PPL */

// FastPoorRandom

package edu.stanford.ppl.ccstm.impl


/** A random number generator that focuses on speed and lack of inter-thread
 *  interference, rather than on the quality of the numbers returned.  The
 *  <code>object FastPoorRandom</code> is striped internally to reduce
 *  contention when accessed from multiple threads.  The <code>class
 *  FastPoorRandom</code> should only be used by a single thread.
 *  <p>
 *  The constants in this 64-bit linear congruential random number generator
 *  are from http://nuclear.llnl.gov/CNP/rng/rngman/node4.html.
 */
private[impl] object FastPoorRandom {
  // TODO: (re)choose the number of slots with a bit more thought
  private def Slots = 1024
  
  private val states = {
    val z = new Array[Long](Slots)
    for (i <- 0 until Slots) z(i) =  i * 0x123456789abcdefL
    z
  }

  def nextInt: Int = {
    val id = System.identityHashCode(Thread.currentThread) & (Slots - 1)


    val next = step(states(id))
    states(id) = next

    extract(next)
  }

  private[FastPoorRandom] def step(x: Long) = x * 2862933555777941757L + 3037000493L
  
  private[FastPoorRandom] def extract(x: Long) = (x >> 30).asInstanceOf[Int]
}

/** A single-threaded random number generator that uses the same algorithm as
 *  the concurrent <code>object FastPoorRandom</code>.
 */
private[impl] class FastPoorRandom {
  import FastPoorRandom._

  private var _state: Long = System.identityHashCode(Thread.currentThread)

  def nextInt: Int = {
    _state = step(_state)
    extract(_state)
  }
}
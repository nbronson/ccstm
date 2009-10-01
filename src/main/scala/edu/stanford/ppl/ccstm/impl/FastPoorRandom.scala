/* CCSTM - (c) 2009 Stanford University - PPL */

// FastPoorRandom

package edu.stanford.ppl.ccstm.impl


/** A thread-safe random number generator that focuses on speed and lack of
 *  inter-thread interference, rather than on the quality of the numbers
 *  returned.  Because it is striped across threads, there is little use in
 *  having more than one instance, so it is an object.
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
    val id = Thread.currentThread.hashCode & (Slots - 1)

    // The constants in this 64-bit linear congruential random number generator
    // are from http://nuclear.llnl.gov/CNP/rng/rngman/node4.html.

    val next = states(id) * 2862933555777941757L + 3037000493L
    states(id) = next

    (next >> 30).asInstanceOf[Int]
  }
}
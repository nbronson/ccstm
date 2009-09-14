/* CCSTM - (c) 2009 Stanford University - PPL */

// FastPoorRandom

package edu.stanford.ppl.ccstm.impls


/** A thread-safe random number generator that focuses on speed and lack of
 *  inter-thread interference, rather than on the quality of the numbers
 *  returned.
 */
private[impls] class FastPoorRandom {
  // TODO: (re)choose the number of slots with a bit more thought
  private def Slots = 1024
  private val states = {
    val z = new Array[Long](Slots)
    for (i <- 0 until Slots) z(i) =  i * 0x123456789abcdefL
    z
  }

  def nextInt: Int = {
    val id = Thread.currentThread.hashCode & (Slots - 1)

    // adapted from Wikipedia

    val s = states(id)

    var m_z = s.asInstanceOf[Int]
    var m_w = (s >> 32).asInstanceOf[Int]

    m_z = 36969 * (m_z & 65535) + (m_z >> 16)
    m_w = 18000 * (m_w & 65535) + (m_w >> 16)

    states(id) = (m_z & 0xffffffffL) | (m_w.asInstanceOf[Long] << 32)

    (m_z << 16) + m_w  /* 32-bit result */
  }

  // nbronson: on my laptop the following implementation is 2 nanoseconds faster (11.9 instead of 13.9)

//  private val states = {
//    val z = new Array[Int](Slots)
//    for (i <- 0 until Slots) z(i) =  i
//    z
//  }
//
//  def nextInt: Int = {
//    val id = Thread.currentThread.hashCode & (Slots - 1)
//
//    val s = states(id) * (2000000000 + 1141592653) + 14142135
//    states(id) = s
//    s
//  }
}
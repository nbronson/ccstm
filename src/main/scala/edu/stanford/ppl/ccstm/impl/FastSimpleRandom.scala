/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// FastSimpleRandom

package edu.stanford.ppl.ccstm.impl


/** A random number generator that focuses on speed and lack of inter-thread
 *  interference, rather than on the quality of the numbers returned.  The
 *  <code>object FastSimpleRandom</code> is striped internally to reduce
 *  contention when accessed from multiple threads.  The <code>class
 *  FastSimpleRandom</code> should only be used by a single thread.
 *  <p>
 *  The constants in this 64-bit linear congruential random number generator
 *  are from http://nuclear.llnl.gov/CNP/rng/rngman/node4.html.
 */
object FastSimpleRandom {
  // TODO: (re)choose the number of slots with a bit more thought
  private def Slots = 1024
  
  private val states = {
    val z = new Array[Long](Slots)
    for (i <- 0 until Slots) z(i) =  i * 0x123456789abcdefL
    z
  }

  def nextInt(): Int = {
    val id = System.identityHashCode(Thread.currentThread) & (Slots - 1)

    val next = step(states(id))
    states(id) = next

    extract(next)
  }
  
  def nextInt(n: Int): Int = {
    require(n > 0)

    var x = -1
    while (x == -1) x = tryClamp(nextInt(), n)
    x
  }

  private[impl] def step(x: Long) = x * 2862933555777941757L + 3037000493L
  
  private def extract(x: Long) = (x >> 30).asInstanceOf[Int]

  /** r is the random, returns -1 on failure. */
  private def tryClamp(r: Int, n: Int): Int = {
    // get a positive number
    val x = r & Int.MaxValue

    if ((n & -n) == n) {
      // for powers of two, we use high bits instead of low bits
      ((x.toLong * n) >> 31).toInt
    } else {
      val z = x % n
      if (x - z + (n - 1) < 0) {
        // x is bigger than floor(MAX_INT/n)*n, so we are not getting an even
        // distribution.  Try again.
        -1
      } else {
        z
      }
    }
  }
}

/** A single-threaded random number generator that uses the same algorithm as
 *  the concurrent <code>object FastSimpleRandom</code>.
 */
final class FastSimpleRandom private (private var _state: Long, dummy: Boolean) {
  import FastSimpleRandom._

  def this(seed: Int) = this(FastSimpleRandom.step(FastSimpleRandom.step(seed)), false)
  def this() = this(System.identityHashCode(Thread.currentThread))

  override def clone = new FastSimpleRandom(_state, false)

  def nextInt(): Int = {
    _state = step(_state)
    extract(_state)
  }

  def nextInt(n: Int): Int = {
    require(n > 0)

    var x = -1
    while (x == -1) x = tryClamp(nextInt(), n)
    x
  }
}

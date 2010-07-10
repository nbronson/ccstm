/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// FastSimpleRandom

package edu.stanford.ppl.ccstm.impl


/** A random number generator that focuses on speed and lack of inter-thread
 *  interference, rather than on the quality of the numbers returned.  The
 *  `object FastSimpleRandom` is striped internally to reduce
 *  contention when accessed from multiple threads.  The `class
 *  FastSimpleRandom` should only be used by a single thread.
 *  <p>
 *  The constants in this 64-bit linear congruential random number generator
 *  are from http://nuclear.llnl.gov/CNP/rng/rngman/node4.html.
 */
private[ccstm] object FastSimpleRandom {
  // 64 byte cache lines are typical, so there are 8 slots per cache line.
  // This means that the probability that any two threads have false sharing is
  // p = 8 / #slots.  If there are n processors, each of which is running 1
  // thread, then the probability that no other threads have false sharing with
  // the current thread is (1-p)^(n-1).  If p is small, that is about
  // 1 - (n-1)p, which is pretty close to 1 - np.  If we want the probability
  // of false conflict for a thread to be less than k, then we need np < k, or
  // p < k/n, or 8/Slots < k/n, or #slots > 8n/k.  If we let k = 1/8, then we
  // get #slots=64*n.
  private val mask = {
    val min = 64 * Runtime.getRuntime.availableProcessors
    var slots = 1
    while (slots < min) slots *= 2
    slots - 1
  }
  
  private val states = Array.tabulate(mask + 1)({ _ * 0x123456789abcdefL })

  def nextInt(): Int = {
    val id = (Thread.currentThread.getId.asInstanceOf[Int] * 13) & mask

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
 *  the concurrent `object FastSimpleRandom`.
 */
private[ccstm] final class FastSimpleRandom private (private var _state: Long, dummy: Boolean) {
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

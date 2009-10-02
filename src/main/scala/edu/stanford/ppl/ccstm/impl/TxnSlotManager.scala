/* CCSTM - (c) 2009 Stanford University - PPL */

// TxnSlotManager

package edu.stanford.ppl.ccstm.impl


import java.util.concurrent.atomic.AtomicReferenceArray

/** This class manages a mapping from active Txn to a bounded integral range.
 *  This allows transaction identities to be packed into metadata.
 */
private[impl] class TxnSlotManager[T <: AnyRef](range: Int, reservedSlots: Int) {
  assert(range >= 16 & (range & (range - 1)) == 0)
  assert(range >= reservedSlots + 16)

  private def nextSlot(tries: Int) = {
    var s = 0
    do {
      s = ((FastPoorRandom.nextInt << 4) | ((-tries >> 1) & 0xf)) & (range - 1)
    } while (s < reservedSlots)
    s
  }

  /** This caches the last slot used by the current thread. */
  private val lastSlot = new ThreadLocal[Int] {
    override def initialValue: Int = nextSlot(0)
  }

  /** CAS on the entries manages the actual acquisition. */
  private val slots = new AtomicReferenceArray[(Int,T)](range)

  def assign(txn: T): Int = {
    val s0 = lastSlot.get
    var s = s0
    var tries = 0
    while ((slots.get(s) ne null) || !slots.compareAndSet(s, null, (1,txn))) {
      s = nextSlot(tries)
      tries += 1
      if (tries > 100) Thread.`yield`
    }
    if (s != s0) lastSlot.set(s)
    s
  }

  /** Returns the slot associated with <code>slot</code> at some instant.  The
   *  returned value may be obsolete before this method returns.
   */
  def lookup(slot:Int): T = slots.get(slot)._2

  /** A non-racy version of <code>lookup</code>, that must be paired with
   *  <code>endLookup</code>.
   */
  def beginLookup(slot: Int): T = {
    var p = slots.get(slot)
    while (p != null && !slots.compareAndSet(slot, p, (p._1 + 1, p._2))) {
      p = slots.get(slot)
    }
    p._2
  }
  
  def endLookup(slot: Int, observed: T) {
    if (observed != null) release(slot)
  }

  def release(slot: Int) {
    var p: (Int,T) = null
    var repl: (Int,T) = null
    do {
      p = slots.get(slot)
      repl = p match {
        case (1, _) => null
        case (n, t) => (n - 1, t)
      }
    } while (!slots.compareAndSet(slot, p, repl))
  }
}
/* CCSTM - (c) 2009 Stanford University - PPL */

// TxnSlotManager

package edu.stanford.ppl.ccstm.impls


import java.util.concurrent.atomic.AtomicReferenceArray

/** This class manages a mapping from active Txn to a bounded integral range.
 *  This allows transaction identities to be packed into metadata.
 */
private[impls] class TxnSlotManager[T <: AnyRef](range: Int, reservedSlots: Int) extends (Int => T) {
  assert(range >= 16 & (range & (range - 1)) == 0)
  assert(range >= reservedSlots + 16)

  private val rand = new FastPoorRandom

  private def nextSlot(tries: Int) = {
    var s = 0
    do {
      s = ((rand.nextInt << 4) | ((-tries >> 1) & 0xf)) & (range - 1)
    } while (s < reservedSlots)
    s
  }

  /** This caches the last slot used by the current thread. */
  private val lastSlot = new ThreadLocal[Int] {
    override def initialValue: Int = nextSlot(0)
  }

  /** CAS on the entries manages the actual acquisition. */
  private val slots = new AtomicReferenceArray[T](range)

  def assign(txn: T): Int = {
    val s0 = lastSlot.get
    var s = s0
    var tries = 0
    while ((slots.get(s) ne null) || !slots.compareAndSet(s, null.asInstanceOf[T], txn)) {
      s = nextSlot(tries)
      tries += 1
      if (tries > 100) Thread.`yield`
    }
    if (s != s0) lastSlot.set(s)
    s
  }

  def apply(slot:Int): T = slots.get(slot)

  def release(slot: Int) {
    slots.set(slot, null.asInstanceOf[T])
  }
}
/* CCSTM - (c) 2009 Stanford University - PPL */

// ConcurrentTrap

package edu.stanford.ppl.ccstm.impl


import java.util.concurrent.atomic.{AtomicReference, AtomicReferenceFieldUpdater}

private[impl] object ConcurrentTrap {
  val headUpdater = (new ConcurrentTrap).newUpdater
}

/** A trap is like a bag, except you can only put things in, not take them out.
 *  The only guarantee is that if the <code>ConcurrentTrap</code> hasn't been
 *  garbage collected, then a strong reference exists to anything that has been
 *  added to the trap.
 */
class ConcurrentTrap {
  // TODO: a more scalable implementation

  @volatile private val _head: List[AnyRef] = Nil

  private[impl] def newUpdater = {
    AtomicReferenceFieldUpdater.newUpdater(classOf[ConcurrentTrap], classOf[List[_]], "_head")
  }

  /** Adds a strong reference from this trap to <code>ref</code>. */
  def +=(ref: AnyRef) {
    while (true) {
      val h = _head
      if (ConcurrentTrap.headUpdater.compareAndSet(this, h, ref :: h)) return
    }
  }
}
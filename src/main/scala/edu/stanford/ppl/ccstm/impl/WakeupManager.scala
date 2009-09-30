/* CCSTM - (c) 2009 Stanford University - PPL */

// WakeupManager

package edu.stanford.ppl.ccstm.impl


import java.util.concurrent.atomic.{AtomicReferenceArray, AtomicLongArray}

private[impl] class WakeupManager(numChannels: Int, numSources: Int) {
  def this() = this(64, 512)

  assert(numChannels > 0 && numChannels <= 64 && (numChannels & (numChannels - 1)) == 0)
  assert(numSources > 0 && (numSources & (numSources - 1)) == 0)

  // To reduce false sharing.  Assume 64 byte cache lines and 4 byte pointers.
  private def ChannelSpacing = 16

  private val pending = new AtomicLongArray(numSources)
  private val events = new AtomicReferenceArray[Event](numChannels * ChannelSpacing)
  
  /** The returned value must later be passed to <code>trigger</code>.
   *  Multiple return values may be passed to a single invocation of
   *  <code>trigger</code> by merging them with bitwise-OR.
   */
  def prepareToTrigger(ref: AnyRef, offset: Int): Long = {
    val i = hash(ref, offset) & (numSources - 1)
    var z = 0L
    do {
      z = pending.get(i)
    } while (z != 0L && !pending.compareAndSet(i, z, 0L))
    z
  }

  /** Completes the wakeups started by <code>prepareToTrigger</code>.  If a
   *  thread completes <code>e = subscribe; e.addSource(r,o)</code> prior to
   *  a call to <code>prepareToTrigger(r,o)</code> call whose return value is
   *  included in <code>wakeups</code>, then any pending call to
   *  <code>e.await</code> will return now and any future calls will return
   *  immediately.
   */
  def trigger(wakeups: Long) {
    var channel = 0
    var w = wakeups
    while (w != 0) {
      if ((w & 0xffffffffL) == 0) {
        w >>>= 32
        channel += 32
      }
      if ((w & 0xffffL) == 0) {
        w >>>= 16
        channel += 16
      }
      if ((w & 0xffL) == 0) {
        w >>>= 8
        channel += 8
      }
      if ((w & 0xfL) == 0) {
        w >>>= 4
        channel += 4
      }
      if ((w & 0x3L) == 0) {
        w >>>= 2
        channel += 2
      }
      if ((w & 0x1L) == 0) {
        assert((w & 2) != 0)
        trigger(channel + 1)
        w >>>= 2
        channel += 2
      } else {
        trigger(channel)
        w >>>= 1
        channel += 1
      }
    }
  }

  private def trigger(channel: Int) {
    val i = channel * ChannelSpacing
    val e = events.get(i)
    if (e != null && events.compareAndSet(i, e, null)) e.trigger
  }

  /** See <code>trigger</code>. */
  def subscribe: Event = {
    // Picking the waiter's identity using the thread hash means that there is
    // a possibility that we will get repeated interference with another thread
    // in a per-VM way, but it minimizes saturation of the pending wakeups,
    // which is quite important.
    subscribe(hash(Thread.currentThread, 0) & (numChannels - 1))
  }

  private def subscribe(channel: Int): Event = {
    val i = channel * ChannelSpacing
    while (true) {
      val existing = events.get(i)
      if (existing != null) return existing
      val fresh = new Event(channel)
      if (events.compareAndSet(i, null, fresh)) return fresh
    }
    throw new Error("unreachable")
  }

  private def hash(ref: AnyRef, offset: Int): Int = {
    // TODO: merge this with WriteBuffer.hash, where should it go?
    var h = System.identityHashCode(ref) ^ (0x40108097 * offset)
    h ^= (h >>> 20) ^ (h >>> 12)
    h ^= (h >>> 7) ^ (h >>> 4)
    h
  }

  class Event(channel: Int) {
    private val mask = 1L << channel
    @volatile private var _triggered = false

    def triggered = _triggered

    def addSource(ref: AnyRef, offset: Int) {
      if (!_triggered) {
        val i = hash(ref, offset) & (numSources - 1)
        var p = 0L
        do {
          p = pending.get(i)
        } while((p & mask) == 0 && !pending.compareAndSet(i, p, p | mask) && !_triggered)
      }
    }

    def await() {
      if (!_triggered) {
        synchronized {
          while (!_triggered) {
            wait
          }
        }
      }
    }

    private[WakeupManager] def trigger() {
      if (!_triggered) {
        synchronized {
          if (!_triggered) {
            _triggered = true
            notifyAll
          }
        }
      }
    }
  }
}
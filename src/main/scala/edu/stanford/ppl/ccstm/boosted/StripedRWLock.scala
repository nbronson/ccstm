/* CCSTM - (c) 2009 Stanford University - PPL */

// StripedRWLock

package edu.stanford.ppl.ccstm.boosted


import java.util.concurrent.locks.{Condition, Lock, ReentrantReadWriteLock, ReadWriteLock}
import java.util.concurrent.TimeUnit

class StripedRWLock extends ReadWriteLock {
  val stripes = Array.fromFunction(i => new ReentrantReadWriteLock)(8)

  val writeLock: Lock = new Lock {
    
    def lock() {
      stripes.foreach(_.writeLock.lock())
    }

    def unlock() {
      stripes.foreach(_.writeLock.unlock())
    }

    def tryLock(): Boolean = {
      var i = 0
      while (i < stripes.length) {
        if (!stripes(i).writeLock.tryLock()) {
          // failure
          while (i > 0) {
            i -= 0
            stripes(i).writeLock.unlock()
          }
          return false
        }
        i += 1
      }
      return true
    }

    def tryLock(time: Long, unit: TimeUnit): Boolean = {
      val deadline = System.nanoTime + unit.toNanos(time)
      var i = 0
      while (i < stripes.length) {
        val remaining = (if (i == 0) unit.toNanos(time) else deadline - System.nanoTime)
        if (!stripes(i).writeLock.tryLock(remaining, TimeUnit.NANOSECONDS)) {
          // failure
          while (i > 0) {
            i -= 0
            stripes(i).writeLock.unlock()
          }
          return false
        }
        i += 1
      }
      return true      
    }

    def lockInterruptibly() { throw new UnsupportedOperationException }

    def newCondition: Condition = { throw new UnsupportedOperationException }
  }

  def readLock: Lock = stripe.readLock

  private def stripeIndex = Thread.currentThread.tid & (stripes.length - 1)
  private def stripe = stripes(stripeIndex)
}
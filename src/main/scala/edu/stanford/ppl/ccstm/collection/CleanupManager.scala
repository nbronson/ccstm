/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// CleanupManager

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm.Txn
import java.util.concurrent.atomic.AtomicReference
import annotation.tailrec


private[ccstm] abstract class CleanupManager[A,B] {

  def cleanup(key: A, value: B)

  def afterCommit(txn: Txn, key: A, value: B) { txn.afterCommit(new Entry(key, value)) }

  def afterRollback(txn: Txn, key: A, value: B) { txn.afterRollback(new Entry(key, value)) }

  def afterCompletion(txn: Txn, key: A, value: B) { txn.afterCompletion(new Entry(key, value)) }

  ////////////////
  
  private def NumStripes = 16
  private def BatchSize = 16

  private val segments = Array.tabulate(NumStripes) { _ => new Segment }

  private def hash(h0: Int): Int = {
    var h = h0
    h += (h << 15) ^ 0xffffcd7d
    h ^= (h >>> 10)
    h += (h << 3)
    h ^= (h >>> 6)
    h += (h << 2) + (h << 14)
    return h ^ (h >>> 16)
  }

  private def segmentFor(key: A): Segment = {
    val h = hash(if (null == key) 0 else key.hashCode)
    return segments((h >>> 28) & 15)
  }

  private class Entry(val key: A, val value: B) extends (Txn => Unit) {
    var size = -1
    var next: Entry = null

    def apply(txn: Txn): Unit = segmentFor(key).enqueue(this)
  }

  private class Segment {
    val head = new AtomicReference[Entry]

    @tailrec final def enqueue(e: Entry) {
      val h = head.get
      val s = if (h == null) 0 else h.size
      if (s == BatchSize) {
        // steal tail while enqueuing e
        e.size = 1
        e.next = null
        if (!head.compareAndSet(h, e)) return enqueue(e)
        process(h)
      } else {
        e.size = 1 + s
        e.next = h
        if (!head.compareAndSet(h, e)) return enqueue(e)
      }
    }

    @tailrec private def process(h: Entry) {
      if (h != null) {
        cleanup(h.key, h.value)
        process(h.next)
      }
    }
  }

}
/* CCSTM - (c) 2009 Stanford University - PPL */

// CleanableRef

package edu.stanford.ppl.ccstm.collection.ji

import java.lang.ref._


private object CleanableRef {
  private val _queue = new ReferenceQueue[AnyRef]
  def queue[T] = _queue.asInstanceOf[ReferenceQueue[T]]

  new Thread("CleanableRef cleaner") {
    setDaemon(true)

    override def run() {
      while (true) {
        try {
          val ref = _queue.remove().asInstanceOf[CleanableRef[_]]
          ref.cleanup()
        } catch {
          case x => x.printStackTrace
        }
      }
    }
  }.start()
}

abstract class CleanableRef[T](value: T) extends WeakReference[T](value, CleanableRef.queue) {
  def cleanup()
}
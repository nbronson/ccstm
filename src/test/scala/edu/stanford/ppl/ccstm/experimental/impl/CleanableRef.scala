/* CCSTM - (c) 2009 Stanford University - PPL */

// CleanableRef

package edu.stanford.ppl.ccstm.experimental.impl

import java.lang.ref._


private object CleanableRef {
  val CleaningThreads = {
    val min = System.getProperty("cleaning-threads", (Runtime.getRuntime.availableProcessors / 4).toString).toInt
    var i = 1
    while (i < min) i *= 2
    i
  }

  private val queues = Array.fromFunction(newQueue(_))(CleaningThreads)
  private def newQueue(i: Int) = {
    val queue = new ReferenceQueue[AnyRef]

    new Thread("CleanableRef cleaner #" + i) {
      setDaemon(true)

      override def run() {
        while (true) {
          try {
            val ref = queue.remove().asInstanceOf[CleanableRef[_]]
            ref.cleanup()
          } catch {
            case x => x.printStackTrace
          }
        }
      }
    }.start()

    queue
  }

  def myQueue[T] = queues(System.identityHashCode(Thread.currentThread) & (CleaningThreads - 1)).asInstanceOf[ReferenceQueue[T]]
}

abstract class CleanableRef[T](value: T) extends WeakReference[T](value, CleanableRef.myQueue[T]) {
  def cleanup()
}

/* CCSTM - (c) 2009 Stanford University - PPL */

// MetaHolder.scala

package edu.stanford.ppl.ccstm.impl


import java.util.concurrent.atomic.AtomicLongFieldUpdater

private object MetaHolder {
  val metadataUpdater = (new MetaHolder {}).newMetaUpdater
}

private[ccstm] abstract class MetaHolder(initialMeta: Long) {
  def this() = this(0L)
  
  @volatile private[ccstm] var meta: Long = initialMeta

  private[ccstm] def metaCAS(before: Long, after: Long) = {
    MetaHolder.metadataUpdater.compareAndSet(this, before, after)
  }

  private[MetaHolder] def newMetaUpdater = {
    AtomicLongFieldUpdater.newUpdater(classOf[MetaHolder], "meta")
  }
}

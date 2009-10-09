/* CCSTM - (c) 2009 Stanford University - PPL */

// MetaHolder.scala

package edu.stanford.ppl.ccstm.impl


import java.util.concurrent.atomic.AtomicLongFieldUpdater

private object MetaHolder {
  val metadataUpdater = (new MetaHolder {}).newMetaUpdater
}

// It would be convenient if this was a trait, but then the actual field would
// change on a per-class basis.  That would require a separate Updater per
// concrete class, with some sort of Class -> AtomicLongFieldUpdater mapping.

abstract class MetaHolder(initialMeta: Long) {
  def this() = this(0L)

  @volatile private[ccstm] var meta: Long = initialMeta

  private[ccstm] def metaCAS(before: Long, after: Long) = {
    MetaHolder.metadataUpdater.compareAndSet(this, before, after)
  }

  private[MetaHolder] def newMetaUpdater = {
    AtomicLongFieldUpdater.newUpdater(classOf[MetaHolder], "meta")
  }
}

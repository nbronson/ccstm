/* CCSTM - (c) 2009 Stanford University - PPL */

// LongMetadataHolder.scala

package edu.stanford.ppl.ccstm


import java.util.concurrent.atomic.AtomicLongFieldUpdater

private object LongMetadataHolder {
  val _metadataUpdater = (new LongMetadataHolder {}).newMetadataUpdater
}

private[ccstm] abstract class LongMetadataHolder {
  @volatile var _metadata: Long = 0L

  def _metadataCAS(before: Long, after: Long) = {
    LongMetadataHolder._metadataUpdater.compareAndSet(this, before, after)
  }

  private[LongMetadataHolder] def newMetadataUpdater = {
    AtomicLongFieldUpdater.newUpdater(classOf[LongMetadataHolder], "_metadata")
  }
}
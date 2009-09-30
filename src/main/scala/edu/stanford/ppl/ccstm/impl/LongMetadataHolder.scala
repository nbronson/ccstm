/* CCSTM - (c) 2009 Stanford University - PPL */

// LongMetadataHolder.scala

package edu.stanford.ppl.ccstm.impl


import java.util.concurrent.atomic.AtomicLongFieldUpdater

private object LongMetadataHolder {
  val metadataUpdater = (new LongMetadataHolder {}).newMetadataUpdater
}

private[ccstm] trait LongMetadataHolder extends MetadataHandle[Long] {
  @volatile private[ccstm] var metadata: Long = 0L

  private[ccstm] def metadataCAS(before: Long, after: Long) = {
    LongMetadataHolder.metadataUpdater.compareAndSet(this, before, after)
  }

  private[LongMetadataHolder] def newMetadataUpdater = {
    AtomicLongFieldUpdater.newUpdater(classOf[LongMetadataHolder], "metadata")
  }
}

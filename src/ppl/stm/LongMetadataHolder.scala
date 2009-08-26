/* LongMetadataHolder
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

package ppl.stm


import java.util.concurrent.atomic.AtomicLongFieldUpdater

private object LongMetadataHolder {
  val _metadataUpdater = (new LongMetadataHolder {}).newMetadataUpdater
}

abstract class LongMetadataHolder {
  @volatile private[stm] var _metadata: Long = 0L

  private[stm] def _metadataCAS(before: Long, after: Long) = {
    LongMetadataHolder._metadataUpdater.compareAndSet(this, before, after)
  }

  private[LongMetadataHolder] def newMetadataUpdater = {
    AtomicLongFieldUpdater.newUpdater(classOf[LongMetadataHolder], "_metadata")
  }
}
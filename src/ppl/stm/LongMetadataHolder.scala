/* LongMetadataHolder
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

package ppl.stm

abstract class LongMetadataHolder {
  @volatile private[stm] var _metadata: Long = 0L  
}
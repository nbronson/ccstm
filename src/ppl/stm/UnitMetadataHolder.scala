/* LongMetadataHolder
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

package ppl.stm


abstract class UnitMetadataHolder {
  private[stm] def _metadata: Unit = {}
  private[stm] def _metadata_=(v: Unit) {}
  private[stm] def _metadataCAS(before: Unit, after: Unit): Boolean = true
}
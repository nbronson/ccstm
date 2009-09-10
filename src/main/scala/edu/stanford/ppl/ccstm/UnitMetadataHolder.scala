/* CCSTM - (c) 2009 Stanford University - PPL */

// UnitMetadataHolder.scala

package edu.stanford.ppl.ccstm


abstract class UnitMetadataHolder {
  private[ccstm] def _metadata: Unit = {}
  private[ccstm] def _metadata_=(v: Unit) {}
  private[ccstm] def _metadataCAS(before: Unit, after: Unit): Boolean = true
}
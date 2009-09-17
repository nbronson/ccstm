/* CCSTM - (c) 2009 Stanford University - PPL */

// UnitMetadataHolder.scala

package edu.stanford.ppl.ccstm


private[ccstm] abstract class UnitMetadataHolder {
  def _metadata: Unit = {}
  def _metadata_=(v: Unit) {}
  def _metadataCAS(before: Unit, after: Unit): Boolean = true
}
/* CCSTM - (c) 2009 Stanford University - PPL */

// LongMetadataHolder.scala

package edu.stanford.ppl.ccstm.impl


private[ccstm] trait UnitMetadataHolder extends MetadataHandle[Unit] {
  private[ccstm] def metadata = ()
  private[ccstm] def metadata_=(v: Unit) {}
  private[ccstm] def metadataCAS(before: Unit, after: Unit) = true
}
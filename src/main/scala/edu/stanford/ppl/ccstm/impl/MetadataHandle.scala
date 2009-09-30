/* CCSTM - (c) 2009 Stanford University - PPL */

// MetadataHandle

package edu.stanford.ppl.ccstm.impl


private[ccstm] trait MetadataHandle[T] {
  private[ccstm] def metadata: T
  private[ccstm] def metadata_=(v: T)
  private[ccstm] def metadataCAS(before: T, after: T): Boolean
}
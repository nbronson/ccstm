/* CCSTM - (c) 2009 Stanford University - PPL */

// InPlaceReadSetHolder

package edu.stanford.ppl.ccstm.impl


private[impl] trait InPlaceReadSetHolder {
  private[impl] var _readCount = 0
  private[impl] var _readAccessors = new Array[MetadataHandle[Long]](16)
  private[impl] var _readVersions = new Array[Long](16)

  private[impl] def addToReadSet(accessor: MetadataHandle[Long], version: Long) {
    if (_readCount == _readAccessors.length) growReadSet()
    _readAccessors(_readCount) = accessor
    _readVersions(_readCount) = version
    _readCount += 1
  }

  private def growReadSet() {
    _readAccessors = java.util.Arrays.copyOf(_readAccessors, _readAccessors.length * 2)
    _readVersions = java.util.Arrays.copyOf(_readVersions, _readVersions.length * 2)
  }
}

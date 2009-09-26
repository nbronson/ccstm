/* CCSTM - (c) 2009 Stanford University - PPL */

// InPlaceReadSetHolder

package edu.stanford.ppl.ccstm.impls


private[impls] trait InPlaceReadSetHolder {
  private[impls] var _readCount = 0
  private[impls] var _readSources = new Array[LongMetadataSource](16)
  private[impls] var _readVersions = new Array[Long](16)

  private[impls] def addToReadSet(source: LongMetadataSource, version: Long) {
    if (_readCount == _readSources.length) growReadSet()
    _readSources(_readCount) = source
    _readVersions(_readCount) = version
    _readCount += 1
  }

  private def growReadSet() {
    _readSources = java.util.Arrays.copyOf(_readSources, _readSources.length * 2)
    _readVersions = java.util.Arrays.copyOf(_readVersions, _readVersions.length * 2)
  }
}

class A extends InPlaceReadSetHolder
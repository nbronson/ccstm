/* CCSTM - (c) 2009 Stanford University - PPL */

// ReadSet

package edu.stanford.ppl.ccstm.impl



private[impl] object StrongRefSet {
  val InitialCapacity = 1024
  val MaxEmptyCapacity = 8192
}

/** A clearable blob of strong references. */
private[impl] class StrongRefSet() {
  import StrongRefSet._

  private var _size = 0
  private var _refs = new Array[AnyRef](InitialCapacity)

  def += (ref: AnyRef) {
    if (_size == _refs.length) grow()
    _refs(_size) = ref
    _size += 1
  }

  private def grow() {
    _refs = java.util.Arrays.copyOf(_refs, _size * 2)
  }

  def clear() {
    if (_size > 0) {
      if (_refs.length > MaxEmptyCapacity) {
        _refs = new Array[AnyRef](InitialCapacity)
      } else {
        java.util.Arrays.fill(_refs, 0, _size, null)
      }
      _size = 0
    }
  }
}
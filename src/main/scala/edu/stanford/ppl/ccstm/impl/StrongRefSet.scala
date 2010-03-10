/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// StrongRefSet

package edu.stanford.ppl.ccstm.impl


private[impl] object StrongRefSet {
  val InitialCapacity = 1024
  val MaxEmptyCapacity = 8192
}

/** A `clear()`able blob of strong references.  The main advantage of using
 *  this class over an `ArrayBuffer` or `java.util.ArrayList` is that we get
 *  precise control over the post-clear capacity without having to rely on
 *  undocumented specifics of the behavior.  Growing is also slightly faster,
 *  because we don't have to look up the references, only pin them. 
 */
private[impl] final class StrongRefSet() {
  import StrongRefSet._

  private var _size = 0
  private var _refs = new Array[AnyRef](InitialCapacity)

  def += (ref: AnyRef) {
    if (_size == _refs.length) grow()
    _refs(_size) = ref
    _size += 1
  }

  private def grow() {
    // no need to copy all of the old references, we can just link into the
    // zero-th element
    val r = _refs
    _refs = new Array[AnyRef](_refs.length * 2)
    _refs(0) = r
    _size = 1
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

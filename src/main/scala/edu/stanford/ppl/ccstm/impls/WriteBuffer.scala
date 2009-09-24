/* CCSTM - (c) 2009 Stanford University - PPL */

// WriteBuffer

package edu.stanford.ppl.ccstm.impls


/** Memory consumption on 32-bit (or 64-bit with compressed OOPs) is
 *  96+16*capacity bytes, on 64-bit is 160+24*capacity bytes.  This means
 *  capacity 8 -> 224 or 352, capacity 16 -> 352 or 544, capacity 32 -> 608 or
 *  928.
 *
 *  TODO: dump the layer of abstraction, and interleave ref+specValue, and hop+offset
 */
class WriteBuffer extends HopscotchLIHashTable(initCap: Int) {
  var _specValues = new Array[Any](initCap)

  def get[T](ref: AnyRef, offset: Int, defaultValue: T): T = {
    val index = find(ref, offset)
    if (index == -1) defaultValue else _specValues(index).asInstanceOf[T]
  }

  def set[T](ref: AnyRef, offset: Int, value: T) {
    val i = findOrAdd(ref, offset) & (_specValues.length - 1)
    _specValues(i) = value
  }


  protected def grow() {
    val oldRefs = _refs
    val oldOffsets = _offsets
    val oldSpecValues = _specValues
    val n = _refs.length
    _size = 0
    _hops = new Array[Int](n * 2)
    _refs = new Array[AnyRef](n * 2)
    _offsets = new Array[Int](n * 2)
    _specValues = new Array[Any](n * 2)
    var i = 0
    while (i < n) {
      if (oldRefs(i) ne null) set(oldRefs(i), oldOffsets(i), oldSpecValues(i))
      i += 1
    }
  }

  protected def relocate(src: Int, dst: Int) {
    _specValues(dst) = _specValues(src)
  }

  override def toString = {
    val buf = new StringBuilder
    buf.append("WriteBuffer(size=").append(_size).append("\n")
    buf.append("%8s | %16s | %7s | %s\n".format("hops", "refs", "offsets", "specValues"))
    for (i <- 0 until _refs.length) {
      buf.append("%08x | %16s | %7d | %s\n".format(_hops(i), _refs(i), _offsets(i), _specValues(i)))
    }
    buf.append(")")
    buf.toString
  }
}
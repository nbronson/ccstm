/* CCSTM - (c) 2009 Stanford University - PPL */

// WriteBuffer

package edu.stanford.ppl.ccstm.impl


private[impl] object WriteBuffer {
  trait Visitor {
    def visit(handle: Handle[_], specValue: Any): Boolean
  }

  val InitialCapacity = 16
  val InitialRealCapacity = 512
  val MaxEmptyRealCapacity = InitialRealCapacity * 8
}

/** Maps Handle[T] to specValue.  Handles are compared using the ref and 
 *  offset.
 */
private[impl] class WriteBuffer {
  import WriteBuffer._

  private var _size = 0
  private var _capacity = InitialCapacity
  private var _entries = new Array[AnyRef](lenForCap(InitialRealCapacity))
  private var _lastInsert = 0

  private def lenForCap(i: Int) = 3 * i
  private def handleI(i: Int) = 3 * i
  private def refI(i: Int) = 3 * i + 1
  private def specValueI(i: Int) = 3 * i + 2

  def isEmpty = _size == 0
  def size = _size

  def get[T](handle: Handle[T]): T = {
    val ref = handle.ref
    val offset = handle.offset
    val hash = STMImpl.hash(ref, offset)
    var i = hash & (_capacity - 1)
    while (true) {
      var r = _entries(refI(i))
      if (r eq null) {
        // miss, read from handle
        return handle.data
      }
      if (r eq ref) {
        val h = _entries(handleI(i))
        if ((h eq handle) || h.asInstanceOf[Handle[_]].offset == offset) {
          // hit
          return _entries(specValueI(i)).asInstanceOf[T]
        }
      }

      // probe
      i = (i + 1) & (_capacity - 1)
    }
    throw new Error("unreachable")
  }

  def put[T](handle: Handle[T], specValue: T) {
    val ref = handle.ref
    val offset = handle.offset
    putImpl(handle, ref, offset, specValue.asInstanceOf[AnyRef])
  }

  private def putImpl(handle: AnyRef, ref: AnyRef, offset: Int, specValue: AnyRef) {
    val hash = STMImpl.hash(ref, offset)
    var i = hash & (_capacity - 1)
    while (true) {
      var r = _entries(refI(i))
      if (r eq null) {
        // miss, insert here
        _lastInsert = i
        _entries(handleI(i)) = handle
        _entries(refI(i)) = ref
        _entries(specValueI(i)) = specValue
        _size += 1
        growIfNeeded()
        return
      }
      if (r eq ref) {
        val h = _entries(handleI(i))
        if ((h eq handle) || h.asInstanceOf[Handle[_]].offset == offset) {
          // hit
          _entries(specValueI(i)) = specValue
          return
        }
      }

      // probe
      i = (i + 1) & (_capacity - 1)
    }
  }

  def allocatingGet[T](handle: Handle[T]): T = {
    val ref = handle.ref
    val offset = handle.offset
    val hash = STMImpl.hash(ref, offset)
    var i = hash & (_capacity - 1)
    while (true) {
      var r = _entries(refI(i))
      if (r eq null) {
        // miss, insert here
        _lastInsert = i
        _entries(handleI(i)) = handle
        _entries(refI(i)) = ref
        val z = handle.data
        _entries(specValueI(i)) = z.asInstanceOf[AnyRef]
        _size += 1
        growIfNeeded()
        return z
      }
      if (r eq ref) {
        val h = _entries(handleI(i))
        if ((h eq handle) || h.asInstanceOf[Handle[_]].offset == offset) {
          // hit
          return _entries(specValueI(i)).asInstanceOf[T]
        }
      }

      // probe
      i = (i + 1) & (_capacity - 1)
    }
    throw new Error("unreachable")
  }

  def getAndTransform[T](handle: Handle[T], f: T => T): T = {
    val ref = handle.ref
    val offset = handle.offset
    val hash = STMImpl.hash(ref, offset)
    var i = hash & (_capacity - 1)
    var done = false
    var v0: T = null.asInstanceOf[T]
    while (!done) {
      var r = _entries(refI(i))
      if (r eq null) {
        // Miss, insert here.  Do the call first so that an exception leaves
        // things in an okay state.
        v0 = handle.data
        val v1 = f(v0)
        _lastInsert = i
        _entries(handleI(i)) = handle
        _entries(refI(i)) = ref
        _entries(specValueI(i)) = v1.asInstanceOf[AnyRef]
        _size += 1
        growIfNeeded()
        done = true
      }
      else if (r eq ref) {
        val h = _entries(handleI(i))
        if ((h eq handle) || h.asInstanceOf[Handle[_]].offset == offset) {
          // hit
          val v0 = _entries(specValueI(i)).asInstanceOf[T]
          val v1 = f(v0)
          _entries(specValueI(i)) = v1.asInstanceOf[AnyRef]
          done = true
        }
      }

      // probe
      i = (i + 1) & (_capacity - 1)
    }
    v0
  }

  def visitBegin: Int = _lastInsert
  def visitHandle(pos: Int) = _entries(handleI(pos)).asInstanceOf[Handle[_]]
  def visitSpecValue(pos: Int) = _entries(specValueI(pos))
  def visitNext(pos: Int): Int = {
    var i = pos
    do {
      i = (i + 1) & (_capacity - 1)
    } while (_entries(handleI(i)) eq null)
    i
  }

  def visit(visitor: Visitor): Boolean = {
    var i = _lastInsert
    var remaining = _size
    while (remaining > 0) {
      val h = _entries(handleI(i))
      if (h ne null) {
        if (!visitor.visit(h.asInstanceOf[Handle[_]], _entries(specValueI(i)))) {
          return false
        }
        remaining -= 1
      }
      i = (i + 1) & (_capacity - 1)
    }
    return true
  }

  def clear() {
    if (_entries.length > lenForCap(MaxEmptyRealCapacity)) {
      // we don't want one large txn to keep the buffer big forever
      _entries = new Array[AnyRef](lenForCap(InitialRealCapacity))
    } else {
      // zero the existing one in-place
      java.util.Arrays.fill(_entries, 0, lenForCap(_capacity), null)
    }
    _size = 0
    _capacity = InitialCapacity
  }

  private def growIfNeeded() {
    // max load factor of 1/2
    if (_size * 2 > _capacity) grow()
  }

  private def grow() {
    val src = (if (lenForCap(2 * _capacity) > _entries.length) {
      // we need to actually reallocate
      val prev = _entries
      _entries = new Array[AnyRef](lenForCap(2 * _capacity))
      prev
    } else {
      // we can reuse _entries, but we must copy out temporarily
      val tmp = java.util.Arrays.copyOf(_entries, lenForCap(_capacity))
      java.util.Arrays.fill(_entries, 0, lenForCap(_capacity), null)
      tmp
    })

    val oldCap = _capacity
    _size = 0
    _capacity *= 2
    var i = 0
    while (i < oldCap) {
      val h = src(handleI(i))
      if (h ne null) {
        putImpl(h, src(refI(i)), h.asInstanceOf[Handle[_]].offset, src(specValueI(i)))
      }
      i += 1
    }
  }
}
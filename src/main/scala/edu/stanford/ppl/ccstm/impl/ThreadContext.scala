/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// ThreadContext

package edu.stanford.ppl.ccstm.impl


/** An object that holds the thread context for the current thread. */
object ThreadContext extends ThreadLocal[ThreadContext] {
  override def initialValue: ThreadContext = new ThreadContext
}

/** A class that holds various objects that benefit from reuse by multiple
 *  transactions that execute on the same thread.
 */
private[impl] final class ThreadContext {
  val rand = new FastSimpleRandom

  private var _callbacks: Callbacks = null
  private var _readSet: ReadSet = null
  private var _writeBuffer: WriteBuffer = null
  private var _strongRefSet: StrongRefSet = null

  var preferredSlot: STMImpl.Slot = rand.nextInt

  def takeCallbacks: Callbacks = {
    if (null == _callbacks) {
      new Callbacks
    } else {
      val z = _callbacks
      _callbacks = null
      z
    }
  }

  def takeReadSet: ReadSet = {
    if (null == _readSet) {
      new ReadSet
    } else {
      val z = _readSet
      _readSet = null
      z
    }
  }

  def takeWriteBuffer: WriteBuffer = {
    if (null == _writeBuffer) {
      new WriteBuffer
    } else {
      val z = _writeBuffer
      _writeBuffer = null
      z
    }
  }

  def takeStrongRefSet: StrongRefSet = {
    if (null == _strongRefSet) {
      new StrongRefSet
    } else {
      val z = _strongRefSet
      _strongRefSet = null
      z
    }
  }

  def put(cb: Callbacks, rs: ReadSet, wb: WriteBuffer, srs: StrongRefSet, slot: STMImpl.Slot) {
    cb.clear()
    _callbacks = cb
    rs.clear()
    _readSet = rs
    wb.clear()
    _writeBuffer = wb
    srs.clear()
    _strongRefSet = srs
    preferredSlot = slot
  }
}

/* CCSTM - (c) 2009 Stanford University - PPL */

// ThreadContext

package edu.stanford.ppl.ccstm.impl


/** An object that holds the thread context for the current thread. */
object ThreadContext extends ThreadLocal[ThreadContext] {
  override def initialValue: ThreadContext = new ThreadContext
}

/** A class that holds various objects that benefit from reuse by multiple
 *  transactions that execute on the same thread.
 */
class ThreadContext {
  val rand = new FastPoorRandom
  
  private var _readSet: ReadSet = null
  private var _writeBuffer: WriteBuffer = null

  var preferredSlot: STMImpl.Slot = rand.nextInt

  def takeReadSet: ReadSet = {
    if (_readSet == null) {
      new ReadSet
    } else {
      val z = _readSet
      _readSet = null
      z
    }
  }

  def takeWriteBuffer: WriteBuffer = {
    if (_writeBuffer == null) {
      new WriteBuffer
    } else {
      val z = _writeBuffer
      _writeBuffer = null
      z
    }
  }

  def put(rs: ReadSet, wb: WriteBuffer, slot: STMImpl.Slot) {
    rs.clear()
    _readSet = rs
    wb.clear()
    _writeBuffer = wb
    preferredSlot = slot
  }
}
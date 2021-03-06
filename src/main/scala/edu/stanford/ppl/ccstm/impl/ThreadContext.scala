/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// ThreadContext

package edu.stanford.ppl.ccstm.impl

import edu.stanford.ppl.ccstm.Txn

/** An object that holds the thread context for the current thread. */
private[ccstm] object ThreadContext extends ThreadLocal[ThreadContext] {
  override def initialValue: ThreadContext = new ThreadContext
}

/** A class that holds various objects that benefit from reuse by multiple
 *  transactions that execute on the same thread.
 */
private[ccstm] final class ThreadContext {
  val rand = new FastSimpleRandom

  private var _txn: Txn = null
  private var _callbacks: Callbacks = null
  private var _readSet: ReadSet = null
  private var _writeBuffer: WriteBuffer = null
  private var _strongRefSet: StrongRefSet = null

  var preferredSlot: STMImpl.Slot = rand.nextInt

  var alternatives: List[Txn => Any] = Nil

  def txn: Txn = _txn
  def txn_=(v: Txn): Unit = {
    if ((_txn eq null) == (v eq null)) throw new IllegalStateException("unmatched txn use")
    _txn = v
  }

  def takeCallbacks(): Callbacks = {
    if (null == _callbacks) {
      new Callbacks
    } else {
      val z = _callbacks
      _callbacks = null
      z
    }
  }

  def takeReadSet(): ReadSet = {
    if (null == _readSet) {
      new ReadSet
    } else {
      val z = _readSet
      _readSet = null
      z
    }
  }

  def takeWriteBuffer(): WriteBuffer = {
    if (null == _writeBuffer) {
      new WriteBuffer
    } else {
      val z = _writeBuffer
      _writeBuffer = null
      z
    }
  }

  def takeStrongRefSet(): StrongRefSet = {
    if (null == _strongRefSet) {
      new StrongRefSet
    } else {
      val z = _strongRefSet
      _strongRefSet = null
      z
    }
  }

  def putCallbacks(cb: Callbacks) {
    if (_callbacks == null) {
      cb.clear()
      _callbacks = cb
    }
  }

  def put(rs: ReadSet, wb: WriteBuffer, srs: StrongRefSet, slot: STMImpl.Slot) {
    rs.clear()
    _readSet = rs
    wb.clear()
    _writeBuffer = wb
    srs.clear()
    _strongRefSet = srs
    preferredSlot = slot
  }
}

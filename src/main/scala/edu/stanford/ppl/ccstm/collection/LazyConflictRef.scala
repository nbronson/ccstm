/* CCSTM - (c) 2009 Stanford University - PPL */

// LazyConflictRef

package edu.stanford.ppl.ccstm.collection


import impl.{STMImpl, Handle}

class LazyConflictRef[T](initialValue: T, factory: T => Ref[T]) extends Ref[T] {

  def this(initialValue: T) = this(initialValue, v => new TAnyRef(v))

  private val underlying: Ref[T] = factory(initialValue)

  private val localHandle = new TxnLocal[Handle[T]] {
    override protected def initialValue(txn: Txn): Handle[T] = {
      val h = new Handle[T] {
        private var _read = false
        private var _value: T = null.asInstanceOf[T]

        private[ccstm] var meta = 0L
        private[ccstm] def metaCAS(before: Long, after: Long): Boolean = {
          assert(meta == before)
          meta = after
          if (STMImpl.owner(before) == STMImpl.UnownedSlot &&
              STMImpl.owner(after) != STMImpl.UnownedSlot) {
            // just acquired write access to this location
            txn.beforeCommit(t => underlying.bind(t).readForWrite)
          }
          true
        }

        private[ccstm] def ref: AnyRef = this
        private[ccstm] def offset: Int = 0

        private[ccstm] def data: T = {
          if (!_read) {
            _read = true
            _value = underlying.map(v => v)(txn)
          }
          _value
        }

        private[ccstm] def data_=(v: T) = {
          val h = underlying.handle2(txn)
          assert(STMImpl.owner(h.meta) == STMImpl.owner(meta))
          h.data = v
        }
      }
      // TODO: handle waiting
      h
    }
  }

  protected def handle(txn: Txn): Handle[T] = {
    if (null == txn || txn.barging) {
      underlying.handle2(txn)
    } else {
      localHandle.get(txn)
    }
  }
}
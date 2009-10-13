/* CCSTM - (c) 2009 Stanford University - PPL */

// LazyConflictRef

package edu.stanford.ppl.ccstm.collection


import impl.{STMImpl, Handle}

class LazyConflictRef[T](initialValue: T, factory: T => Ref[T]) extends Ref[T] {

  def this(initialValue: T) = this(initialValue, v => new TAnyRef(v))

  private val underlying: Ref[T] = factory(initialValue)

  private val localHandle = new TxnLocal[Handle[T]] {
    override protected def initialValue(txn: Txn): Handle[T] = {
      val h = new Handle[T] with (Txn => Unit) {

        private var _read = false
        private var _value: T = null.asInstanceOf[T]

        private[ccstm] var meta = 0L
        
        // before-commit and after-completion handler
        def apply(t: Txn) {
          if (t.status == Txn.Active) {
            // acquire write access to the underlying location before commit
            txn.readForWrite(underlying.handle2(txn))
          } else {
            // If there are any UnrecordedRead-s outstanding, we need to make
            // sure they are now invalid.  If we are rolling back due to an
            // explicit retry, we can't actually cause the appropriate
            // wakeups.  The best we can do is make sure that the waiter
            // retries immediately, and we will use barging mode next time.
            meta += 1
          }
        }

        private[ccstm] def metaCAS(before: Long, after: Long): Boolean = {
          assert(meta == before)
          meta = after
          if (STMImpl.owner(before) == STMImpl.UnownedSlot &&
              STMImpl.owner(after) != STMImpl.UnownedSlot) {
            // just acquired write access to this location
            txn.beforeCommit(this)
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

      txn.afterCompletion(h)
      h
    }
  }

  protected def handle(txn: Txn): Handle[T] = {
    if (null == txn || txn.barging || txn.explicitRetrying) {
      underlying.handle2(txn)
    } else {
      localHandle.get(txn)
    }
  }
}
/* CCSTM - (c) 2009 Stanford University - PPL */

// BoostedMap

package edu.stanford.ppl.ccstm.boosted


private object BoostedMap {
  val NULL = new AnyRef
}

// TODO: store undo log inline in the same TxnLocal as the lock set

/** A boosted <code>ConcurrentHashMap</code>. */
class BoostedMap[A,B] {
  import BoostedMap._

  private val underlying = new java.util.concurrent.ConcurrentHashMap[A,AnyRef]
  private val locks = new BoostingLockSet.Mapped

  def containsKey(key: A)(implicit txn: Txn): Boolean = {
    locks.lockKey(key, txn)
    underlying.containsKey(key)
  }

  def get(key: A)(implicit txn: Txn): Option[B] = {
    locks.lockKey(key, txn)
    unpackOption(underlying.get(key))
  }

  def put(key: A, value: B)(implicit txn: Txn): Option[B] = {
    locks.lockKey(key, txn)
    val prev = underlying.put(key, pack(value))
    txn.afterRollback(t => {
      if (null == prev) {
        underlying.remove(key)
      } else {
        underlying.put(key, prev)
      }
    }, BoostingLockSet.UndoPriority)
    unpackOption(prev)
  }

  def remove(key: A)(implicit txn: Txn): Option[B] = {
    locks.lockKey(key, txn)
    val prev = underlying.remove(key)
    if (null != prev) {
      txn.afterRollback(t => {
        underlying.put(key, prev)
      }, BoostingLockSet.UndoPriority)
    }
    unpackOption(prev)    
  }

  private def pack(v: B): AnyRef = {
    if (null == v) NULL else v.asInstanceOf[AnyRef]
  }

  private def unpack(v: AnyRef): B = {
    (if (v eq NULL) null else v).asInstanceOf[B]
  }

  private def unpackOption(v: AnyRef): Option[B] = {
    if (null == v) None else Some(unpack(v))
  }
}
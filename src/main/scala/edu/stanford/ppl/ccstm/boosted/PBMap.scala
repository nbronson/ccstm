/* CCSTM - (c) 2009 Stanford University - PPL */

// PBMap

package edu.stanford.ppl.ccstm.boosted


private object PBMap {
  val NumLocks = 128

  val NonTxn = {
    val t = new Txn
    t.commit()
    t
  }
}

/** A pessimistically-boosted map implementation. */
class PBMap[A,B] {
  import PBMap._

  private val underlying = new java.util.concurrent.ConcurrentHashMap[A,B]
  private val locks = new java.util.concurrent.atomic.AtomicReferenceArray[Txn](NumLocks)

  def get(key: A)(implicit txn: Txn): Option[B] = {
    val lock = key.hashCode & (NumLocks - 1)
    acquire(lock, txn)

  }

  private def acquire(lock: Int, txn: Txn) {
    while (true) {
      val owner = locks.get(lock)
      if (owner eq txn) {
        // already got it
        return
      }
      if ((owner eq null) && locks.compareAndSet(lock, null, txn)) {
        // newly acquired
        txn.afterCompletion(t => {
          
        })
      }
    }
  }
}
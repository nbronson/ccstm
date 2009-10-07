/* CCSTM - (c) 2009 Stanford University - PPL */

// Atomic.scala

package edu.stanford.ppl.ccstm


/** Works like <code>Atomic</code>, but allows atomic blocks to return a value.
 *
 *  @author Nathan Bronson
 */

abstract class AtomicFunc[Z] extends (Txn => Z) {
  private var _currentTxn: Txn = null

  /** Calls <code>body</code> while providing implicit access to
   *  <code>txn</code>.
   */
  def apply(txn: Txn): Z = {
    assert(null == _currentTxn)
    _currentTxn = txn
    try {
      body
    } finally {
      _currentTxn = null
    }
  }

  /** Returns the transaction currently being attempted by this atomic block,
   *  or null if none.
   */
  implicit def currentTxn: Txn = _currentTxn

  /** Performs the work of this atomic block. */
  def body: Z

  /** Rolls the transaction back, indicating that it should be retried after
   *  one or more of the values read during the transaction have changed.
   *  @throws IllegalStateException if the transaction is not active.
   *  @see edu.stanford.ppl.ccstm.Txn#retry
   */
  def retry = _currentTxn.retry

//  /** Returns an atomic block that when run will perform either the actions of
//   *  this block or the actions of <code>alternative</code>, but not both.
//   */
//  def orElse(alternative: Atomic): Atomic = {
//    val first = this
//    new Atomic {
//      def body { throw new Exception("body of joined atomic block should not be executed") }
//      override def attemptImpl(failureHistory: List[Txn.RollbackCause]): Txn = {
//        val a = first.attemptImpl(failureHistory)
//        if (a.committed) return a
//        val b = alternative.attemptImpl(failureHistory)
//        if (b.committed) return b
//        // merge retry sets, if explicit retry
//      }
//    }
//  }

  /** Performs a single attempt to execute this atomic block in a transaction.
   *  @see edu.stanford.ppl.ccstm.Atomic#attemptAtomic
   */
  def attempt(): Option[Z] = STM.attemptAtomic(this)

  /** Repeatedly attempts to perform the work of <code>body</code> in a
   *  transaction, until an attempt is successfully committed or an exception is
   *  thrown by <code>block</code> or a callback registered during the body's
   *  execution.  On successful commit this method returns.  If the body
   *  throws an exception, the transaction will be rolled back and the
   *  exception will be rethrown from this method without further retries.
   */
  def run(): Z = { STM.atomic(this) }
}
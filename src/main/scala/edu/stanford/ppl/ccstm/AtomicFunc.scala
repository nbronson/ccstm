/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// Atomic.scala

package edu.stanford.ppl.ccstm


/** '''Deprecated:''' Prefer `STM.atomic` with a closure taking a parameter
 *  marked `implicit`.
 *
 *  Works like <code>Atomic</code>, but allows atomic blocks to return a value.
 *
 *  @deprecated Prefer `STM.atomic` with a closure taking a parameter marked `implicit`
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
  def retry(): Nothing = _currentTxn.retry()

  /** Returns an atomic block that when run will perform either the actions of
   *  this block or the actions of <code>alternative</code>, but not both.
   *  @see edu.stanford.ppl.ccstm.STM#atomicOrElse
   */
  def orElse(alternative: AtomicFunc[Z]): AtomicFunc[Z] = {
    val b = bodies ++ alternative.bodies
    new AtomicFunc[Z] {
      def body: Z = { throw new IllegalStateException }
      override def retry(): Nothing = { throw new IllegalStateException }
      override private[AtomicFunc] def bodies = b
      override def run()(implicit mt: MaybeTxn): Z = { STM.atomicOrElse(b:_*)(mt) }
    }
  }

  private[AtomicFunc] def bodies = List(this)

  /** Repeatedly attempts to perform the work of <code>body</code> in a
   *  transaction, until an attempt is successfully committed or an exception is
   *  thrown by <code>block</code> or a callback registered during the body's
   *  execution.  On successful commit this method returns.  If the body
   *  throws an exception, the transaction will be rolled back and the
   *  exception will be rethrown from this method without further retries.
   */
  def run()(implicit mt: MaybeTxn): Z = {
    // we have to pass mt explicitly because of our implicit currentTxn
    STM.atomic(this)(mt)
  }
}

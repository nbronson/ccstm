/* CCSTM - (c) 2009 Stanford University - PPL */

// Atomic.scala

package edu.stanford.ppl.ccstm


/** An abstract class that allows a relatively compact syntax for transactions
 *  under current Scala rules.  The trait performs its magic by declaring an
 *  implicit function that returns a <code>Txn</code>, which means that
 *  anonymous subclasses that implement <code>body</code> have an implicit
 *  transaction in scope.  If/when Scala allows anonymous function parameters
 *  to be marked implicit, this class will probably become obsolete.
 *  <p>
 *  Typical usage:<pre>
 *    val tx: TVar[Int] = ..
 *    val ty: TVar[Int] = ..
 *
 *    new Atomic { def body {
 *      if (tx.elem &gt; 10) ty.elem = 20
 *    }}.run
 *  </pre>
 *
 *  @author Nathan Bronson
 */
abstract class Atomic {
  private var _currentTxn: Txn = null

  /** Returns the transaction currently being attempted by this atomic block,
   *  or null if none.
   */
  implicit def currentTxn: Txn = _currentTxn

  /** Performs the work of this atomic block. */
  def body

  /** Runs the body of this atomic block in a transaction, returning true if it
   *  was successfully committed, false if it was rolled back.  If the body
   *  threw an exception the transaction will roll back and then the exception
   *  will be rethrown from this method.
   */
  def attempt(): Boolean = attemptImpl(Nil).committed

  private def attemptImpl(failureHistory: List[Throwable]): Txn = {
    assert(_currentTxn == null)

    val txn = new Txn(failureHistory)
    _currentTxn = txn
    try {
      body
    }
    catch {
      case x => txn.fail(x)
    }
    _currentTxn = null

    txn.commitAndRethrow()
    txn
  }

  /** Repeatedly calls <code>attempt()</code> until a transaction can be
   *  successfully committed, possibly raising the priority of subsequent
   *  attempts in an implementation-specific manner.  If the body throws an
   *  exception, the transaction will be rolled back and the exception will be
   *  rethrown from this method without retrying the body.
   */
  def run() {
    var hist: List[Throwable] = Nil
    while (true) {
      val txn = attemptImpl(hist)
      if (txn.committed) return
      hist = txn.rollbackCause :: hist
      if (txn.rollbackCause == Txn.ExplicitRetryError) {
        // TODO: handle explicit retry more intelligently
      }
    }
  }
}

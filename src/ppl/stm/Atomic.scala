/* Atomic
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

package ppl.stm

/** An abstract class that allows a relatively compact syntax for transactions
 *  under current Scala rules.  The trait performs its magic by declaring an
 *  implicit function that returns a {@link ppl.stm.Txn}, which means that
 *  anonymous subclasses that implement {@link ppl.stm.Atomic#body body} have
 *  an implicit transaction in scope.  If/when Scala allows anonymous function
 *  parameters to be marked implicit, this class will probably become obsolete.
 *  <p>
 *  Typical usage:<pre>
 *    val tv = new TVar(0)
 *    new Atomic { def body {
 *      if (tv.elem > 10) tv.elem = 20
 *    }}.run
 *  </pre>
 */
abstract class Atomic {
  private var _currentTxn: Txn = _

  /** Returns the transaction currently being attempted by this atomic block,
   *  or null if none.
   */
  implicit def currentTxn: Txn = _currentTxn

  /** Performs the work of this atomic block. */
  def body

  /** Runs the body of this atomic block in a transaction, returning true if it
   *  was successfully committed, false if it was rolled back.  If the body
   *  threw an exception and the transaction is valid, the exception will be
   *  rethrown from this method.  If the body threw an exception but the
   *  transaction was not valid, the exception will be discarded and this
   *  method will return false. 
   */
  def attempt(): Boolean = {
    assert(_currentTxn == null)
    _currentTxn = new Txn
    try {
      body
      _currentTxn.attemptCommit
    }
    catch {
      case RollbackException => false
      case xx => if (_currentTxn.attemptCommit) throw xx else false
    }
    finally {
      assert(_currentTxn.completed)
      _currentTxn = null
    }
  }

  /** Repeatedly calls {@link ppl.stm.Atomic#attempt attempt()} until a
   *  transaction can be successfully committed, possibly raising the priority
   *  of subsequent attempts in an implementation-specific manner.  If the body
   *  throws an exception and the transaction is valid, the exception will be
   *  rethrown from this method.
   */
  def run() {
    while (!attempt()) {} // TODO: something more sophisticated
  }
}
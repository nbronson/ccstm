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

  /** Allows access to a <code>TVar[T]</code> without using the
   *  <code>elem</code> or <code>!</code> methods if used in a context that can
   *  accept a <code>T</code> but cannot accept a <code>TVar</code>.
   *  <p>
   *  TODO: Reevaluate if this is a good idea.
   *  <p>
   *  Pros: less clutter when implicit conversion is applicable.  Cons: caller
   *  must consider whether or not the implicit conversion does the right
   *  thing.  This can be considered a tradeoff between syntactic and semantic
   *  complexity.  Pay special attempt to <code>constant == tvar</code>.  
   */
  implicit def implicitRead[T](v: TVar[T]): T = v.elem

  /** Performs the work of this atomic block. */
  def body

  /** Calls <code>Txn.retry</code> on the current transaction.
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

  /** Runs the body of this atomic block in a transaction, returning true if it
   *  was successfully committed, false if it was rolled back.  If the body
   *  threw an exception the transaction will roll back and then the exception
   *  will be rethrown from this method.
   */
  def attempt(): Boolean = attemptImpl(Nil).committed

  private def attemptImpl(failureHistory: List[Txn.RollbackCause]): Txn = {
    assert(_currentTxn == null)

    val txn = new Txn(failureHistory)
    _currentTxn = txn
    try {
      body
    }
    catch {
      case RollbackError => {}
      case x => txn.forceRollback(Txn.UserExceptionCause(x))
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
    var hist: List[Txn.RollbackCause] = Nil
    while (true) {
      val txn = attemptImpl(hist)
      if (txn.committed) return
      hist = txn.rollbackCause :: hist
      txn.rollbackCause match {
        case x: Txn.ExplicitRetryCause => {
          Txn.awaitRetry(x)
        }
        case _ => {}
      }
    }
  }
}

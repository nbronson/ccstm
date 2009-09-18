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
 *    val tx: Ref[Int] = ..
 *    val ty: Ref[Int] = ..
 *
 *    new Atomic { def body {
 *      if (!tx &gt; 10) ty := 20
 *    }}.run
 *  </pre>
 *
 *  @author Nathan Bronson
 */
abstract class Atomic extends (Txn => Unit) {
  private var _currentTxn: Txn = null

  /** Calls <code>body</code> while providing implicit access to
   *  <code>txn</code>.
   */
  def apply(txn: Txn): Unit = {
    assert(_currentTxn == null)
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

  // TODO: reevaluate
  /** Allows access to a <code>Ref[T]</code> in a transaction without using the
   *  <code>get</code> or <code>unary_!</code> methods, if used in a context
   *  that can accept a <code>T</code> but cannot accept a <code>Ref</code>.
   *  <p>
   *  <em>This feature is still under consideration.</em>
   *  <p>
   *  Pros: less clutter when implicit conversion is applicable.  Cons: caller
   *  must consider whether or not the implicit conversion does the right
   *  thing.  This can be considered a tradeoff between syntactic and semantic
   *  complexity.  Pay special attempt to <code>constant == tvar</code>.
   */
  implicit def implicitRead[T](v: Ref[T]): T = v.get

  /** Performs the work of this atomic block. */
  def body

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
  def attempt(): Boolean = STM.attemptAtomic(this)

  /** Repeatedly attempts to perform the work of <code>body</code> in a
   *  transaction, until an attempt is successfully committed or an exception is
   *  thrown by <code>block</code> or a callback registered during the body's
   *  execution.  On successful commit this method returns.  If the body
   *  throws an exception, the transaction will be rolled back and the
   *  exception will be rethrown from this method without further retries.
   */
  def run() { STM.atomic(this) }
}

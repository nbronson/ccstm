/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// Dynamic.scala

package edu.stanford.ppl.ccstm

/** Import `Dynamic._` to get a syntax for atomic blocks that is more concise
 *  than that presented into `STM`.  Usage:
 *
 *  {{{
 *    import edu.stanford.ppl.ccstm.Dynamic._
 *
 *    val x = Ref(10)
 *    atomic {
 *      x() = x() + 1
 *    }
 *  }}}
 *
 *  This object provides execution of atomic blocks with type `=> Z`, rather
 *  than the `Txn => Z` blocks expected by `STM.atomic`.  It also provides an
 *  implicit that will perform a dynamic lookup of the current transaction
 *  during each `Ref` access.  The resulting code will likely be slower than
 *  code using the static `Txn` binding, but it will also be more concise.
 *
 *  @author Nathan Bronson
 */
object Dynamic {

  /** Executes `block` in a transaction, retrying until it succeeds.  See
   * `STM.atomic`.
   */
  def atomic[Z](block: => Z): Z = STM.atomic({ _ => block })(TxnUnknown)

  /** Forwards to `STM.retry`. */
  def retry { STM.retry }

  /** Returns the currently active transaction, or throws an 
   *  `IllegalStateException` if no transaction is attached to the current
   *  thread.
   */
  implicit def currentTxn: Txn = {
    val t = Txn.dynCurrentOrNull
    if (t == null) throw new IllegalStateException("no active transaction")
    t
  }
}

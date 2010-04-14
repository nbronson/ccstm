/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// IntRef

package edu.stanford.ppl.ccstm

import impl.NonTxn


object IntRef {

  trait Bound extends Ref.Bound[Int] with Ordered[Int] {
    def unbind: IntRef

    /** Increments the bound <code>IntRef</code> by <code>delta</code>.
     *  Optimized by some <code>IntRef</code> implementations.
     */
    def += (delta: Int) { transform(_ + delta) }
    def -= (delta: Int) { this += (-delta) }

    def compare(rhs: Int): Int = map(_ compare rhs)

    override def < (rhs: Int): Boolean = map(_ < rhs)
    override def > (rhs: Int): Boolean = map(_ > rhs)
    override def <= (rhs: Int): Boolean = !(this > rhs)
    override def >= (rhs: Int): Boolean = !(this < rhs)

    /** Equivalent to <code>map(_ == ths)</code>, but more concise and
     *  possibly more efficient.
     */
    def ==? (rhs: Int): Boolean = map(_ == rhs)

    /** Equivalent to <code>map(_ != ths)</code>, but more concise and
     *  possibly more efficient.
     */
    def !=? (rhs: Int): Boolean = !(this ==? rhs)
  }
}

/** Adds functions for comparison and increment/decrement to `Ref[Int]`.
 *  Implementations of `Ref[Int]` may provide better performance and/or
 *  concurrency when using these operations.
 */
trait IntRef extends Ref[Int] {

  override def bind(implicit txn: Txn): IntRef.Bound = new impl.TxnBound(this, handle, txn) with IntRef.Bound {
    override val unbind: IntRef = IntRef.this
  }

  override def single: IntRef.Bound = new impl.SingleBound(this, nonTxnHandle, handle) with IntRef.Bound {
    override val unbind: IntRef = IntRef.this

    override def += (delta: Int) {
      Txn.dynCurrentOrNull match {
        case null => NonTxn.getAndAdd(nonTxnHandle, delta)
        case txn: Txn => txn.getAndTransform(txnHandle, { (i: Int) => i + delta })
      }
    }
  }

  override def escaped: IntRef.Bound = new impl.EscapedBound(this, nonTxnHandle) with IntRef.Bound {
    override val unbind: IntRef = IntRef.this

    override def += (delta: Int): Unit = NonTxn.getAndAdd(handle, delta)
  }

  @deprecated("consider replacing with Ref.single, otherwise use Ref.escaped")
  override def nonTxn: IntRef.Bound = escaped 

  //////////////// convenience functions for ints

  /** Increments this <code>IntRef</code> by <code>delta</code>.  Optimized by
   *  some <code>IntRef</code> implementations.
   */
  def +=  (delta: Int)(implicit txn: Txn) { if (delta != 0) transform(_ + delta) }
  def -=  (delta: Int)(implicit txn: Txn) { this += (-delta) }

  def compare(rhs: Int)(implicit txn: Txn): Int = { map(_ compare rhs) }

  def <   (rhs: Int)(implicit txn: Txn): Boolean = { map(_ < rhs) }
  def >   (rhs: Int)(implicit txn: Txn): Boolean = { map(_ > rhs) }
  def <=  (rhs: Int)(implicit txn: Txn): Boolean = { !(this > rhs) }
  def >=  (rhs: Int)(implicit txn: Txn): Boolean = { !(this < rhs) }

  /** Equivalent to <code>map(_ == ths)</code>, but more concise and
   *  possibly more efficient.
   */
  def ==? (rhs: Int)(implicit txn: Txn): Boolean = { map(_ == rhs) }

  /** Equivalent to <code>map(_ != ths)</code>, but more concise and
   *  possibly more efficient.
   */
  def !=? (rhs: Int)(implicit txn: Txn): Boolean = { !(this ==? rhs) }
}

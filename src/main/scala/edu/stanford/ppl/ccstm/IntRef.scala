/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// IntRef

package edu.stanford.ppl.ccstm

import impl.NonTxn


object IntRef {

  trait View extends Ref.View[Int] with Ordered[Int] {
    def unbind: IntRef

    /** Increments the bound <code>IntRef</code> by <code>delta</code>.
     *  Optimized by some <code>IntRef</code> implementations.
     */
    def += (delta: Int) { transform(_ + delta) }
    def -= (delta: Int) { this += (-delta) }

    def compare(rhs: Int): Int = getWith(_ compare rhs)

    override def < (rhs: Int): Boolean = getWith(_ < rhs)
    override def > (rhs: Int): Boolean = getWith(_ > rhs)
    override def <= (rhs: Int): Boolean = !(this > rhs)
    override def >= (rhs: Int): Boolean = !(this < rhs)

    /** Equivalent to <code>getWith(_ == ths)</code>, but more concise and
     *  possibly more efficient.
     */
    def ==? (rhs: Int): Boolean = getWith(_ == rhs)

    /** Equivalent to <code>getWith(_ != ths)</code>, but more concise and
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

  override def bind(implicit txn: Txn): IntRef.View = new impl.TxnView(this, handle, txn) with IntRef.View {
    override val unbind: IntRef = IntRef.this
  }

  override def single: IntRef.View = new impl.SingleView(this, nonTxnHandle, handle) with IntRef.View {
    override val unbind: IntRef = IntRef.this

    override def += (delta: Int) {
      Txn.dynCurrentOrNull match {
        case null => NonTxn.getAndAdd(nonTxnHandle, delta)
        case txn: Txn => txn.getAndTransform(txnHandle, { (i: Int) => i + delta })
      }
    }
  }

  override def escaped: IntRef.View = new impl.EscapedView(this, nonTxnHandle) with IntRef.View {
    override val unbind: IntRef = IntRef.this

    override def += (delta: Int): Unit = NonTxn.getAndAdd(handle, delta)
  }

  @deprecated("consider replacing with Ref.single, otherwise use Ref.escaped")
  override def nonTxn: IntRef.View = escaped

  //////////////// convenience functions for ints

  /** Increments this <code>IntRef</code> by <code>delta</code>.  Optimized by
   *  some <code>IntRef</code> implementations.
   */
  def +=  (delta: Int)(implicit txn: Txn) { if (delta != 0) transform(_ + delta) }
  def -=  (delta: Int)(implicit txn: Txn) { this += (-delta) }

  def compare(rhs: Int)(implicit txn: Txn): Int = { getWith(_ compare rhs) }

  def <   (rhs: Int)(implicit txn: Txn): Boolean = { getWith(_ < rhs) }
  def >   (rhs: Int)(implicit txn: Txn): Boolean = { getWith(_ > rhs) }
  def <=  (rhs: Int)(implicit txn: Txn): Boolean = { !(this > rhs) }
  def >=  (rhs: Int)(implicit txn: Txn): Boolean = { !(this < rhs) }

  /** Equivalent to <code>getWith(_ == ths)</code>, but more concise and
   *  possibly more efficient.
   */
  def ==? (rhs: Int)(implicit txn: Txn): Boolean = { getWith(_ == rhs) }

  /** Equivalent to <code>getWith(_ != ths)</code>, but more concise and
   *  possibly more efficient.
   */
  def !=? (rhs: Int)(implicit txn: Txn): Boolean = { !(this ==? rhs) }
}

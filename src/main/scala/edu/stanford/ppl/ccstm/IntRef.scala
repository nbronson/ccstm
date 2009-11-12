/* CCSTM - (c) 2009 Stanford University - PPL */

// IntRef

package edu.stanford.ppl.ccstm


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
     *  possibly more efficient.  The pneumonic is that you can replace
     *  <code>(!ref == rhs)</code> with <code>(ref ==! rhs)</code.
     */
    def ==! (rhs: Int): Boolean = map(_ == rhs)

    /** Equivalent to <code>map(_ != ths)</code>, but more concise and
     *  possibly more efficient.  The pneumonic is that you can replace
     *  <code>(!ref != rhs)</code> with <code>(ref !=! rhs)</code.
     */
    def !=! (rhs: Int): Boolean = !(this ==! rhs)
  }
}

/** Adds convenience functions for comparison and increment/decrement to
 *  <code>Ref[Int]</code>.  Alternate implementations of <code>Ref[Int]</code>
 *  may be able to obtain greater concurrency when using these operations.
 *  @see edu.stanford.ppl.ccstm.collection.TIntRef
 *  @see edu.stanford.ppl.ccstm.collection.LazyConflictIntRef
 *  @see edu.stanford.ppl.ccstm.collection.StripedIntRef
 */
trait IntRef extends Ref[Int] {

  override def bind(implicit txn: Txn): IntRef.Bound = new Ref.TxnBound(this, handle, txn) with IntRef.Bound {
    override val unbind: IntRef = IntRef.this
  }

  override def nonTxn: IntRef.Bound = new impl.NonTxnBound(this, handle) with IntRef.Bound {
    override val unbind: IntRef = IntRef.this
  }

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
   *  possibly more efficient.  The pneumonic is that you can replace
   *  <code>(!ref == rhs)</code> with <code>(ref ==! rhs)</code.
   */
  def ==! (rhs: Int)(implicit txn: Txn): Boolean = { map(_ == rhs) }

  /** Equivalent to <code>map(_ != ths)</code>, but more concise and
   *  possibly more efficient.  The pneumonic is that you can replace
   *  <code>(!ref != rhs)</code> with <code>(ref !=! rhs)</code.
   */
  def !=! (rhs: Int)(implicit txn: Txn): Boolean = { !(this ==! rhs) }
}
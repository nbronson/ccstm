/* CCSTM - (c) 2009 Stanford University - PPL */

// LazyConflictIntRef

package edu.stanford.ppl.ccstm


object IntCounter {

  trait Bound extends Ref.Bound[Int] with Ordered[Int] {
    def += (delta: Int) { transform(_ + delta) }
    def -= (delta: Int) { this += (-delta) }

    def compare(rhs: Int): Int = (get compare rhs)

    override def < (rhs: Int): Boolean = (get < rhs)
    override def > (rhs: Int): Boolean = (get > rhs)
    override def <= (rhs: Int): Boolean = !(this > rhs)
    override def >= (rhs: Int): Boolean = !(this < rhs)

    /** Equivalent to <code>map(_ == ths)</code>, but more concise and more
     *  efficient.  The pneumonic is that you can replace
     *  <code>(!ref == rhs)</code> with <code>(ref ==! rhs)</code.
     */
    def ==! (rhs: Int): Boolean = (get == rhs)

    /** Equivalent to <code>map(_ != ths)</code>, but more concise and more
     *  efficient.  The pneumonic is that you can replace
     *  <code>(!ref != rhs)</code> with <code>(ref !=! rhs)</code.
     */
    def !=! (rhs: Int): Boolean = !(this ==! rhs)
  }
}

trait IntCounter extends Ref[Int] {

  override def bind(implicit txn: Txn): IntCounter.Bound = throw new Error

  override def nonTxn: IntCounter.Bound = throw new Error

  //////////////// convenience functions for ints

  def += (delta: Int)(implicit txn: Txn) { bind += delta }
  def -= (delta: Int)(implicit txn: Txn) { bind -= delta }

  def compare(rhs: Int)(implicit txn: Txn): Int = { bind.compare(rhs) }
  def <  (rhs: Int)(implicit txn: Txn): Boolean = { bind < rhs }
  def >  (rhs: Int)(implicit txn: Txn): Boolean = { bind > rhs }
  def <= (rhs: Int)(implicit txn: Txn): Boolean = { bind <= rhs }
  def >= (rhs: Int)(implicit txn: Txn): Boolean = { bind >= rhs }
  def ==! (rhs: Int)(implicit txn: Txn): Boolean = { bind ==! rhs }
  def !=! (rhs: Int)(implicit txn: Txn): Boolean = { bind !=! rhs }
}
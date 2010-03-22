/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// Sink

package edu.stanford.ppl.ccstm


object Sink {
  
  /** <code>Sink.Bound</code> defines the contravariant write-only view of a
   *  <code>Ref</code> bound to a particular context. The context may be
   *  either a <code>Txn</code>, which guarantees that all writes will appear
   *  to other contexts as if they were executed atomically at the
   *  transaction's commit (linearization) point, or the non-transactional
   *  context, which guarantees that each write will be linearized (and a
   *  happens-before relationship established) with all other reads and writes
   *  to an equal reference.
   */
  trait Bound[-T] {

    /** The restriction of <code>Ref.Bound.unbind</code> to <code>Sink</code>.
     *  @see edu.stanford.ppl.ccstm.Ref.Bound#unbind
     */
    def unbind: Sink[T]

    /** Returns the `Binding` instance that describes how this bound view was
     *  created.
     *  @see edu.stanford.ppl.ccstm.Single
     *  @see edu.stanford.ppl.ccstm.Escaped
     *  @see edu.stanford.ppl.ccstm.Txn
     */
    def context: Binding

    /** Writes to the bound <code>Ref</code>, equivalent to <code>set</code>.
     *  @see edu.stanford.ppl.ccstm.Sink.Bound#set
     */
    def :=(v: T) { set(v) }

    /** Updates the value refered to by the bound <code>Ref</code>.  If this
     *  view was created by <code>bind(txn)</code> then the new value will not
     *  be visible to other contexts until (and unless) <code>txn</code>
     *  successfully commits.  If this view was created by <code>nonTxn</code>,
     *  the value will be made available immediately, and a happens-before
     *  relationship will be established between this thread and any thread
     *  that reads the <code>v</code> stored by this method.
     *  @param v a value to store in the <code>Ref</code>.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def set(v: T)

    /** Updates the element held by the bound <code>Ref</code> without
     *  blocking and returns true, or does nothing and returns false.  This
     *  method may be used to reduce blocking or rollback in the case of write
     *  contention.  The efficient use of this method may require knowledge of
     *  the conflict detection and versioning strategy used by the specific STM
     *  implementation in use.
     *  @param v a value to store in the <code>Ref</code>.
     *  @return true if the value was stored, false if nothing was done.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def tryWrite(v: T): Boolean
  }
}

/** <code>Sink</code> defines the contravariant write-only interface of a
 *  <code>Ref</code> instance.
 *
 *  @author Nathan Bronson
 */
trait Sink[-T] {

  /** Performs a transactional write.  Equivalent to <code>set(v)</code>.
   *  @see edu.stanford.ppl.ccstm.Sink#set
   */
  def :=(v: T)(implicit txn: Txn)

  /** Performs a transactional write.  The new value will not be visible by
   *  any other transactions or any non-transactional accesses until (and
   *  unless) <code>txn</code> successfully commits.
   *  @param v a value to store in the <code>Ref</code>.
   *  @throws IllegalStateException if <code>txn</code> is not active.
   */
  def set(v: T)(implicit txn: Txn)

  /** The restriction of <code>Ref.bind</code> to <code>Sink</code>.
   *  @see edu.stanford.ppl.ccstm.Ref#bind
   */
  def bind(implicit txn: Txn): Sink.Bound[T]

  /** The restriction of <code>Ref.single</code> to <code>Sink</code>.
   *  @see edu.stanford.ppl.ccstm.Ref#single
   */
  def single: Sink.Bound[T]

  /** The restriction of <code>Ref.escaped</code> to <code>Sink</code>.
   *  @see edu.stanford.ppl.ccstm.Ref#escaped
   */
  def escaped: Sink.Bound[T]

  @deprecated("consider replacing with Sink.single, otherwise use Sink.escaped")
  def nonTxn: Sink.Bound[T]
}

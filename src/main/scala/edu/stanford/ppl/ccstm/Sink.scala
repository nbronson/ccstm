/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// Sink

package edu.stanford.ppl.ccstm


object Sink {
  
  /** `Sink.Bound` defines the contravariant write-only view of a `Ref` that
   *  binds the reference to a desired context.  If the bound view was created
   *  with `Sink.single`, then `mode` will return `Single` and methods on this
   *  instance will be performed (as if) in a new single-operation transaction.
   *  If the bound view was created with `Sink.bind(txn)`, then `mode` will
   *  return the bound `Txn` and methods on this instance will occur as if in
   *  that transaction.  If the bound view was created with `Sink.escaped`,
   *  then `mode` will return `Escaped` and methods on this instance will
   *  ignore any transaction active on the current thread, acting as if they
   *  were performed in a new top-level transaction.
   */
  trait Bound[-T] {

    /** The restriction of <code>Ref.Bound.unbind</code> to <code>Sink</code>.
     *  @see edu.stanford.ppl.ccstm.Ref.Bound#unbind
     */
    def unbind: Sink[T]

    /** Returns the `BindingMode` instance that describes how this bound view
     *  was created.
     *  @see edu.stanford.ppl.ccstm.Single
     *  @see edu.stanford.ppl.ccstm.Escaped
     *  @see edu.stanford.ppl.ccstm.Txn
     */
    def mode: BindingMode

    /** Writes to the bound <code>Ref</code>, equivalent to <code>set</code>.
     *  @see edu.stanford.ppl.ccstm.Sink.Bound#set
     */
    def :=(v: T) { set(v) }

    /** Updates the value referred to by the bound `Ref`.
     *  @param v a value to store in `unbind` using the current binding mode.
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

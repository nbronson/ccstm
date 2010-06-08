/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// Sink

package edu.stanford.ppl.ccstm


object Sink {
  
  /** `Sink.View` defines the contravariant write-only portion of `Ref.View`.
   * 
   *  @author Nathan Bronson
   */
  trait View[-T] {

    /** The restriction of `Ref.View.unbind` to `Sink`.
     *  @see edu.stanford.ppl.ccstm.Ref.View#unbind
     */
    def unbind: Sink[T]

    /** Returns the `AccessMode` instance that describes how this bound view
     *  was created.
     */
    def mode: AccessMode

    /** Writes to `Ref` bound to this view.  Equivalent to `set`, but more
     *  concise in many situations.
     *
     *  Example: {{{
     *    val x = Ref(0)
     * 
     *    // create and use a view on x that performs single-operation transactions
     *    val xs: Sink.View[Int] = x.single
     *    xs() = 10
     * 
     *    // merging the two for concise write of x outside an atomic block
     *    x.single() = 20
     *  }}}
     *  @param v a value to store in `unbind` using the current binding mode.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
   def update(v: T) { set(v) }

    /** Updates the value referred to by the bound `Ref`.  Equivalent to
     *  `update`.
     *  @param v a value to store in `unbind` using the current binding mode.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def set(v: T)

    /** Updates the element held by the bound `Ref` without blocking and
     *  returns true, or does nothing and returns false.  This method may be
     *  used to reduce blocking or rollback in the case of write contention.
     *  The efficient use of this method may require knowledge of the conflict
     *  detection and versioning strategies used by CCSTM.
     *  @param v a value to store in the `Ref`.
     *  @return true if the value was stored, false if nothing was done.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def trySet(v: T): Boolean
  }
}

/** `Sink` defines the contravariant write-only interface of `Ref`.
 *
 *  @author Nathan Bronson
 */
trait Sink[-T] {

  /** Performs a transactional write.  The new value will not be visible by
   *  any other transactions or any non-transactional accesses until (and
   *  unless) `txn` successfully commits.  Equivalent to `set(v)`.
   * 
   *  Example: {{{
   *    val x = Ref(0)
   *    atomic { implicit t =>
   *      ...
   *      x() = 10 // perform a write inside a transaction
   *      ...
   *    }
   *  }}}
   *  @param v a value to store in the `Ref`.
   *  @throws IllegalStateException if `txn` is not active. */
  def update(v: T)(implicit txn: Txn) { set(v) }

  /** Performs a transactional write.  The new value will not be visible by
   *  any other transactions or any non-transactional accesses until (and
   *  unless) `txn` successfully commits.  Equivalent to `update`.
   *  @param v a value to store in the `Ref`.
   *  @throws IllegalStateException if `txn` is not active.
   */
  def set(v: T)(implicit txn: Txn)

  /** The restriction of `Ref.bind` to `Sink`.
   *  @see edu.stanford.ppl.ccstm.Ref#bind
   */
  def bind(implicit txn: Txn): Sink.View[T]

  /** The restriction of `Ref.single` to `Sink`.
   *  @see edu.stanford.ppl.ccstm.Ref#single
   */
  def single: Sink.View[T]

  /** The restriction of `Ref.escaped` to `Sink`.
   *  @see edu.stanford.ppl.ccstm.Ref#escaped
   */
  def escaped: Sink.View[T]

  @deprecated("replace with Sink.single if possible, otherwise use Sink.escaped")
  def nonTxn: Sink.View[T]
}

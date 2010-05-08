/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// Sink

package edu.stanford.ppl.ccstm


object Sink {
  
  /** `Sink.View` defines the contravariant write-only portion of
   *  `Ref.View`.
   */
  trait View[@specialized(Int) -T] {

    /** The restriction of `Ref.View.unbind` to `Sink`.
     *  @see edu.stanford.ppl.ccstm.Ref.View#unbind
     */
    def unbind: Sink[T]

    /** Returns the `AccessMode` instance that describes how this bound view
     *  was created.
     *  @see edu.stanford.ppl.ccstm.Single
     *  @see edu.stanford.ppl.ccstm.Escaped
     *  @see edu.stanford.ppl.ccstm.Txn
     */
    def mode: AccessMode

    /** Writes to the bound `Ref`, equivalent to `set`.
     *  @see edu.stanford.ppl.ccstm.Sink.View#set
     */
    def update(v: T) { set(v) }

    /** Updates the value referred to by the bound `Ref`.
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
trait Sink[@specialized(Int) -T] {

  /** Performs a transactional write.  Equivalent to `set(v)`.
   *  @see edu.stanford.ppl.ccstm.Sink#set
   */
  def update(v: T)(implicit txn: Txn) { set(v) }

  /** Performs a transactional write.  The new value will not be visible by
   *  any other transactions or any non-transactional accesses until (and
   *  unless) `txn` successfully commits.
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

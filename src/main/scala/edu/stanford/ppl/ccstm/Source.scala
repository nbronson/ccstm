/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// Source

package edu.stanford.ppl.ccstm


object Source {

  /** `Source.View` defines the covariant read-only view of `Ref.View`. */
  trait View[+T] {

    /** The restriction of `Ref.View.unbind` to `Source`.
     *  @see edu.stanford.ppl.ccstm.Ref.View#unbind
     */
    def unbind: Source[T]

    /** Returns the `BindingMode` instance that describes how this bound view
     *  was created.
     *  @see edu.stanford.ppl.ccstm.Single
     *  @see edu.stanford.ppl.ccstm.Escaped
     *  @see edu.stanford.ppl.ccstm.Txn
     */
    def mode: BindingMode

    /** Performs a read in the bound context.  Equivalent to `get`.
     *  @see edu.stanford.ppl.ccstm.Source.View#get
     */
    def apply() : T = get

    /** Performs a read of the value managed by the bound `Ref`.
     *  @return the current value of the bound `Ref` in the bound context.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def get: T

    /** Returns `f(get)`, possibly reevaluating `f` to avoid rollback if a
     *  conflicting change is made but the old and new values are equal after
     *  application of `f`.  Requires that `f(x) == f(y)` if `x == y`.
     *  @param f an idempotent function.
     *  @return the result of applying `f` to the value read by this view.
     */
    def map[Z](f: T => Z): Z

    /** Blocks until `pred(get)` is true, in a manner consistent with the bound
     *  context.  Requires that the predicate be safe to reevaluate, and that
     *  `pred(x) == pred(y)` if `x == y`.
     *
     *  If this bound view is `v`, this method is equivalent to
     *
     *  <pre>
     *  STM.atomic { implicit t =>
     *    if (!pref(v.get)) STM.retry
     *  }
     *  </pre>
     *
     *  If you want to wait for a predicate that involves more than one `Ref`
     *  then just use `retry` directly.
     *
     *  @param pred an idempotent predicate.
     *  @see edu.stanford.ppl.ccstm.STM#retry
     */
    def await(pred: T => Boolean)

    /** Returns an `UnrecordedRead` instance that wraps the value that would be
     *  returned from `get`.  This method never adds the bound `Ref` to a
     *  transaction's read set.  The caller is responsible for guaranteeing
     *  that any active transaction's behavior is correct even if the `Ref` is
     *  changed by an outside context before commit.  This method may be useful
     *  for heuristic decisions that can tolerate inconsistent or stale data,
     *  or for methods that register transaction handlers to perform validation
     *  at a semantic level.
     *
     *  If ''C'' is the context in which `unrecordedRead` was called, the
     *  returned instance will remain valid until a change is committed to the
     *  bound `Ref` by any context that is not a child of ''C''.  (This means
     *  that changes committed by ''C'' itself will cause `stillValid` to return
     *  false.)
     *
     *  This method may be called when a transaction is `Active` or when it is 
     *  `Validating` (from a `ReadResource`).  If the current transaction is
     *  `Validating` and the request cannot be satisfied without blocking (due
     *  to an obstruction by a concurrent commit that is updating this
     *  reference) then the current transaction will be rolled back.
     *
     *  Although this method does not add to the read set, the returned value
     *  is consistent with the transaction's previous (recorded) reads.
     *  @return an `UnrecordedRead` instance that holds the read value and
     *      allows explicit revalidation.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def unrecordedRead: UnrecordedRead[T]
    
    /** Returns a `ReleasableRead` instance that wraps the value that would be
     *  returned from `get`.
     *  @return an `ReleasableRead` instance that holds the read value and 
     *      allows the read to be elided from a transaction's read set.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def releasableRead: ReleasableRead[T]
  }
}

/** `Source` defines the covariant read-only interface of `Ref`.
 *
 *  @author Nathan Bronson
 */
trait Source[+T] {

  /** Performs a transactional read and checks that it is consistent with all
   *  reads already made by `txn`.  Equivalent to `get`.
   *  @param txn an active transaction.
   *  @return the value of the `Ref` as observed by `txn`.
   *  @throws IllegalStateException if `txn` is not active.
   */
  def apply()(implicit txn: Txn): T

  /** Performs a transactional read and checks that it is consistent with all
   *  reads already made by `txn`.
   *  @param txn an active transaction.
   *  @return the value of the `Ref` as observed by `txn`.
   *  @throws IllegalStateException if `txn` is not active.
   */
  def get(implicit txn: Txn): T

  /** Returns `f(get)`, possibly reevaluating `f` to avoid rollback if a
   *  conflicting change is made but the old and new values are equal after
   *  application of `f`.  Requires that `f(x) == f(y)` if `x == y`.
   *  @param f an idempotent function.
   *  @return the result of applying `f` to the value contained in this `Ref`.
   */
  def map[Z](f: T => Z)(implicit txn: Txn): Z

  /** The restriction of `Ref.bind` to `Source`.
   *  @see edu.stanford.ppl.ccstm.Ref#bind
   */
  def bind(implicit txn: Txn): Source.View[T]

  /** The restriction of `Ref.single` to `Source`.
   *  @see edu.stanford.ppl.ccstm.Ref#single
   */
  def single: Source.View[T]

  /** The restriction of `Ref.escaped` to `Source`.
   *  @see edu.stanford.ppl.ccstm.Ref#escaped
   */
  def escaped: Source.View[T]

  @deprecated("replace with Source.single if possible, otherwise use Source.escaped")
  def nonTxn: Source.View[T]

  // see Source.View for more operations
}

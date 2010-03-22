/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// Source

package edu.stanford.ppl.ccstm


object Source {

  /** `Source.Bound` defines the covariant read-only view of a `Ref` that binds
   *  the reference to a desired context.  If the bound view was created with
   * `Source.single`, then `mode` will return `Single` and methods on this
   *  instance will be performed (as if) in a new single-operation transaction.
   *  If the bound view was created with `Sink.bind(txn)`, then `mode` will
   *  return the bound `Txn` and methods on this instance will occur as if in
   *  that transaction.  If the bound view was created with `Sink.escaped`,
   *  then `mode` will return `Escaped` and methods on this instance will
   *  ignore any transaction active on the current thread, acting as if they
   *  were performed in a new top-level transaction.
   */
  trait Bound[+T] {

    /** The restriction of <code>Ref.Bound.unbind</code> to a
     *  <code>Source</code>.
     *  @see edu.stanford.ppl.ccstm.Ref.Bound#unbind
     */
    def unbind: Source[T]

    /** Returns the `BindingMode` instance that describes how this bound view
     *  was created.
     *  @see edu.stanford.ppl.ccstm.Single
     *  @see edu.stanford.ppl.ccstm.Escaped
     *  @see edu.stanford.ppl.ccstm.Txn
     */
    def mode: BindingMode

    /** Performs a read in the current context.  Equivalent to <code>get</code>.
     *  @see edu.stanford.ppl.ccstm.Source.Bound#get
     */
    def apply() : T = get

    /** Performs a read of the value managed by the bound `Ref`, using the
     *  access mode requested when this bound view was created.
     *  @return the current value of the bound `Source` using the binding mode.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def get: T

    /** Returns <code>f(get)</code>, possibly reevaluating <code>f</code> to
     *  avoid rollback if a conflicting change is made but both the old and new
     *  values are equal after application of <code>f</code>.  Requires that
     *  <code>f(x) == f(y)</code> if <code>x == y</code>.
     *  @param f an idempotent function.
     *  @return the result of applying <code>f</code> to the value read by this
     *      view.
     */
    def map[Z](f: T => Z): Z

    /** Blocks until `pred(get)` is true, in a manner consistent with the
     *  current binding mode.  Requires that the predicate be safe to
     *  reevaluate, and that `pred(x) == pred(y)` if `x == y`.
     *  @param pred an idempotent predicate.
     *  @see edu.stanford.ppl.ccstm.Txn#retry
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
     *  If *C* is the context in which `unrecordedRead` was called, the
     *  returned instance will remain valid until a change is committed to the
     *  bound `Ref` by any context that is not a child of *C*.  (This means
     *  that changes committed by *C* itself will cause `stillValid` to return
     *  false.)
     *
     *  This method may be called when a transaction is <code>Active</code> or
     *  when it is <code>Validating</code> (from a <code>ReadResource</code>).
     *  If the current transaction is <code>Validating</code> and the request
     *  cannot be satisfied without blocking (due to an obstruction by a
     *  concurrent commit that is updating this reference) then the current
     *  transaction will be rolled back.
     *
     *  Although this method does not add to the read set, the returned value
     *  is consistent with the transaction's previous (recorded) reads.
     *  @return an <code>UnrecordedRead</code> instance that holds the read
     *      value and allows explicit revalidation.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def unrecordedRead: UnrecordedRead[T]
    
    /** Returns a <code>ReleasableRead</code> instance that wraps the value
     *  that would be returned from <code>get</code>.  If this source is bound
     *  to a transactional context, <code>Ref</code> will be added to the
     *  transaction's read set, but it may be removed by a call to
     *  <code>ReleasableRead.release()</code>.  When called from a
     *  non-transactional context this method returns a pre-released
     *  <code>ReleasableRead</code>, which is just an expensive way to call
     *  <code>get</code>.
     *  @return an <code>ReleasableRead</code> instance that holds the read
     *      value and allows the read to be elided from a transaction's read
     *      set.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def releasableRead: ReleasableRead[T]
  }
}

/** <code>Source</code> defines the covariant read-only interface of a
 *  <code>Ref</code> instance.
 *
 *  @author Nathan Bronson
 */
trait Source[+T] {

  /** Performs a transactional read.  Equivalent to <code>get</code>.
   *  @see edu.stanford.ppl.ccstm.Source#get
   */
  def apply()(implicit txn: Txn): T

  /** Performs a transactional read of the value managed by this
   *  <code>Ref</code>.  The returned value will take into account the
   *  writes already performed in this transaction, and the returned value
   *  will be consistent with all reads already performed by
   *  <code>txn</code>.  If there does not exist a point in time at which all
   *  of <code>txn</code>'s reads could have been performed, then it will be
   *  immediately rolled back.
   *  <p>
   *  This method is equivalent to <code>bind(txn).get</code>.  To perform a
   *  solitary read from a <code>Ref</code> outside a transaction, use
   *  <code>nonTxn.get</code>.
   *  @param txn an active transaction.
   *  @return the value of the <code>Ref</code> as observed by
   *      <code>txn</code>.
   *  @throws IllegalStateException if <code>txn</code> is not active.
   *  @see edu.stanford.ppl.ccstm.Ref#bind
   *  @see edu.stanford.ppl.ccstm.Ref#nonTxn
   */
  def get(implicit txn: Txn): T

  /** Returns <code>f(get)</code>, possibly reevaluating <code>f</code> to
   *  avoid rollback if a conflicting change is made but both the old and new
   *  values are equal after application of <code>f</code>.  Requires that
   *  <code>f(x) == f(y)</code> if <code>x == y</code>.
   *  @param f an idempotent function.
   *  @return the result of applying <code>f</code> to the value read by this
   *      view.
   */
  def map[Z](f: T => Z)(implicit txn: Txn): Z

  /** The restriction of <code>Ref.bind</code> to <code>Source</code>.
   *  @see edu.stanford.ppl.ccstm.Ref#bind
   */
  def bind(implicit txn: Txn): Source.Bound[T]

  /** The restriction of <code>Ref.single</code> to <code>Source</code>.
   *  @see edu.stanford.ppl.ccstm.Ref#single
   */
  def single: Source.Bound[T]

  /** The restriction of <code>Ref.escaped</code> to <code>Source</code>.
   *  @see edu.stanford.ppl.ccstm.Ref#escaped
   */
  def escaped: Source.Bound[T]

  @deprecated("consider replacing with Source.single, otherwise use Source.escaped")
  def nonTxn: Source.Bound[T]

  // implicit access to unrecordedRead is omitted to discourage its use
}

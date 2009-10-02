/* CCSTM - (c) 2009 Stanford University - PPL */

// Source

package edu.stanford.ppl.ccstm


object Source {

  /** <code>Source.Bound</code> defines the covariant read-only view of a
   *  <code>Ref</code> bound to a particular context.  The context may be
   *  either a <code>Txn</code>, which guarantees that all reads will observe
   *  the same values as if they were executed at the transaction's commit
   *  (linearization) point, or the non-transactional context, which guarantees
   *  that each read will be linearized with all writes to the cell.
   */
  trait Bound[+T] {

    /** The restriction of <code>Ref.Bound.unbind</code> to a
     *  <code>Source</code>.
     *  @see edu.stanford.ppl.ccstm.Ref.Bound#unbind
     */
    def unbind: Source[T]

    /** Returns <code>Some(txn)</code> if this view is bound to a transaction
     *  <code>txn</code>, <code>None</code> if it is bound to the
     *  non-transactional context.
     */
    def context: Option[Txn]

    /** Performs a read in the current context.  Equivalent to <code>get</code>.
     *  @see edu.stanford.ppl.ccstm.Source.Bound#get
     */
    def unary_! : T = get

    /** Performs a transactional read of the value managed by the bound
     *  <code>Ref</code>.  If this view was created by <code>bind(txn)</code>,
     *  the returned value will take into account the writes already performed
     *  in this transaction, and the returned value will be consistent with all
     *  reads already performed by <code>txn</code>.  If this view was created
     *  by <code>nonTxn</code>, the read will be strongly atomic and isolated
     *  with respect to all transactions.  This means that if a non-txn read
     *  observes a value stored by transaction <i>A</i>, any subsequent non-txn
     *  read on the same thread is guaranteed to observe all changes committed
     *  by <i>A</i>.
     *  @return the current value of the bound <code>Ref</code> as observed by
     *      the binding context.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def get: T

    /** Returns <code>f(get)</code>, possibly reevaluating <code>f</code> to
     *  avoid rollback if a conflicting change is made but both the old and new
     *  values are equal after application of <code>f</code>.  Requires that
     *  <code>f(x) == f(x)</code>.
     *  @param f an idempotent function.
     *  @return the result of applying <code>f</code> to the value read by this
     *      view.
     */
    def map[Z](f: T => Z): Z

    /** Blocks until <code>pred(get)</code> is true, in a manner consistent
     *  with the current context.  If called from a transactional context, this
     *  method is equivalent to <code>if(!pred(get)) txn.retry</code>. If
     *  called from a non-transactional context, this method blocks until the
     *  predicate holds.  Requires that the predicate be safe to reevaluate,
     *  and that <code>pred(x) == pred(x)</code>.
     *  @param pred an idempotent predicate.
     *  @see edu.stanford.ppl.ccstm.Txn#retry
     */
    def await(pred: T => Boolean)

    /** Returns an <code>UnrecordedRead</code> instance that wraps the value
     *  that would be returned from <code>get</code>.  If this source is bound
     *  to a transactional context does not add the <code>Ref</code> to the
     *  transaction's read set.  The caller is responsible for guaranteeing
     *  that the transaction's behavior is correct even if the
     *  <code>Ref</code> is changed by an outside context before commit.  This
     *  method may be useful for heuristic decisions that can tolerate
     *  inconsistent or stale data, or for methods that register transaction
     *  handlers to perform validation at a semantic level.  When called from a
     *  non-transactional context the wrapping <code>UnrecordedRead</code>
     *  instance can be used to determine if any changes have been made to a
     *  <code>Ref</code>, which may be useful to avoid ABA problems.  Some STM
     *  implementations may spuriously indicate that a read is no longer valid.
     *  <p>
     *  When combining this method with transaction resource callbacks, it is
     *  important to consider the case that the unrecorded read is already
     *  invalid when it is returned from this method.
     *  <p>
     *  Although this method does not add to the read set, the returned value
     *  is consistent with the transaction's previous (recorded) reads.
     *  @return an <code>UnrecordedRead</code> instance that holds the read
     *      value and allows explicit revalidation.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def unrecordedRead: UnrecordedRead[T]
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
  def unary_!(implicit txn: Txn): T

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
   *  <code>f(x) == f(x)</code>.
   *  @param f an idempotent function.
   *  @return the result of applying <code>f</code> to the value read by this
   *      view.
   */
  def map[Z](f: T => Z)(implicit txn: Txn): Z

  /** The restriction of <code>Ref.bind</code> to <code>Source</code>.
   *  @see edu.stanford.ppl.ccstm.Ref#bind
   */
  def bind(implicit txn: Txn): Source.Bound[T]

  /** The restriction of <code>Ref.nonTxn</code> to <code>Source</code>.
   *  @see edu.stanford.ppl.ccstm.Ref#nonTxn
   */
  def nonTxn: Source.Bound[T]

  // implicit access to unrecordedRead is omitted to discourage its use
}

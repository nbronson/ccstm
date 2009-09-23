/* CCSTM - (c) 2009 Stanford University - PPL */

// Ref.scala

package edu.stanford.ppl.ccstm


/** An object that provides factory methods for <code>Ref</code> instances.
 *  @see edu.stanford.ppl.ccstm.Ref
 *
 *  @author Nathan Bronson
 */
object Ref {

  /** Returns a new <code>Ref</code> instance, initialized to the default value
   *  for objects of type <code>T</code>.
   */
  def apply[T](): Ref[T] = apply(null.asInstanceOf[T])

  /** Returns a new <code>Ref</code> instance with the specified initial
   *  value.
   */
  def apply[T](initialValue: T): Ref[T] = new collection.TRef(initialValue)

  /** Returns a new constant <code>Ref</code> instance with the specified
   *  value.  Reads of the reference will always return the same value, and any
   *  attempt to change the value will result in an exception.  This may be
   *  used as an optimization when it can be dynamically determined at
   *  construction time that a reference will never need to be changed.  The
   *  returned reference acts as if it has already been frozen with
   *  <code>freeze</code>.
   *  <p>
   *  When feasible, the <code>Ref.Source</code> returned by
   *  <code>source</code> will provide better type-checking for constant
   *  references.
   *  @see edu.stanford.ppl.ccstm.Ref#source
   *  @see edu.stanford.ppl.ccstm.Ref.Bound#freeze
   */
  def constant[T](value: T): Ref[T] = new collection.ConstantRef(value)
  
  /** Returns a new constant <code>Ref.Source</code> instance with the
   *  specified value.  Reads of the source will always return the same value.
   *  @see edu.stanford.ppl.ccstm.Ref#constant
   */
  def source[T](value: T): Ref.Source[T] = constant(value)


  /** <code>Source</code> defines the covariant read-only interface of a
   *  <code>Ref</code> instance.
   */
  trait Source[+T] {

    /** Performs a transactional read.  Equivalent to <code>get</code>.
     *  @see edu.stanford.ppl.ccstm.Ref.Source#get
     */
    def unary_!(implicit txn: Txn): T = get

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
    def get(implicit txn: Txn): T = bind.get

    /** Returns <code>f(get)</code>, possibly reevaluating <code>f</code> to
     *  avoid rollback if a conflicting change is made but both the old and new
     *  values are equal after application of <code>f</code>.  Requires that
     *  <code>f(x) == f(x)</code>.
     *  @param f an idempotent function.
     *  @return the result of applying <code>f</code> to the value read by this
     *      view.
     */
    def map[Z](f: T => Z)(implicit txn: Txn) = bind.map(f)

    /** The restriction of <code>Ref.bind</code> to <code>Ref.Source</code>.
     *  @see edu.stanford.ppl.ccstm.Ref#bind
     */
    def bind(implicit txn: Txn): BoundSource[T]

    /** The restriction of <code>Ref.nonTxn</code> to <code>Ref.Source</code>.
     *  @see edu.stanford.ppl.ccstm.Ref#nonTxn
     */
    def nonTxn: BoundSource[T]

    // implicit access to unrecordedRead is omitted to discourage its use
  }

  /** <code>Sink</code> defines the contravariant write-only interface of a
   *  <code>Ref</code> instance.
   */
  trait Sink[-T] {

    /** Performs a transactional write.  Equivalent to <code>set(v)</code>.
     *  @see edu.stanford.ppl.ccstm.Ref.Sink#set
     */
    def :=(v: T)(implicit txn: Txn) { set(v) }

    /** Performs a transactional write.  The new value will not be visible by
     *  any other transactions or any non-transactional accesses until (and
     *  unless) <code>txn</code> successfully commits.
     *  @param v a value to store in the <code>Ref</code>.
     *  @throws IllegalStateException if <code>txn</code> is not active.
     */
    def set(v: T)(implicit txn: Txn) { bind.set(v) }

    /** The restriction of <code>Ref.bind</code> to <code>Ref.Sink</code>.
     *  @see edu.stanford.ppl.ccstm.Ref#bind
     */
    def bind(implicit txn: Txn): BoundSink[T]

    /** The restriction of <code>Ref.nonTxn</code> to <code>Ref.Sink</code>.
     *  @see edu.stanford.ppl.ccstm.Ref#nonTxn
     */
    def nonTxn: BoundSink[T]
  }

  /** <code>Ref.BoundSource</code> defines the covariant read-only view of a
   *  <code>Ref</code> bound to a particular context.  The context may be
   *  either a <code>Txn</code>, which guarantees that all reads will observe
   *  the same values as if they were executed at the transaction's commit
   *  (linearization) point, or the non-transactional context, which guarantees
   *  that each read will be linearized with all writes to the cell.
   */
  trait BoundSource[+T] {
    
    /** The restriction of <code>Ref.Bound.unbind</code> to
     *  <code>Ref.BoundSource</code>.
     *  @see edu.stanford.ppl.ccstm.Bound#unbind
     */
    def unbind: Ref.Source[T]

    /** Returns <code>Some(txn)</code> if this view is bound to a transaction
     *  <code>txn</code>, <code>None</code> if it is bound to the
     *  non-transactional context.
     */
    def context: Option[Txn]

    /** Performs a read in the current context.  Equivalent to <code>get</code>.
     *  @see edu.stanford.ppl.ccstm.Ref.BoundSource#get
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

  /** <code>BoundSink</code> defines the contravariant write-only view of a
   *  <code>Ref</code> bound to a particular context. The context may be
   *  either a <code>Txn</code>, which guarantees that all writes will appear
   *  to other contexts as if they were executed atomically at the
   *  transaction's commit (linearization) point, or the non-transactional
   *  context, which guarantees that each write will be linearized (and a
   *  happens-before relationship established) with all other reads and writes
   *  to an equal reference.
   */
  trait BoundSink[-T] {

    /** The restriction of <code>Ref.Bound.unbind</code> to
     *  <code>Ref.BoundSink</code>.
     *  @see edu.stanford.ppl.ccstm.Bound#unbind
     */
    def unbind: Ref.Sink[T]

    /** Returns <code>Some(txn)</code> if this view is bound to a transaction
     *  <code>txn</code>, <code>None</code> if it is bound to the
     *  non-transactional context.
     */
    def context: Option[Txn]

    /** Writes to the bound <code>Ref</code>, equivalent to <code>set</code>.
     *  @see edu.stanford.ppl.ccstm.Ref.BoundSink#set
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

    /** Prohibits future changes to the value referenced by the bound
     *  <code>Ref</code>.  If this method is called from a transactional
     *  context the prohibition will be removed if the transaction rolls back.
     *  Future calls to <code>set</code> will cause an
     *  <code>IllegalStateException</code> to be thrown.  Future calls to a
     *  conditional update function such as <code>compareAndSet</code> or
     *  <code>transformIfDefined</code> will be allowed only if the condition
     *  for update does not hold (they must return false).  Future calls to
     *  <code>readForWrite</code> are allowed.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def freeze()
  }

  /** A <code>Ref</code> view that supports reads and writes.  Reads and writes
   *  are performed from the perspective of the bound context, which is either
   *  a <code>Txn</code> or the non-transactional context.  Reading operations
   *  are defined in <code>BoundSource</code>, writing operations defined in
   *  <code>BoundSink</code>, and operations that both read and write are
   *  defined in this trait.
   */
  trait Bound[T] extends BoundSource[T] with BoundSink[T] {

    /** Provides access to a <code>Ref</code> that refers to the same value as
     *  the one that was bound to produce this <code>Ref.Bound</code> instance.
     *  The returned <code>Ref</code> might be a new instance, but it is always
     *  true that <code>ref.bind.unbind == ref</code>.
     *  @return a <code>Ref</code> instance equal to (or the same as) the one
     *      that was bound to create this view instance.
     */
    def unbind: Ref[T]

    /** Returns the same value as that returned by <code>get</code>, but adds
     *  the <code>Ref</code> to the write set of the bound transaction context,
     *  if any.  Equivalent to <code>get</code> when called from a
     *  non-transactional context.
     *  @return the current value of the bound <code>Ref</code>, as observed by
     *      the binding context.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def readForWrite: T

    /** Equivalent to atomically executing
     *  <code>(if (before == get) { set(after); true } else false)</code>, but
     *  may be more efficient, especially in a non-transactional context.
     *  @param before a value to compare against the current <code>Ref</code>
     *      contents.
     *  @param after a value to store in the <code>Ref</code> if
     *      <code>before</code> was equal to the previous cell contents.
     *  @return <code>true</code> if <code>before</code> was equal to the
     *      previous value of the bound <code>Ref</code>, <code>false</code>
     *      otherwise.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def compareAndSet(before: T, after: T): Boolean

    /** Equivalent to atomically executing
     *  <code>(if (before eq get) { set(after); true } else false)</code>, but
     *  may be more efficient, especially in a non-transactional context.
     *  @param before a reference whose identity will be compared against the
     *      current <code>Ref</code> contents.
     *  @param after a value to store in the <code>Ref</code> if
     *      <code>before</code> has the same reference identity as the previous
     *      cell contents.
     *  @return <code>true</code> if <code>before</code> has the same reference
     *      identity as the previous value of the bound <code>Ref</code>,
     *      <code>false</code> otherwise.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     *  @see edu.stanford.ppl.ccstm.Ref.Bound#compareAndSet
     */
    def compareAndSetIdentity[A <: T with AnyRef](before: A, after: T): Boolean

    /** Works like <code>compareAndSet</code>, but allows spurious failures.  A
     *  false return from this method does not necessarily mean that the
     *  previous value of the <code>Ref</code> is not equal to
     *  <code>before</code>.
     *  @see edu.stanford.ppl.ccstm.Ref.Bound#compareAndSet
     */
    def weakCompareAndSet(before: T, after: T): Boolean

    /** Works like <code>compareAndSetIdentity</code>, but allows spurious
     *  failures.  A false return from this method does not necessarily mean
     *  that the previous value of the <code>Ref</code> has a different
     *  reference identity than <code>before</code>.
     *  @see edu.stanford.ppl.ccstm.Ref.Bound#compareAndSetIdentity
     */
    def weakCompareAndSetIdentity[A <: T with AnyRef](before: A, after: T): Boolean

    /** Atomically replaces the value <i>v</i> stored in the <code>Ref</code>
     *  with <code>f</code>(<i>v</i>), possibly deferring execution of
     *  <code>f</code> or calling <code>f</code> multiple times.
     *  @param f a function that is safe to call multiple times, and safe to
     *      call later during the bound transaction (if any).
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def transform(f: T => T)

    /** Atomically replaces the value <i>v</i> stored in the <code>Ref</code>
     *  with <code>pf</code>(<i>v</i>) if <code>pf.isDefinedAt</code>(<i>v</i>),
     *  returning true, otherwise leaves the element unchanged and returns
     *  false.  <code>pf.apply</code> and <code>pf.isDefinedAt</code> may be
     *  called multiple times in both transactional and non-transactional
     *  contexts.  If the current context is transactional, <code>pf</code>'s
     *  methods may be deferred until later in the transaction to reduce
     *  transaction conflicts.
     *  @param pf a partial function that is safe to call multiple times, and
     *      safe to call later in the bound transaction (if any).
     *  @return <code>pf.isDefinedAt(<i>v</i>)</code>, where <i>v</i> is the
     *      element held by this <code>Ref</code> on entry.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def transformIfDefined(pf: PartialFunction[T,T]): Boolean

    override def equals(rhs: Any) = rhs match {
      case b: Bound[_] => (context == b.context) && (unbind == b.unbind)
      case _ => false
    }

    override def hashCode: Int = (context.hashCode * 137) ^ unbind.hashCode ^ 101
  }
}

/** Provides access to a single element of type <i>T</i>.  Accesses may be
 *  performed as part of a <i>memory transaction</i>, composing an atomic
 *  block, or they may be performed individually in a non-transactional manner.
 *  The software transactional memory performs concurrency control to make sure
 *  that all committed transactions and all non-transactional accesses are
 *  linearizable.  Reads and writes performed by a successful <code>Txn</code>
 *  return the same values as if they were executed atomically at the
 *  transaction's commit (linearization) point.  Reads and writes performed in
 *  a non-transactional manner have sequential consistency with regard to all
 *  accesses (transactional and non-transactional), for values managed by the
 *  STM.
 *  <p>
 *  In a transactional context (one in which an implicit <code>Txn</code> is
 *  available), reads and writes are performed with either <code>get</code> and
 *  <code>set</code>, or with the ML-inspired <code>unary_!</code> and
 *  <code>:=</code>.  A <code>Txn</code> may be bound with a <code>Ref</code>,
 *  the resulting <code>Ref.Bound</code> instance does not need access to the
 *  implicit transaction, so it may be more convenient when passing refs to a
 *  method.  The bound instance is only valid for the duration of the
 *  transaction.
 *  <p>
 *  It is possible for separate <code>Ref</code> instances to refer to the same
 *  element; in this case they will compare equal.  (As an example, a
 *  transactional array class might store elements in an array and create
 *  <code>Ref</code>s on demand.)  <code>Ref</code>s (or
 *  <code>Ref.Source</code>s) may be provided for computed values, such as the
 *  emptiness of a queue, to allow conditional retry and waiting on semantic
 *  properties.
 *  <p>
 *  Non-transactional access is obtained via the view returned from
 *  <code>nonTxn</code>.  Each non-transactional access will be linearized with
 *  all transactions that access <code>Ref</code>s equal to this one, and with
 *  all non-transactional accesses to <code>Ref</code>s equal to this one.
 *  <p>
 *  Concrete <code>Ref</code> instances may be obtained from the factory
 *  methods in the <code>Ref</code> object.
 *  @see edu.stanford.ppl.ccstm.STM
 *
 *  @author Nathan Bronson
 */
trait Ref[T] extends Ref.Source[T] with Ref.Sink[T] {

  /** Transforms the value referenced by this <code>Ref</code> by applying the
   *  function <code>f</code>.  Acts like <code>ref.set(f(ref.get))</code>, but
   *  the execution of <code>f</code> may be deferred or duplicated to reduce
   *  transaction conflicts.
   *  @param f a function that is safe to call multiple times, and safe to
   *      call later during the transaction.
   *  @throws IllegalStateException if <code>txn</code> is not active.
   */
  def transform(f: T => T)(implicit txn: Txn) { bind(txn).transform(f) }

  /** Transforms the value referenced by this <code>Ref</code> by applying the
   *  <code>pf.apply</code>, but only if <code>pf.isDefinedAt</code> holds for
   *  the current value.  Returns true if a transformation was performed, false
   *  otherwise.  <code>pf.apply</code> and <code>pf.isDefinedAt</code> may be
   *  called multiple times, and may be deferred until later in the
   *  transaction.
   *  @param pf a partial function that is safe to call multiple times, and
   *      safe to call later in the transaction.
   *  @return <code>pf.isDefinedAt(<i>v</i>)</code>, where <i>v</i> is the
   *      current value of this <code>Ref</code> on entry.
   *  @throws IllegalStateException if <code>txn</code> is not active.
   */
  def transformIfDefined(pf: PartialFunction[T,T])(implicit txn: Txn) = bind(txn).transformIfDefined(pf)

  /** Returns a reference view that does not require an implicit
   *  <code>Txn</code> parameter on each method call, but instead always
   *  performs accesses in the context of <code>txn</code>.   A transaction may
   *  be bound regardless of its state, but reads and writes are only allowed
   *  while a transaction is active.  The view returned from this method may be
   *  convenient when passing <code>Ref</code>s to scopes that would not
   *  otherwise have implicit access to <code>txn</code>, and the view provides
   *  some extra functionality that is less frequently needed.
   *  @param txn a transaction to be bound to the view.
   *  @return a view of this instance that performs all accesses as if from
   *      <code>txn</code>.
   */
  def bind(implicit txn: Txn): Ref.Bound[T]

  /** Returns a view that can be used to perform individual reads and writes to
   *  this reference outside any transactional context.  Each operation acts as
   *  if it was performed in its own transaction.  The returned instance is
   *  valid for the lifetime of the program.
   *  @return a view into the value of a <code>Ref</code>, that will perform
   *      each operation as if in its own transaction.
   */
  def nonTxn: Ref.Bound[T]

  /** Returns this instance, but with only the read-only portion accessible.
   *  Equivalent to <code>asInstanceOf[Ref.Source[T]]</code>, but may be more
   *  readable.
   *  @return this instance, but with only read-only methods accessible
   */
  def source: Ref.Source[T] = this
}

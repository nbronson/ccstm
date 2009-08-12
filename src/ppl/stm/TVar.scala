/* TVar
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

package ppl.stm

/** See {@link ppl.stm.TVar}. */
object TVar {
  
  /** Returns a new {@link ppl.stm.TVar} instance with the specified initial
   *  value.
   */
  def apply[T](initialValue: T) = new TVar(initialValue)

  
  /** {@code Source} defines the covariant read-only interface of a
   *  {@link ppl.stm.TVar}.
   */
  trait Source[+T] {
    /** Equivalent to {@link ppl.stm.TVar.Source#bind bind(txn)}
     *  {@link ppl.stm.TVar.BoundSource#elem .elem}.
     */
    def elem(implicit txn: Txn): T = bind(txn).elem

    /** Equivalent to {@link ppl.stm.TVar.Source#bind bind(txn)}
     *  {@link ppl.stm.TVar.BoundSource#elemMap .elemMap}.
     */
    def elemMap[Z](f: T => Z)(implicit txn: Txn) = bind(txn).elemMap(f)

    /** Returns a view on this {@code TVar.Source} that can be used to read
     *  the cell's value as part of a transaction {@code txn}.  A transaction
     *  may be bound regardless of its state, but reads (and writes) are only
     *  allowed for active transactions.
     *  @param txn a transaction to be bound to the view.
     *  @returns a read-only view onto the value of a {@code TVar} under the
     *      context of {@code txn}.
     */
    def bind(implicit txn: Txn): BoundSource[T]

    /** Returns a view of this {@code TVar.Source} that can be used to perform
     *  individual (non-transactional) reads of the cell's value.  The returned
     *  view acts as if each operation is performed in its own transaction.
     *  The returned view is valid for the lifetime of the program.
     *  @returns a read-only view into the value of a {@code TVar} that will
     *      perform each read operation as if in its own transaction.
     */
    def nonTxn: BoundSource[T]
  }

  /** {@code Sink} defines the contravariant write-only interface of a
   *  {@link ppl.stm.TVar}.
   */
  trait Sink[-T] {
    /** Equivalent to {@link ppl.stm.TVar.Sink#bind bind(txn)}
     *  {@link ppl.stm.TVar.BoundSink#elem_= .elem_=}.
     */
    def elem_=(v: T)(implicit txn: Txn) { bind(txn).elem_=(v) }

    /** Returns a view on this {@code TVar.Source} that can be used to write
     *  the cell's value as part of a transaction {@code txn}.  A transaction
     *  may be bound regardless of its state, but writes (and reads) are only
     *  allowed for active transactions.
     *  @param txn a transaction to be bound to the view.
     *  @returns a write-only view onto the value of a {@code TVar} under the
     *      context of {@code txn}.
     */
    def bind(implicit txn: Txn): BoundSink[T]

    /** Returns a view of this {@code TVar.Sink} that can be used to perform
     *  individual (non-transactional) writes of the cell's value.  The
     *  returned view acts as if each operation is performed in its own
     *  transaction. The returned view is valid for the lifetime of the
     *  program.
     *  @returns a write-only view into the value of a {@code TVar}, that will
     *      perform each update operation as if in its own transaction.
     */
    def nonTxn: BoundSink[T]
  }

  /** {@code BoundSource} defines the covariant read-only view of a
   *  {@code TVar} bound to a particular context.  The context may be either
   *  a {@link ppl.stm.Txn Txn}, which guarantees that all reads will observe
   *  the same values as if they were executed at the transaction's commit
   *  (linearization) point, or the non-transactional context, which guarantees
   *  that each read will be linearized with all writes to the cell.
   */
  trait BoundSource[+T] {
    /** Returns {@code Some(txn)} if this view is bound to a transaction {@code
     *  txn}, {@code None} if it is bound to the non-transactional context.
     */
    def context: Option[Txn]

    /** Reads the element held by the bound {@code TVar}.  If this view was
     *  created by {@link ppl.stm.TVar#bind TVar.bind(txn)} (or
     *  {@link ppl.stm.TVar.Source#bind TVar.Source.bind(txn)}) then this
     *  method will include the results of previous modifications of the
     *  {@code TVar} in {@code txn}, and will validate {@code txn} prior to
     *  returning.  If this view was created by {@link ppl.stm.TVar#nonTxn
     *  TVar.nonTxn} (or {@link ppl.stm.TVar.Source#nonTxn
     *  TVar.Source.nonTxn}) the read will be strongly atomic and isolated
     *  with respect to all transactions.  This means that if a non-txn read
     *  observes a value stored by transaction <i>A</i>, any subsequent
     *  non-txn read on the same thread is guaranteed to observe all changes
     *  committed by <i>A</i>.
     *  @returns the current value of the bound {@code TVar} as observed by
     *      the binding context.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def elem: T

    /** Equivalent to {@code f}({@link ppl.stm.TVar.BoundSource#elem elem}),
     *  except that transaction rollback may be avoided on concurrent update to
     *  the {@code TVar} if there is no change to {@code f(elem)}.  Requires
     *  that <code>f(x) == f(x)</code>.
     *  @param f an idempotent function.
     *  @returns the result of applying {@code f} to the value read by this
     *      view.
     */
    def elemMap[Z](f: T => Z): Z
  }

  /** {@code BoundSink} defines the contravariant write-only view of a
   *  {@link ppl.stm.TVar} bound to a particular context. The context may be
   *  either a {@link ppl.stm.Txn Txn}, which guarantees that all writes will
   *  appear to other contexts as if they were executed atomically at the
   *  transaction's commit (linearization) point, or the non-transactional
   *  context, which guarantees that each write will be linearized (and a
   *  happens-before relationship established) with all other reads and writes
   *  to the cell.
   */
  trait BoundSink[-T] {
    /** Returns {@code Some(txn)} if this view is bound to a transaction {@code
     *  txn}, {@code None} if it is bound to the non-transactional context.
     */
    def context: Option[Txn]

    /** Updates the element held by the {@code TVar}.  If this view was
     *  created by {@link ppl.stm.TVar#bind TVar.bind(txn)} (or
     *  {@link ppl.stm.TVar.Sink#bind TVar.Sink.bind(txn)}) then the new
     *  value will not be visible to other contexts until (and unless)
     *  {@code txn} successfully commits.  If this view was created by
     *  {@link ppl.stm.TVar#nonTxn TVar.nonTxn} (or
     *  {@link ppl.stm.TVar.Sink#nonTxn TVar.Sink#nonTxn}) the value will be
     *  made available immediately, and a happens-before relationship will be
     *  established between this thread and any thread that reads {@code v}
     *  from this {@code TVar}.
     *  @param v a value to store in the {@code TVar}.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def elem_=(v: T)
  }

  /** A {@code TVar} view that supports reads and writes.  Reads and writes
   *  are performed from the perspective of the bound context, which is either
   *  a {@link ppl.stm.Txn Txn} or the non-transactional context.
   */
  trait Bound[T] extends BoundSource[T] with BoundSink[T] {
    /** Equivalent to <pre>
     *    (if (elem == before) { elem = after; true } else false)
     *  </pre>but may be more efficient.
     *  @param before a value to compare against the current {@code TVar}
     *      contents.
     *  @param after a value to store in the {@code TVar} if {@code before}
     *      was equal to the previous cell contents.
     *  @returns {@code true} if {@code after} was written into the cell,
     *      {@code false} otherwise.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def compareAndSet(before: T, after: T): Boolean

    /** Atomically replaces the value <i>v</i> stored in the {@code TVar} with
     *  {@code f}(<i>v</i>), possibly deferring execution of {@code f} or
     *  calling {@code f} multiple times to reduce transaction conflicts.
     *  @param f a function that may be called multiple times during the bound
     *      transaction (if any).
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def transform(f: T => T)

    /** Atomically replaces the value <i>v</i> stored in the {@code TVar} with
     *  {@code f}(<i>v</i>) if {@code cond}(<i>v</i>) is true, otherwise leaves
     *  the element unchanged.  Returns true if the condition was true, false
     *  otherwise.  {@code cond} and {@code f} may be called multiple times and
     *  may be called after this method has returned.
     *  @param cond a precondition that may be tested multiple times during the
     *      bound transaction (if any).
     *  @param f a function that may be called multiple times during the bound
     *      transaction (if any).
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def testAndTransform(cond: T => Boolean, f: T => T): Boolean
  }
}

private trait TVarAccessor[T] {
  val instance: TVar[T]
  def field = 0
  def metadata = instance._metadata
  def metadata_=(v: STM.Metadata) { instance._metadata = v }
  def data = instance._data
  def data_=(v: Any) { instance._data = v }
}

private class TVarNonTxnAccessor[T](val instance: TVar[T]) extends STM.NonTxnAccessor[T] with TVarAccessor[T]
private class TVarTxnAccessor[T](val txn: Txn, val instance: TVar[T]) extends STM.TxnAccessor[T] with TVarAccessor[T]

/** Holds a single element of type <i>T</i>, providing optimistic concurrency
 *  control using software transactional memory.  Reads and writes performed
 *  by a successful {@link ppl.stm.Txn Txn} return the same values as if they
 *  were executed atomically at the transaction's commit (linearization) point.
 *  Reads and writes performed in a non-transactional manner have sequential
 *  consistency with regard to all accesses (transactional and
 *  non-transactional) for STM data types.
 *  <p>
 *  Loads and stores can be performed by invoking {@link ppl.stm.TVar#elem(Txn)
 *  elem} and {@link ppl.stm.TVar#elem_= elem_=} with an implicit {@code Txn}
 *  available (cleanest syntax), by invoking those methods with an explicit
 *  {@code Txn} instance, or via a {@link ppl.stm.TVar.Bound TVar.Bound} view
 *  created by {@link ppl.stm.TVar#bind bind} (fastest if multiple accesses to
 *  this {@code TVar} are performed).
 *  <p>
 *  {@code TVar} allows subclassing to allow it to be opportunistically
 *  embedded in the internal node objects of collection classes, to reduce
 *  storage and indirection overheads.  If storage efficiency is a primary
 *  concern {@link ppl.stm.TFieldUpdater} may also be useful.
 */
class TVar[T](initialValue: T) extends STM.MetadataHolder with TVar.Source[T] with TVar.Sink[T] {

  @volatile private[stm] var _data: Any = initialValue

  /** Equivalent to {@link ppl.stm.TVar#bind bind(txn)}
   *  {@link ppl.stm.TVar.Bound#compareAndSet .compareAndSet(before,after)}.
   */
  def compareAndSet(before: T, after: T)(implicit txn: Txn) = bind(txn).compareAndSet(before, after)

  /** Equivalent to {@link ppl.stm.TVar#bind bind(txn)}
   *  {@link ppl.stm.TVar.Bound#transform .transform(f)}.
   */
  def transform(f: T => T)(implicit txn: Txn) { bind(txn).transform(f) }

  /** Equivalent to {@link ppl.stm.TVar#bind bind(txn)}
   *  {@link ppl.stm.TVar.Bound#testAndTransform .testAndTransform(f)}.
   */
  def testAndTransform(cond: T => Boolean, f: T => T)(implicit txn: Txn) = bind(txn).testAndTransform(cond, f)

  /** Returns a view on this {@code TVar.Source} that can be used to read or
   *  write the cell's value as part of the transaction {@code txn}.  A
   *  transaction may be bound regardless of its state, but reads and writes
   *  are only allowed for active transactions.
   *  @param txn the transaction to be bound.
   *  @returns a view onto the value of a {@code TVar} under the context of
   *      {@code txn}.
   */
  def bind(implicit txn: Txn): TVar.Bound[T] = new TVarTxnAccessor(txn, this)

  /** Returns a view of this {@code TVar.Sink} that can be used to perform
   *  individual (non-transactional) reads and writes of the cell's value. The
   *  returned view acts as if each operation is performed in its own
   *  transaction. The returned instance is valid for the lifetime of the
   *  program.
   *  @returns a view into the value of a {@code TVar}, that will perform each
   *      operation as if in its own transaction.
   */
  def nonTxn: TVar.Bound[T] = new TVarNonTxnAccessor(this)
}

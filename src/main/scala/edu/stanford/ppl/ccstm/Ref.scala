/* CCSTM - (c) 2009 Stanford University - PPL */

// Ref.scala

package edu.stanford.ppl.ccstm


/** An object that provides factory methods for <code>Ref</code> instances.
 *  @see edu.stanford.ppl.ccstm.Ref
 *
 *  @author Nathan Bronson
 */
object Ref {
  import collection._

  /** Returns a new <code>Ref</code> instance, initialized to the default value
   *  for objects of type <code>T</code>.
   */
  def apply[T](): Ref[T] = apply(null.asInstanceOf[T])

  /** Returns a new <code>Ref</code> instance with the specified initial
   *  value.
   */
  def apply[T](initialValue: T): Ref[T] = new TAnyRef(initialValue)

  def apply(initialValue: Int): Ref[Int] = new TIntRef(initialValue)
  def apply(initialValue: Long): Ref[Long] = new TLongRef(initialValue)
  def apply(initialValue: Float): Ref[Float] = new TFloatRef(initialValue)
  def apply(initialValue: Double): Ref[Double] = new TDoubleRef(initialValue)
  def apply(initialValue: Boolean): Ref[Boolean] = new TBooleanRef(initialValue)

  /** Returns a new constant <code>Ref</code> instance with the specified
   *  value.  Reads of the reference will always return the same value, and any
   *  attempt to change the value will result in an exception.  This may be
   *  used as an optimization when it can be dynamically determined at
   *  construction time that a reference will never need to be changed.  The
   *  returned reference acts as if it has already been frozen with
   *  <code>freeze</code>.
   *  <p>
   *  When feasible, the <code>source</code> may be preferred, as it provides
   *  better compile-time checking.
   *  @see edu.stanford.ppl.ccstm.Ref#source
   *  @see edu.stanford.ppl.ccstm.Ref.Bound#freeze
   */
  def constant[T](value: T): Ref[T] = new collection.ConstantRef(value)
  
  /** Returns a new constant <code>Source</code> instance with the
   *  specified value.  Reads of the source will always return the same value.
   *  @see edu.stanford.ppl.ccstm.Ref#constant
   */
  def source[T](value: T): Source[T] = constant(value)


  /** A <code>Ref</code> view that supports reads and writes.  Reads and writes
   *  are performed from the perspective of the bound context, which is either
   *  a <code>Txn</code> or the non-transactional context.  Reading operations
   *  are defined in <code>BoundSource</code>, writing operations defined in
   *  <code>BoundSink</code>, and operations that both read and write are
   *  defined in this trait.
   */
  trait Bound[T] extends Source.Bound[T] with Sink.Bound[T] {

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
 *  <code>Ref</code>s on demand.)  <code>Ref</code>s (or <code>Source</code>s)
 *  may be provided for computed values, such as the emptiness of a queue, to
 *  allow conditional retry and waiting on semantic properties.
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
trait Ref[T] extends Source[T] with Sink[T] {

  /** Provides access to the data and metadata associated with this reference.
   *  This is the only method for which the default <code>Ref</code>
   *  implementation is not sufficient.
   */
  protected def handle: impl.Handle[T]

  //////////////// Source stuff

  def unary_!(implicit txn: Txn): T = txn.get(handle)
  def get(implicit txn: Txn): T = txn.get(handle)
  def map[Z](f: (T) => Z)(implicit txn: Txn): Z = txn.map(handle, f)

  //////////////// Sink stuff

  def :=(v: T)(implicit txn: Txn) { txn.set(handle, v) }
  def set(v: T)(implicit txn: Txn) { txn.set(handle, v) }

  //////////////// Ref functions

  /** Transforms the value referenced by this <code>Ref</code> by applying the
   *  function <code>f</code>.  Acts like <code>ref.set(f(ref.get))</code>, but
   *  the execution of <code>f</code> may be deferred or duplicated to reduce
   *  transaction conflicts.
   *  @param f a function that is safe to call multiple times, and safe to
   *      call later during the transaction.
   *  @throws IllegalStateException if <code>txn</code> is not active.
   */
  def transform(f: T => T)(implicit txn: Txn) { txn.transform(handle, f) }

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
  def transformIfDefined(pf: PartialFunction[T,T])(implicit txn: Txn) = txn.transformIfDefined(handle, pf)

  /** Returns this instance, but with only the read-only portion accessible.
   *  Equivalent to <code>asInstanceOf[Source[T]]</code>, but may be more
   *  readable.
   *  @return this instance, but with only read-only methods accessible
   */
  def source: Source[T] = this

  //////////////// Binding

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
  def bind(implicit txn: Txn): Ref.Bound[T] = new Ref.Bound[T] {
    def unbind: Ref[T] = Ref.this
    def context: Option[Txn] = Some(txn)

    def get: T = txn.get(handle)
    def map[Z](f: (T) => Z): Z = txn.map(handle, f)
    def await(pred: (T) => Boolean) { if (!pred(get)) txn.retry } 
    def unrecordedRead: UnrecordedRead[T] = txn.unrecordedRead(handle)
    def releasableRead: ReleasableRead[T] = txn.releasableRead(handle)

    def set(v: T) { txn.set(handle, v) }
    def tryWrite(v: T): Boolean = txn.tryWrite(handle, v)
    def freeze() = txn.freeze(handle)

    def readForWrite: T = txn.readForWrite(handle)
    def compareAndSet(before: T, after: T): Boolean = {
      txn.compareAndSet(handle, before, after)
    }
    def compareAndSetIdentity[A <: T with AnyRef](before: A, after: T): Boolean = {
      txn.compareAndSetIdentity(handle, before, after)
    }
    def weakCompareAndSet(before: T, after: T): Boolean = {
      txn.weakCompareAndSet(handle, before, after)
    }
    def weakCompareAndSetIdentity[A <: T with AnyRef](before: A, after: T): Boolean = {
      txn.weakCompareAndSetIdentity(handle, before, after)
    }
    def transform(f: T => T) {
      txn.transform(handle, f)
    }
    def transformIfDefined(pf: PartialFunction[T,T]): Boolean = {
      txn.transformIfDefined(handle, pf)
    }
  }

  /** Returns a view that can be used to perform individual reads and writes to
   *  this reference outside any transactional context.  Each operation acts as
   *  if it was performed in its own transaction.  The returned instance is
   *  valid for the lifetime of the program.
   *  @return a view into the value of this <code>Ref</code>, that will perform
   *      each operation as if in its own transaction.
   */
  def nonTxn: Ref.Bound[T] = new Ref.Bound[T] {
    def unbind: Ref[T] = Ref.this
    def context: Option[Txn] = None

    def get: T = impl.NonTxn.get(handle)
    def map[Z](f: (T) => Z): Z = f(impl.NonTxn.get(handle))
    def await(pred: (T) => Boolean) { impl.NonTxn.await(handle, pred) }
    def unrecordedRead: UnrecordedRead[T] = impl.NonTxn.unrecordedRead(handle)
    def releasableRead: ReleasableRead[T] = new ReleasableRead[T] {
      def context: Option[Txn] = None
      val value: T = get
      def release() {}
    }

    def set(v: T) { impl.NonTxn.set(handle, v) }
    def tryWrite(v: T): Boolean = impl.NonTxn.tryWrite(handle, v)
    def freeze() = impl.NonTxn.freeze(handle)

    def readForWrite: T = impl.NonTxn.get(handle)
    def compareAndSet(before: T, after: T): Boolean = {
      impl.NonTxn.compareAndSet(handle, before, after)
    }
    def compareAndSetIdentity[A <: T with AnyRef](before: A, after: T): Boolean = {
      impl.NonTxn.compareAndSetIdentity(handle, before, after)
    }
    def weakCompareAndSet(before: T, after: T): Boolean = {
      impl.NonTxn.weakCompareAndSet(handle, before, after)
    }
    def weakCompareAndSetIdentity[A <: T with AnyRef](before: A, after: T): Boolean = {
      impl.NonTxn.weakCompareAndSetIdentity(handle, before, after)
    }
    def transform(f: T => T) {
      impl.NonTxn.transform(handle, f)
    }
    def transformIfDefined(pf: PartialFunction[T,T]): Boolean = {
      impl.NonTxn.transformIfDefined(handle, pf)
    }
  }

  override def hashCode = impl.STMImpl.hash(handle.ref, handle.offset)

  override def equals(rhs: Any) = {
    rhs match {
      case r: Ref[_] => {
        val h1 = handle
        val h2 = r.handle
        (h1.ref eq h2.ref) && (h1.offset == h2.offset)
      }
      case _ => false
    }
  }
}

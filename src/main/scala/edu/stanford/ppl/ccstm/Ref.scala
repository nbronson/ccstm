/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// Ref.scala

package edu.stanford.ppl.ccstm

import reflect.AnyValManifest

/** An object that provides factory methods for `Ref` instances.
 *
 *  @author Nathan Bronson
 */
object Ref {
  import collection._

  /** Returns a new <code>Ref</code> instance suitable for holding instances of
   *  <code>T</code>.
   *
   *  If you have an initial value `v0` available, prefer `apply(v0)`.
   */
  def make[T]()(implicit m: ClassManifest[T]): Ref[T] = {
    (m.newArray(0).asInstanceOf[AnyRef] match {
      case x: Array[Boolean] => new TBooleanRef(false)
      case x: Array[Byte]    => new TByteRef(   0 : Byte)
      case x: Array[Short]   => new TShortRef(  0 : Short)
      case x: Array[Char]    => new TCharRef(   0 : Char)
      case x: Array[Int]     => new TIntRef(    0 : Int)
      case x: Array[Float]   => new TFloatRef(  0 : Float)
      case x: Array[Long]    => new TLongRef(   0 : Long)
      case x: Array[Double]  => new TDoubleRef( 0 : Double)
      case x: Array[Unit]    => new TAnyRef[Unit](())
      case x: Array[AnyRef]  => new TAnyRef[AnyRef](null)
    }).asInstanceOf[Ref[T]]
  }

  /** Returns a new `Ref` instance with the specified initial value.  The
   *  returned instance is not part of any transaction's read or write set.
   */
  def apply[T](initialValue: T)(implicit m: ClassManifest[T]): Ref[T] = {
    // TODO: this is likely to be a hot spot, perhaps RFE a method in Manifest for this test?
    if (m.isInstanceOf[AnyValManifest[_]]) {
      newPrimitiveRef(initialValue)
    } else {
      new TAnyRef(initialValue)
    }
  }

  def apply(initialValue: Boolean): Ref[Boolean] = new TBooleanRef(initialValue)
  def apply(initialValue: Byte   ): Ref[Byte]    = new TByteRef(   initialValue)
  def apply(initialValue: Short  ): Ref[Short]   = new TShortRef(  initialValue)
  def apply(initialValue: Char   ): Ref[Char]    = new TCharRef(   initialValue)
  def apply(initialValue: Int    ): IntRef       = new TIntRef(    initialValue)
  def apply(initialValue: Long   ): Ref[Long]    = new TLongRef(   initialValue)
  def apply(initialValue: Float  ): Ref[Float]   = new TFloatRef(  initialValue)
  def apply(initialValue: Double ): Ref[Double]  = new TDoubleRef( initialValue)
  def apply(initialValue: Unit   ): Ref[Unit]    = new TAnyRef(    initialValue)

  private def newPrimitiveRef[T](initialValue: T)(implicit m: ClassManifest[T]): Ref[T] = {
    (m.newArray(0).asInstanceOf[AnyRef] match {
      case x: Array[Boolean] => apply(initialValue.asInstanceOf[Boolean])
      case x: Array[Byte]    => apply(initialValue.asInstanceOf[Byte])
      case x: Array[Short]   => apply(initialValue.asInstanceOf[Short])
      case x: Array[Char]    => apply(initialValue.asInstanceOf[Char])
      case x: Array[Int]     => apply(initialValue.asInstanceOf[Int])
      case x: Array[Float]   => apply(initialValue.asInstanceOf[Float])
      case x: Array[Long]    => apply(initialValue.asInstanceOf[Long])
      case x: Array[Double]  => apply(initialValue.asInstanceOf[Double])
      case x: Array[Unit]    => apply(initialValue.asInstanceOf[Unit])
    }).asInstanceOf[Ref[T]]
  }


  /** A `Ref` view that supports reads and writes.  Reads and writes are
   *  performed according to the binding mode, which is a `Txn` instance if
   *  this view is bound to a transaction; `Single` if each method call on
   *  this instance will occur as if in its own atomic block, with dynamic
   *  resolution of nesting; or `Escaped` if each method call will occur as if
   *  in its own top-level transaction, regardless of whether a transaction is
   *  active on the current thread.  The granularity of atomicity for `Single`
   *  and `Escaped` views is a single `Ref` method, even if that method
   *  performs a read and a write (such as `transform`).  All instances with 
   *  the same target `Ref` and the same mode are considered equal and
   *  equivalent.
   *
   *  Some terms:
   *
   *  * Bound context: If `mode` is a `Txn`, that `Txn` is the bound context.
   *    If `mode` is `Single` and a `Txn` is active on the current thread, then
   *    the bound context is an unnamed atomic block nested in the active
   *    `Txn`.  If `mode` is `Single` and there is no active transaction, or if
   *    `mode` is `Escaped`, then the bound context is an unnamed top level
   *    atomic block.
   *
   *  * Enclosing context: If `mode` is a `Txn`, that `Txn` is the enclosing
   *    context.  If `mode` is `Single` and there is an active `Txn` on the
   *    current thread, then that transaction is the enclosing context.  Top
   *    level `Single` and `Escaped` bound views have no enclosing context.
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
     *  the <code>Ref</code> to the write set of the enclosing context, if any.
     *  @return the current value of the bound <code>Ref</code>, as observed by
     *      the bound context.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def readForWrite: T

    /** Works like <code>set(v)</code>, but returns the old value.  This is an
     *  atomic swap, equivalent to atomically performing a <code>get</code>
     *  followed by <code>set(v)</code>.
     *  @return the previous value of the bound <code>Ref</code>, as observed
     *      by the bound context.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def getAndSet(v: T): T

    /** Equivalent to atomically executing
     *  <code>(if (before == get) { set(after); true } else false)</code>, but
     *  may be more efficient, especially if there is no enclosing context.
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
     *  may be more efficient, especially if there is no enclosing context.
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

    /** Atomically replaces the value ''v'' stored in the `Ref` with
     *  `f`(''v'').  Some implementations may defer execution of `f` or call
     *  `f` multiple times.
     *  @param f a function that is safe to call multiple times, and safe to
     *      call later during the enclosing context, if any.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def transform(f: T => T)

    /** Atomically replaces the value ''v'' stored in the `Ref` with
     *  `f`(''v''), returning the old value.  `transform` should be preferred
     *  in a transactional context if the return value is not needed and `f` is
     *  idempotent, since it gives the STM more flexibility to avoid
     *  transaction conflicts.
     */
    def getAndTransform(f: T => T): T

    /** Either atomically transforms this reference without blocking and
     *  returns true, or returns false.  `transform` is to `tryTransform` as
     *  `set` is to `tryWrite`.  A true return value does not necessarily mean
     *  that `f` has already been called, just that the transformation will be
     *  performed in the bound context if it commits.
     *  @param f a function that is safe to call multiple times, and safe to
     *      call later during the enclosing context, if any.
     *  @return true if the function was (or will be) applied, false if it was not.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def tryTransform(f: T => T): Boolean

    /** Atomically replaces the value ''v'' stored in the `Ref` with
     *  `pf`(''v'') if `pf.isDefinedAt`(''v''), returning true, otherwise
     *  leaves the element unchanged and returns false.  `pf.apply` and
     *  `pf.isDefinedAt` may be called multiple times, and may be called later
     *  in the enclosing context.
     *  @param pf a partial function that is safe to call multiple times, and
     *      safe to call later in the enclosing context, if any.
     *  @return `pf.isDefinedAt``(''v''), where ''v'' is the element held by 
     *      this `Ref` on entry.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def transformIfDefined(pf: PartialFunction[T,T]): Boolean

    override def equals(rhs: Any) = rhs match {
      case b: Bound[_] => (mode == b.mode) && (unbind == b.unbind)
      case _ => false
    }

    override def hashCode: Int = (mode.hashCode * 137) ^ unbind.hashCode ^ 101

    override def toString: String = {
      "Bound(" + unbind + ", " + mode + " => " + get + ")"
    }
  }
}

/** Provides access to a single element of type ''T''.  Accesses are
 *  performed as part of a ''memory transaction'' that comprises all of the
 *  operations of an atomic block and any nested blocks.  Single-operation
 *  memory transactions may be performed without an explicit atomic block using
 *  the instance returned from `single`.  The software transactional memory
 *  performs concurrency control to make sure that all committed transactions
 *  are linearizable.  Reads and writes performed by a successful transaction
 *  return the same values as if they were executed instantaneously at the
 *  transaction's commit (linearization) point.
 *  STM.
 *
 *  The static scope of an atomic block is defined by access to an implicit
 *  `Txn` passed to the block by the STM.  Atomic blocks nest, so to
 *  participate in an atomic block for which the `Txn` is not conveniently
 *  available, just create a new atomic block using `STM.atomic`.  In the
 *  static scope of an atomic block reads and writes of a `x: Ref` are
 *  performed with either `x.get` and `x.set(v)`, or with `x()` and `x := v`.
 *  `single` provides a means to dynamically resolve the current scope during
 *  each method call.
 *
 *  It is possible for separate `Ref` instances to refer to the same element;
 *  in this case they will compare equal.  (As an example, a transactional
 *  array class might store elements in an array and create `Ref`s on demand.)
 *  `Ref`s (or `Source`s) may be provided for computed values, such as the
 *  emptiness of a queue, to allow conditional retry and waiting on semantic
 *  properties.
 *
 *  To perform an access outside a transaction, use the view returned by
 *  `single`.  Each access through the returned view will act as if it was
 *  performed in its own single-operation transaction, dynamically nesting into
 *  an active atomic block as appropriate.
 *
 *  Concrete `Ref` instances may be obtained from the factory methods in
 *  `Ref`'s companion object.
 *  @see edu.stanford.ppl.ccstm.STM
 *
 *  @author Nathan Bronson
 */
trait Ref[T] extends Source[T] with Sink[T] {

  /** Provides access to the data and metadata associated with this reference.
   *  This is the only method for which the default <code>Ref</code>
   *  implementation is not sufficient.
   */
  private[ccstm] def handle: impl.Handle[T]

  /** Provides access to the handle for use by non-transactional direct access. */ 
  private[ccstm] def nonTxnHandle = handle

  //////////////// Source stuff

  def apply()(implicit txn: Txn): T = get
  def get(implicit txn: Txn): T = txn.get(handle)
  def map[Z](f: (T) => Z)(implicit txn: Txn): Z = txn.map(handle, f)

  //////////////// Sink stuff

  def :=(v: T)(implicit txn: Txn) { set(v) }
  def set(v: T)(implicit txn: Txn) { txn.set(handle, v) }

  //////////////// Ref functions

  /** Works like <code>set(v)</code>, but returns the old value.  This is an
   *  atomic swap, equivalent to atomically performing a <code>get</code>
   *  followed by <code>set(v)</code>.
   *  @return the previous value of this <code>Ref</code>, as observed by
   *      <code>txn</code>.
   *  @throws IllegalStateException if <code>txn</code> is not active.
   */
  def getAndSet(v: T)(implicit txn: Txn): T = {
    txn.getAndSet(handle, v)
  }

  /** Transforms the value referenced by this <code>Ref</code> by applying the
   *  function <code>f</code>.  Acts like <code>ref.set(f(ref.get))</code>, but
   *  the execution of <code>f</code> may be deferred or duplicated to reduce
   *  transaction conflicts.
   *  @param f a function that is safe to call multiple times, and safe to
   *      call later during the transaction.
   *  @throws IllegalStateException if <code>txn</code> is not active.
   */
  def transform(f: T => T)(implicit txn: Txn) {
    // only sub-types of Ref actually perform deferral, the base implementation
    // evaluates f immediately
    txn.getAndTransform(handle, f)
  }

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
  def transformIfDefined(pf: PartialFunction[T,T])(implicit txn: Txn): Boolean = {
    txn.transformIfDefined(handle, pf)
  }

  /** Returns this instance, but with only the read-only portion accessible.
   *  Equivalent to <code>asInstanceOf[Source[T]]</code>, but may be more
   *  readable.
   *  @return this instance, but with only read-only methods accessible
   */
  def source: Source[T] = this

  //////////////// BindingMode

  /** Returns a reference view that does not require an implicit
   *  <code>Txn</code> parameter on each method call, but instead always
   *  performs accesses in the context of <code>txn</code>.  A transaction may
   *  be bound regardless of its state, but reads and writes are only allowed
   *  while a transaction is active.  The view returned from this method may be
   *  convenient when passing <code>Ref</code>s to scopes that would not
   *  otherwise have implicit access to <code>txn</code>, and the view provides
   *  some extra functionality that is less frequently needed.
   *  @param txn a transaction to be bound to the view.
   *  @return a view of this instance that performs all accesses as if from
   *      <code>txn</code>.
   */
  def bind(implicit txn: Txn): Ref.Bound[T] = new impl.TxnBound(this, handle, txn)

  /** Returns a view that acts as if each operation is performed in an atomic
   *  block containing that single operation.  The new single-operation
   *  transaction will be nested inside an existing transaction if one is
   *  active (see `Txn.current`).  The returned instance is valid for the
   *  lifetime of the program.
   *  @return a view into the value of this <code>Ref</code>, that will perform
   *      each operation as if in its own transaction.
   */
  def single: Ref.Bound[T] = new impl.SingleBound(this, nonTxnHandle, handle)

  /** (Uncommon) Returns a view that can be used to perform individual reads 
   *  and writes to this reference outside any transactional context,
   *  regardless of whether a transaction is active on the current thread.
   *  Each operation acts as if it was performed in its own transaction while
   *  any active transaction is suspended.  The returned instance is valid for
   *  the lifetime of the program.
   *  @return a view into the value of this <code>Ref</code>, that will bypass
   *      any active transaction.
   */
  def escaped: Ref.Bound[T] = new impl.EscapedBound(this, nonTxnHandle)

  @deprecated("replace with Ref.single if possible, otherwise use Ref.escaped")
  def nonTxn: Ref.Bound[T] = escaped

  override def hashCode: Int = {
    val h = handle
    impl.STMImpl.hash(h.ref, h.offset)
  }

  override def equals(rhs: Any): Boolean = {
    (this eq rhs.asInstanceOf[AnyRef]) || (rhs match {
      case r: Ref[_] => {
        val h1 = handle
        val h2 = r.handle
        (h1.ref eq h2.ref) && (h1.offset == h2.offset)
      }
      case _ => false
    })
  }

  override def toString: String = {
    getClass.getSimpleName + "@" + (System.identityHashCode(this).toHexString)
  }
}

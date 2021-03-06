/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// Ref.scala

package edu.stanford.ppl.ccstm

/** An object that provides factory methods for `Ref` instances.
 *
 *  @author Nathan Bronson
 */
object Ref {
  import impl._

  /** Returns a new `Ref` instance suitable for holding instances of `T`.
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
   *
   *  Example: {{{
   *    val x = Ref("initial") // creates a Ref[String]
   *    val head1 = Ref(Nil : List[String]) // creates a Ref[List[String]] 
   *    val head2 = Ref[List[String]](Nil)  // creates a Ref[List[String]]
   *  }}}
   */
  def apply[T](initialValue: T)(implicit m: ClassManifest[T]): Ref[T] = {
    // TODO: this is likely to be a hot spot, perhaps RFE a method in Manifest for this test?
    if (m.isInstanceOf[scala.reflect.AnyValManifest[_]]) {
      newPrimitiveRef(initialValue)
    } else {
      new TAnyRef(initialValue)
    }
  }

  def apply(initialValue: Boolean): Ref[Boolean] = new TBooleanRef(initialValue)
  def apply(initialValue: Byte   ): Ref[Byte]    = new TByteRef(   initialValue)
  def apply(initialValue: Short  ): Ref[Short]   = new TShortRef(  initialValue)
  def apply(initialValue: Char   ): Ref[Char]    = new TCharRef(   initialValue)
  def apply(initialValue: Int    ): Ref[Int]     = new TIntRef(    initialValue)
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


  /** Returns a `Ref` instance that trades higher overhead for a reduced need
   *  to roll back transactions.  All operations and their result values are
   *  recorded, and writes will be delayed until just prior to commit.  If
   *  another transaction writes to the reference, a conflict will be avoided
   *  if the history can be replayed starting with the new value without an
   *  observable difference (detected using `==` on the values).
   * 
   *  `Ref.getWith` performs a similar function on a call-by-call basis.
   */
  def lazyConflict[T](initialValue: T)(implicit m: ClassManifest[T]) = new LazyConflictRef(initialValue)

  /** Returns a `Ref[Int]` instance optimized for concurrent increment and
   *  decrement using `+=` and `-=`.
   */
  def striped(initialValue: Int) = new StripedIntRef(initialValue)


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
   *   - Bound context: If `mode` is a `Txn`, that `Txn` is the bound context.
   *     If `mode` is `Single` and a `Txn` is active on the current thread,
   *     then the bound context is an unnamed atomic block nested in the active
   *     `Txn`.  If `mode` is `Single` and there is no active transaction, or
   *     if `mode` is `Escaped`, then the bound context is an unnamed top level
   *     atomic block.
   *
   *   - Enclosing context: If `mode` is a `Txn`, that `Txn` is the enclosing
   *     context.  If `mode` is `Single` and there is an active `Txn` on the
   *     current thread, then that transaction is the enclosing context.  Top
   *     level `Single` and `Escaped` bound views have no enclosing context.
   */
  trait View[T] extends Source.View[T] with Sink.View[T] {

    /** Provides access to a `Ref` that refers to the same value as
     *  the one that was bound to produce this `Ref.View` instance.
     *  The returned `Ref` might be the original reference or it might be a new
     *  instance that is equal to the original.  It is always true that
     *  `ref.bind.unbind == ref`.
     *  @return a `Ref` instance equal to the one that was bound to create this
     *      view instance.
     */
    def unbind: Ref[T]

    /** Returns the same value as that returned by `get`, but adds
     *  the `Ref` to the write set of the enclosing context, if any.
     *  @return the current value of the bound `Ref`, as observed by
     *      the bound context.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def readForWrite: T

    /** Works like `set(v)`, but returns the old value.  This is an
     *  atomic swap, equivalent to atomically performing a `get`
     *  followed by `set(v)`.
     *  @return the previous value of the bound `Ref`, as observed
     *      by the bound context.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def swap(v: T): T

    /** Equivalent to atomically executing
     *  `(if (before == get) { set(after); true } else false)`, but
     *  may be more efficient, especially if there is no enclosing context.
     *  @param before a value to compare against the current `Ref` contents.
     *  @param after a value to store in the `Ref` if `before` was equal to the
     *      previous cell contents.
     *  @return true if `before` was equal to the previous value of the bound
     *      `Ref`, false otherwise.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def compareAndSet(before: T, after: T): Boolean

    /** Equivalent to atomically executing
     *  `(if (before eq get) { set(after); true } else false)`, but
     *  may be more efficient, especially if there is no enclosing context.
     *  @param before a reference whose identity will be compared against the
     *      current `Ref` contents.
     *  @param after a value to store in the `Ref` if `before` has the same
     *      reference identity as the previous cell contents.
     *  @return true if `before` has the same reference identity as the
     *      previous value of the bound `Ref`, false otherwise.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active. */
    def compareAndSetIdentity[A <: T with AnyRef](before: A, after: T): Boolean

    /** Atomically replaces the value ''v'' stored in the `Ref` with
     *  `f`(''v'').  Some `Ref` implementations may defer execution of `f` or
     *  call `f` multiple times to reduce conflicts.
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
     *  @param f a function that is safe to call multiple times, and safe to
     *      call later during the enclosing context, if any.
     *  @return the previous value of the bound `Ref`, as observed
     *      by the bound context.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active. */
    def getAndTransform(f: T => T): T

    /** Either atomically transforms this reference without blocking and
     *  returns true, or returns false.  `transform` is to `tryTransform` as
     *  `set` is to `trySet`.  A true return value does not necessarily mean
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

    /** Transforms the value stored in the `Ref` by incrementing it.
     *
     *  '''Note: Some `Ref` implementations may choose to ignore the passed-in
     *  `Numeric[T]` instance if `T` is a primitive type.'''
     *
     *  @param rhs the quantity by which to increment the value of `unbind`.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def += (rhs: T)(implicit num: Numeric[T]) { transform { v => num.plus(v, rhs) } }

    /** Transforms the value stored in the `Ref` by decrementing it.
     *
     *  '''Note: Some `Ref` implementations may choose to ignore the passed-in
     *  `Numeric[T]` instance if `T` is a primitive type.'''
     *
     *  @param rhs the quantity by which to decrement the value of `unbind`.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def -= (rhs: T)(implicit num: Numeric[T]) { transform { v => num.minus(v, rhs) } }

    /** Transforms the value stored in the `Ref` by multiplying it.
     *
     *  '''Note: Some `Ref` implementations may choose to ignore the passed-in
     *  `Numeric[T]` instance if `T` is a primitive type.'''
     *
     *  @param rhs the quantity by which to multiple the value of `unbind`.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def *= (rhs: T)(implicit num: Numeric[T]) { transform { v => num.times(v, rhs) } }

    /** Transforms the value stored in the `Ref` by performing a division on it,
     *  throwing away the remainder.  The careful reader will note that division
     *  is actually provided by either an implicit `Fractional[T]` or an implicit
     *  `Integral[T]`.  Due to problems overloading based on the available
     *  implicits we accept any `Numeric[T]` and assume that it can be converted
     *  at runtime into either of the two previous types.
     *
     *  '''Note: Some `Ref` implementations may choose to ignore the passed-in
     *  `Integral[T]` instance if `T` is a primitive type.'''
     *
     *  @param rhs the quantity by which to divide the value of `unbind`.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def /= (rhs: T)(implicit num: Numeric[T]) {
      num match {
        case numF: Fractional[T] => transform { v => numF.div(v, rhs) }
        case numI: Integral[T] => transform { v => numI.quot(v, rhs) }
      }
    }

    override def equals(rhs: Any) = rhs match {
      case b: View[_] => (mode == b.mode) && (unbind == b.unbind)
      case _ => false
    }

    override def hashCode: Int = (mode.hashCode * 137) ^ unbind.hashCode ^ 101

    override def toString: String = {
      "View(" + unbind + ", " + mode + " => " + get + ")"
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
 *
 *  The static scope of an atomic block is defined by access to an implicit
 *  `Txn` passed to the block by the STM.  Atomic blocks nest, so to
 *  participate in an atomic block for which the `Txn` is not conveniently
 *  available, just create a new atomic block using {{{
 *    atomic { implicit t =>
 *      // the body
 *    }
 *  }}}
 *  In the static scope of an atomic block reads and writes of a `Ref`
 *  are performed by `x.get` and `x.set(v)`, or more concisely by `x()` and
 *  `x() = v`. `x.single` returns a `Ref.View` that will dynamically resolve
 *  the current scope during each method call, automatically creating a
 *  single-operation atomic block if no transaction is active.
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
 *
 *  @author Nathan Bronson
 */
trait Ref[T] extends Source[T] with Sink[T] {

  /** Works like `set(v)`, but returns the old value.  This is an
   *  atomic swap, equivalent to atomically performing a `get`
   *  followed by `set(v)`.
   *  @return the previous value of this `Ref`, as observed by `txn`.
   *  @throws IllegalStateException if `txn` is not active.
   */
  def swap(v: T)(implicit txn: Txn): T

  /** Transforms the value referenced by this `Ref` by applying the
   *  function `f`.  Acts like `ref.set(f(ref.get))`, but
   *  the execution of `f` may be deferred or duplicated to reduce
   *  transaction conflicts.
   *  @param f a function that is safe to call multiple times, and safe to
   *      call later during the transaction.
   *  @throws IllegalStateException if `txn` is not active.
   */
  def transform(f: T => T)(implicit txn: Txn)

  /** Transforms the value referenced by this `Ref` by applying the
   *  `pf.apply`, but only if `pf.isDefinedAt` holds for
   *  the current value.  Returns true if a transformation was performed, false
   *  otherwise.  `pf.apply` and `pf.isDefinedAt` may be
   *  called multiple times, and may be deferred until later in the
   *  transaction.
   *  @param pf a partial function that is safe to call multiple times, and
   *      safe to call later in the transaction.
   *  @return `pf.isDefinedAt(v)`, where `v` was the value of this `Ref`
   *      before transformation (if any).
   *  @throws IllegalStateException if `txn` is not active.
   */
  def transformIfDefined(pf: PartialFunction[T,T])(implicit txn: Txn): Boolean

  // numeric stuff

  /** Transforms the value stored in the `Ref` by incrementing it.
   *
   *  '''Note: Some `Ref` implementations may choose to ignore the passed-in
   *  `Numeric[T]` instance if `T` is a primitive type.'''
   *
   *  @param rhs the quantity by which to increment the value of this `Ref`.
   *  @throws IllegalStateException if `txn` is not active. */
  def += (rhs: T)(implicit txn: Txn, num: Numeric[T]) { transform { v => num.plus(v, rhs) } }

  /** Transforms the value stored in the `Ref` by decrementing it.
   *
   *  '''Note: Some `Ref` implementations may choose to ignore the passed-in
   *  `Numeric[T]` instance if `T` is a primitive type.'''
   *
   *  @param rhs the quantity by which to decrement the value of this `Ref`.
   *  @throws IllegalStateException if `txn` is not active. */
  def -= (rhs: T)(implicit txn: Txn, num: Numeric[T]) { transform { v => num.minus(v, rhs) } }

  /** Transforms the value stored in the `Ref` by multiplying it.
   *
   *  '''Note: Some `Ref` implementations may choose to ignore the passed-in
   *  `Numeric[T]` instance if `T` is a primitive type.'''
   *
   *  @param rhs the quantity by which to multiply the value of this `Ref`.
   *  @throws IllegalStateException if `txn` is not active.
   */
  def *= (rhs: T)(implicit txn: Txn, num: Numeric[T]) { transform { v => num.times(v, rhs) } }

  /** Transforms the value stored in the `Ref` by performing a division on it,
   *  throwing away the remainder.  The careful reader will note that division
   *  is actually provided by either an implicit `Fractional[T]` or an implicit
   *  `Integral[T]`.  Due to problems overloading based on the available
   *  implicits we accept any `Numeric[T]` and assume that it can be converted
   *  at runtime into either of the two previous types.
   *
   *  '''Note: Some `Ref` implementations may choose to ignore the passed-in
   *  `Numeric[T]` instance if `T` is a primitive type.'''
   *
   *  @param rhs the quantity by which to divide the value of this `Ref`.
   *  @throws IllegalStateException if `txn` is not active.
   */
  def /= (rhs: T)(implicit txn: Txn, num: Numeric[T]) {
    num match {
      case numF: Fractional[T] => transform { v => numF.div(v, rhs) }
      case numI: Integral[T] => transform { v => numI.quot(v, rhs) }
    }
  }

  //////////////// AccessMode

  /** Returns a reference view that does not require an implicit
   *  `Txn` parameter on each method call, but instead always
   *  performs accesses in the context of `txn`.  A transaction may
   *  be bound regardless of its state, but reads and writes are only allowed
   *  while a transaction is active.  The view returned from this method may be
   *  convenient when passing `Ref`s to scopes that would not
   *  otherwise have implicit access to `txn`, and the view provides
   *  some extra functionality that is less frequently needed.
   *  @param txn a transaction to be bound to the view.
   *  @return a view of this instance that performs all accesses as if from
   *      `txn`.
   */
  def bind(implicit txn: Txn): Ref.View[T]

  /** Returns a view that acts as if each operation is performed in an atomic
   *  block containing that single operation.  The new single-operation
   *  transaction will be nested inside an existing transaction if one is
   *  active (see `Txn.current`).  The returned instance is valid for the
   *  lifetime of the program.
   *  @return a view into the value of this `Ref`, that will perform
   *      each operation as if in its own transaction.
   */
  def single: Ref.View[T]

  /** (Uncommon) Returns a view that can be used to perform individual reads 
   *  and writes to this reference outside any transactional context,
   *  regardless of whether a transaction is active on the current thread.
   *  Each operation acts as if it was performed in its own transaction while
   *  any active transaction is suspended.  The returned instance is valid for
   *  the lifetime of the program.
   *  @return a view into the value of this `Ref`, that will bypass
   *      any active transaction.
   */
  def escaped: Ref.View[T]

  private[ccstm] def embalm(identity: Int)
  private[ccstm] def resurrect(identity: Int)

  override def toString: String = {
    // we use hashCode instead of System.identityHashCode, because we want
    // Ref-s that point to the same location to have the same string  
    getClass.getSimpleName + "->" + (hashCode.toHexString)
  }
}

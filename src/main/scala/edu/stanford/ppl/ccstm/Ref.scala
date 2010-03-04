/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// Ref.scala

package edu.stanford.ppl.ccstm

import reflect.AnyValManifest

/** An object that provides factory methods for <code>Ref</code> instances.
 *  @see edu.stanford.ppl.ccstm.Ref
 *
 *  @author Nathan Bronson
 */
object Ref {
  import collection._

  /** Returns a new <code>Ref</code> instance suitable for holding instances of
   *  <code>T</code>.
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

  /** Returns a new <code>Ref</code> instance with the specified initial
   *  value.
   *  <p>
   *  The returned instance is not part of any transaction's read or write set.
   *  This is normally not a problem, because transactional references to new
   *  <code>Ref</code>s are isolated from other contexts.  If a
   *  non-transactional reference to the new <code>Ref</code> is published,
   *  however, this can result in unintuitive behavior.  To avoid this problem,
   *  read from the reference prior to leaking a reference to it from inside
   *  the constructing transaction.
   */
  def apply[T](initialValue: T)(implicit m: ClassManifest[T]): Ref[T] = {
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

    /** Works like <code>set(v)</code>, but returns the old value.  This is an
     *  atomic swap, equivalent to atomically performing a <code>get</code>
     *  followed by <code>set(v)</code>.
     *  @return the previous value of the bound <code>Ref</code>, as observed
     *      by the binding context.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def getAndSet(v: T): T

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
     *  with <code>f</code>(<i>v</i>), returning the old value.
     *  <code>getAndTransform</code> should be preferred in a transactional context,
     *  since when implementing <code>getAndTransform</code> the STM cannot
     *  defer execution of <code>f</code> to reduce transaction conflicts.
     */
    def getAndTransform(f: T => T): T

    /** Either atomically transforms this reference without blocking and
     *  returns true, or returns false.  <code>transform</code> is to
     *  <code>tryTransform</code> as <code>set</code> is to
     *  <code>tryWrite</code>.
     *  @param f a function that is safe to call multiple times, and safe to
     *      call later during the bound transaction (if any).
     *  @returns true if the function was applied, false if it was not.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def tryTransform(f: T => T): Boolean

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

    override def toString: String = {
      val c = (context match { case Some(t) => t.toString; case None => "nonTxn" }) 
      "Bound(" + unbind + ", " + c + " => " + get + ")"
    }
  }

  private[ccstm] class TxnBound[T](val unbind: Ref[T],
                                   protected val handle: impl.Handle[T],
                                   txn: Txn) extends Ref.Bound[T] {
    
    def context: Option[Txn] = Some(txn)

    def get: T = txn.get(handle)
    def map[Z](f: (T) => Z): Z = txn.map(handle, f)
    def await(pred: (T) => Boolean) { if (!pred(get)) txn.retry }
    def unrecordedRead: UnrecordedRead[T] = txn.unrecordedRead(handle)
    def releasableRead: ReleasableRead[T] = txn.releasableRead(handle)

    def set(v: T) { txn.set(handle, v) }
    def tryWrite(v: T): Boolean = txn.tryWrite(handle, v)

    def readForWrite: T = txn.readForWrite(handle)
    def getAndSet(v: T): T = txn.getAndSet(handle, v)
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
      // this isn't as silly as it seems, because some Bound implementations
      // override getAndTransform()
      txn.getAndTransform(handle, f)
    }
    def getAndTransform(f: T => T): T = {
      txn.getAndTransform(handle, f)
    }
    def tryTransform(f: T => T): Boolean = {
      txn.tryTransform(handle, f)
    }
    def transformIfDefined(pf: PartialFunction[T,T]): Boolean = {
      txn.transformIfDefined(handle, pf)
    }
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

  /** Provides access to the handle for use by non-transactional direct access. */ 
  private[ccstm] def nonTxnHandle = handle

  //////////////// Source stuff

  def unary_!(implicit txn: Txn): T = get
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
  def bind(implicit txn: Txn): Ref.Bound[T] = new Ref.TxnBound(this, handle, txn)

  /** Returns a view that can be used to perform individual reads and writes to
   *  this reference outside any transactional context.  Each operation acts as
   *  if it was performed in its own transaction.  The returned instance is
   *  valid for the lifetime of the program.
   *  @return a view into the value of this <code>Ref</code>, that will perform
   *      each operation as if in its own transaction.
   */
  def nonTxn: Ref.Bound[T] = new impl.NonTxnBound(this, nonTxnHandle)

  override def hashCode: Int = {
    val h = handle
    impl.STMImpl.hash(h.ref, h.offset)
  }

  override def equals(rhs: Any): Boolean = {
    rhs match {
      case r: Ref[_] => {
        val h1 = handle
        val h2 = r.handle
        (h1.ref eq h2.ref) && (h1.offset == h2.offset)
      }
      case _ => false
    }
  }

  override def toString: String = {
    getClass.getSimpleName + "@" + (System.identityHashCode(this).toHexString)
  }
}

/* CCSTM - (c) 2009 Stanford University - PPL */

// Ref.scala

package edu.stanford.ppl.ccstm


import java.util.concurrent.atomic.AtomicReferenceFieldUpdater

/** An object that provides a factory method for <code>Ref</code> instances.
 *  @see edu.stanford.ppl.ccstm.Ref
 *
 *  @author Nathan Bronson
 */
object Ref {
  
  /** Returns a new <code>Ref</code> instance with the specified initial
   *  value.
   */
  def apply[T](initialValue: T) = new Ref(initialValue)

  
  /** <code>Source</code> defines the covariant read-only interface of a
   *  <code>Ref</code> instance.
   */
  trait Source[+T] {
    /** Performs a transactional read.  Equivalent to <code>elem</code> or
     *  <code>bind(txn).elem</code>.
     *  @see edu.stanford.ppl.ccstm.Ref.Source#bind
     *  @see edu.stanford.ppl.ccstm.Ref.BoundSource#elem
     */
    def unary_! (implicit txn: Txn): T = elem(txn)

    /** Performs a transactional read.  Equivalent to
     *  <code>bind(txn).elem</code>.
     *  @see edu.stanford.ppl.ccstm.Ref.Source#bind
     *  @see edu.stanford.ppl.ccstm.Ref.BoundSource#elem
     */
    def elem(implicit txn: Txn): T = bind(txn).elem

    /** Equivalent to <code>bind(txn).map(f)</code>.
     *  @see edu.stanford.ppl.ccstm.Ref.Source#bind
     *  @see edu.stanford.ppl.ccstm.Ref.BoundSource#map
     */
    def map[Z](f: T => Z)(implicit txn: Txn) = bind(txn).map(f)

    /** Equivalent to <code>bind(txn).await(pred)</code>.
     *  @see edu.stanford.ppl.ccstm.Ref.Source#bind
     *  @see edu.stanford.ppl.ccstm.Ref.BoundSource#await
     */
    def await(pred: T => Boolean)(implicit txn: Txn) = bind(txn).await(pred)

    /** Returns a view on this <code>Ref.Source</code> that can be used to read
     *  the cell's value as part of a transaction <code>txn</code>.  A transaction
     *  may be bound regardless of its state, but reads (and writes) are only
     *  allowed while a transaction is active.
     *  @param txn a transaction to be bound to the view.
     *  @return a read-only view onto the value of a <code>Ref</code> under the
     *      context of <code>txn</code>.
     */
    def bind(implicit txn: Txn): BoundSource[T]

    /** Returns a view of this <code>Ref.Source</code> that can be used to perform
     *  individual (non-transactional) reads of the cell's value.  The returned
     *  view acts as if each operation is performed in its own transaction.
     *  The returned view is valid for the lifetime of the program.
     *  @return a read-only view into the value of a <code>Ref</code> that will
     *      perform each read operation as if in its own transaction.
     */
    def nonTxn: BoundSource[T]

    // implicit access to unrecordedRead is omitted to discourage its use
  }

  /** <code>Sink</code> defines the contravariant write-only interface of a
   *  <code>Ref</code> instance.
   */
  trait Sink[-T] {
    /** Performs a transactional write.  Equivalent to
     *  <code>bind(txn).elem_=(v)</code>.
     *  @see edu.stanford.ppl.ccstm.Ref.Sink#bind
     *  @see edu.stanford.ppl.ccstm.Ref.BoundSink#elem_=
     */
    def := (v: T)(implicit txn: Txn) { bind(txn).elem_=(v) }

    /** Performs a transactional write.  Equivalent to
     *  <code>bind(txn).elem_=(v)</code>.
     *  <p>
     *  Note that Scala allows this method to be called without the underscore
     *  (as a property setter) only if the corresponding property getter is
     *  available from <code>Ref.Source</code>.
     *  @see edu.stanford.ppl.ccstm.Ref.Sink#bind
     *  @see edu.stanford.ppl.ccstm.Ref.BoundSink#elem_=
     */
    def elem_=(v: T)(implicit txn: Txn) { bind(txn).elem_=(v) }

    /** Returns a view on this <code>Ref.Source</code> that can be used to write
     *  the cell's value as part of a transaction <code>txn</code>.  A transaction
     *  may be bound regardless of its state, but writes (and reads) are only
     *  allowed while a transaction is active.
     *  @param txn a transaction to be bound to the view.
     *  @return a write-only view onto the value of a <code>Ref</code> under the
     *      context of <code>txn</code>.
     */
    def bind(implicit txn: Txn): BoundSink[T]

    /** Returns a view of this <code>Ref.Sink</code> that can be used to
     *  perform individual (non-transactional) writes of the cell's value.  The
     *  returned view acts as if each operation is performed in its own
     *  transaction. The returned view is valid for the lifetime of the program.
     *  @return a write-only view into the value of a <code>Ref</code>, that will
     *      perform each update operation as if in its own transaction.
     */
    def nonTxn: BoundSink[T]
  }

  /** <code>BoundSource</code> defines the covariant read-only view of a
   *  <code>Ref</code> bound to a particular context.  The context may be
   *  either a <code>Txn</code>, which guarantees that all reads will observe
   *  the same values as if they were executed at the transaction's commit
   *  (linearization) point, or the non-transactional context, which guarantees
   *  that each read will be linearized with all writes to the cell.
   *  @see edu.stanford.ppl.ccstm.Ref
   */
  trait BoundSource[+T] {
    /** Returns <code>Some(txn)</code> if this view is bound to a transaction
     *  <code>txn</code>, <code>None</code> if it is bound to the
     *  non-transactional context.
     */
    def context: Option[Txn]

    /** Reads the element held by the bound <code>Ref</code>.  If this view
     *  was created by <code>Ref.bind(txn)</code> (or
     *  <code>Ref.Source.bind(txn)</code>) then this method will include the
     *  results of previous modifications of the <code>Ref</code> in
     *  <code>txn</code>, and will validate <code>txn</code> prior to
     *  returning.  If this view was created by <code>Ref.nonTxn</code> (or
     *  <code>Ref.Source.nonTxn</code>) the read will be strongly atomic and
     *  isolated with respect to all transactions.  This means that if a
     *  non-txn read observes a value stored by transaction <i>A</i>, any
     *  subsequent non-txn read on the same thread is guaranteed to observe all
     *  changes committed by <i>A</i>.
     *  @return the current value of the bound <code>Ref</code> as observed by
     *      the binding context.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     *  @see edu.stanford.ppl.ccstm.Ref#bind
     *  @see edu.stanford.ppl.ccstm.Ref#nonTxn
     */
    def elem: T

    /** Returns <code>f(elem)</code>, possibly reevaluating <code>f</code> to
     *  avoid rollback if a conflicting change is made to <code>elem</code> but
     *  both the old and new values are equal after application of
     *  <code>f</code>.  Requires that <code>f(x) == f(x)</code>.
     *  @param f an idempotent function.
     *  @return the result of applying <code>f</code> to the value read by this
     *      view.
     *  @see edu.stanford.ppl.ccstm.Ref.BoundSource#elem
     */
    def map[Z](f: T => Z): Z = {
      context match {
        case None => {
          // non-transactional, no semantic conflict detection
          f(elem)
        }
        case Some(txn) => {
          val u = unrecordedRead
          val result = f(u.value)
          if (!u.recorded) {
            val callback = new Txn.ReadResource {
              var _latestRead = u

              def valid(txn: Txn) = {
                if (!_latestRead.stillValid) {
                  // reread, and see if that changes the result
                  _latestRead = unrecordedRead
                  val reapply = f(_latestRead.value)
                  result == reapply
                } else {
                  true
                }
              }
            }

            // It is safe to skip calling callback.valid() here, because we
            // have made no calls into the txn that might have resulted in it
            // moving its virtual snapshot forward.  This means that the
            // unrecorded read that initialized u is consistent with all of the
            // reads performed so far.
            txn.addReadResource(callback, 0, false)
          }

          result
        }
      }
    }

    /** Blocks until <code>pred(elem)</code> is true, in a manner consistent
     *  with the current context.  If called from a transactional context this
     *  method performs a conditional retry of the transaction if the predicate
     *  is not true.  If called from a non-transactional context this method
     *  blocks until the predicate holds.  Requires that the predicate be safe
     *  to reevaluate, and that <code>pred(x) == pred(x)</code>.
     *  @param pred an idempotent predicate.
     *  @see edu.stanford.ppl.ccstm.Txn#retry
     */
    def await(pred: T => Boolean)

    /** Returns an <code>UnrecordedRead</code> instance that wraps the value as
     *  that would be returned from <code>elem</code>, but if this source is
     *  bound to a transactional context does not add the <code>Ref</code> to
     *  the transaction's read set.  The caller is responsible for guaranteeing
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
     *  Although this method does not add to the read set, it validates the
     *  existing elements of the read set prior to returning.
     *  @return an <code>UnrecordedRead</code> instance that holds the read value
     *      and allows explicit revalidation.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     *  @see edu.stanford.ppl.ccstm.Ref.BoundSource#elem
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
   *  to the cell.
   *  @see edu.stanford.ppl.ccstm.Ref
   */
  trait BoundSink[-T] {
    /** Returns <code>Some(txn)</code> if this view is bound to a transaction
     *  <code>txn</code>, <code>None</code> if it is bound to the
     *  non-transactional context.
     */
    def context: Option[Txn]

    /** Updates the element held by the <code>Ref</code>.  If this view was
     *  created by <code>Ref.bind(txn)</code> (or
     *  <code>Ref.Sink.bind(txn)</code>) then the new value will not be
     *  visible to other contexts until (and unless) <code>txn</code>
     *  successfully commits.  If this view was created by
     *  <code>Ref.nonTxn</code> (or <code>Ref.Sink.nonTxn</code>) the value
     *  will be made available immediately, and a happens-before relationship
     *  will be established between this thread and any thread that reads the
     *  <code>v</code> stored by this method.
     *  <p>
     *  Note that Scala allows this method to be called without the underscore
     *  (as a property setter) only if the corresponding property getter is
     *  available from <code>Ref.Source</code>.
     *  @param v a value to store in the <code>Ref</code>.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     *  @see edu.stanford.ppl.ccstm.Ref#bind
     *  @see edu.stanford.ppl.ccstm.Ref#nonTxn
     */
    def elem_=(v: T)

    /** Updates the element held by the bound <code>Ref</code> without
     *  blocking and returns true, or does nothing and returns false.  For STM
     *  implementations that perform eager acquisition of write locks this
     *  method will return false if the bound <code>Ref</code> is already
     *  locked by a different (non-parent) transaction.  For STMs that perform
     *  lazy detection of write-write conflicts this method may operate in an
     *  identical fashion to <code>elem_=(v)</code>.  Regardless of the return
     *  value from this method the bound transaction (if any) will be
     *  validated.
     *  @param v a value to store in the <code>Ref</code>.
     *  @return true if the value was stored, false if nothing was done.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     *  @see edu.stanford.ppl.ccstm.Ref#elem_=
     */
    def tryWrite(v: T): Boolean
  }

  /** A <code>Ref</code> view that supports reads and writes.  Reads and writes
   *  are performed from the perspective of the bound context, which is either
   *  a <code>Txn</code> or the non-transactional context.
   */
  trait Bound[T] extends BoundSource[T] with BoundSink[T] {
    /** Returns the same value as <code>elem</code>, but adds the
     *  <code>Ref</code> to the write set of the bound transaction context,
     *  if any.  Equivalent to <code>elem</code> when called from a
     *  non-transactional context.
     *  @return the current value of the bound <code>Ref</code> as observed by
     *      the binding context.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def readForWrite: T = {
      // a more efficient implementation may be provided by the STM
      context match {
        case None => {
          // non-transactional, no difference from a normal read
          elem
        }
        case Some(txn) => {
          // transactional
          val z = elem
          elem = z
          z
        }
      }
    }

    /** Equivalent to atomically executing <code>(if (elem == before) { elem =
     *  after; true } else false)</code>, but may be more efficient.
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
    def compareAndSet(before: T, after: T): Boolean = {
      // a more efficient implementation may be provided by the STM
      transformIfDefined(new PartialFunction[T,T] {
        def isDefinedAt(v: T): Boolean = before == v
        def apply(v: T): T = after
      })
    }

    /** Equivalent to atomically executing <code>(if (elem eq before) { elem =
     *  after; true } else false)</code>, but may be more efficient.
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
    def compareAndSetIdentity[A <: T with AnyRef](before: A, after: T): Boolean = {
      // a more efficient implementation may be provided by the STM
      transformIfDefined(new PartialFunction[T,T] {
        def isDefinedAt(v: T): Boolean = (before eq v.asInstanceOf[AnyRef])
        def apply(v: T): T = after
      })
    }

    /** Works like <code>compareAndSet</code>, but allows spurious failures.  A
     *  false return from this method does not necessarily mean that the
     *  previous value of the <code>Ref</code> is not equal to
     *  <code>before</code>.
     *  @see edu.stanford.ppl.ccstm.Ref.Bound#compareAndSet
     */
    def weakCompareAndSet(before: T, after: T): Boolean = {
      // a more efficient implementation may be provided by the STM
      compareAndSet(before, after)
    }

    /** Works like <code>compareAndSetIdentity</code>, but allows spurious
     *  failures.  A false return from this method does not necessarily mean
     *  that the previous value of the <code>Ref</code> has a different
     *  reference identity than <code>before</code>.
     *  @see edu.stanford.ppl.ccstm.Ref.Bound#compareAndSetIdentity
     */
    def weakCompareAndSetIdentity[A <: T with AnyRef](before: A, after: T): Boolean = {
      // a more efficient implementation may be provided by the STM
      compareAndSetIdentity(before, after)
    }

    /** Atomically replaces the value <i>v</i> stored in the <code>Ref</code>
     *  with <code>f</code>(<i>v</i>), possibly deferring execution of
     *  <code>f</code> or calling <code>f</code> multiple times.
     *  @param f a function that is safe to call multiple times, and safe to
     *      call later during the bound transaction (if any).
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def transform(f: T => T) {
      // a more efficient implementation may be provided by the STM
      transformIfDefined(new PartialFunction[T,T] {
        def isDefinedAt(v: T): Boolean = true
        def apply(v: T): T = f(v)
      })
    }

    /** Atomically replaces the value <i>v</i> stored in the <code>Ref</code>
     *  with <code>pf</code>(<i>v</i>) if <code>pf.isDefinedAt</code>(<i>v</i>),
     *  returning true, otherwise leaves the element unchanged and returns
     *  false.  <code>pf.apply</code> and <code>pf.isDefinedAt</code> may be
     *  called multiple times in both transactional and non-transactional
     *  contexts.  They may be called after this method has returned in a
     *  transactional context, to avoid transaction rollback due to a
     *  conflicting update.
     *  @param pf a partial function that is safe to call multiple times, and
     *      safe to call later in the bound transaction (if any).
     *  @return <code>pf.isDefinedAt(<i>v</i>)</code>, where <i>v</i> is the
     *      element held by this <code>Ref</code> on entry.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def transformIfDefined(pf: PartialFunction[T,T]): Boolean
  }

  private[ccstm] val dataUpdater = (new Ref[Any](null)).newDataUpdater

  private[ccstm] trait Accessor[T] {
    val instance: Ref[T]
    def fieldIndex = 0
    def metadata = instance._metadata
    def metadata_=(v: STM.Metadata) { instance._metadata = v }
    def metadataCAS(before: STM.Metadata, after: STM.Metadata) = instance._metadataCAS(before, after)
    def data: STM.Data[T] = instance._data
    def data_=(v: STM.Data[T]) { instance._data = v }
    def dataCAS(before: STM.Data[T], after: STM.Data[T]) = {
      Ref.dataUpdater.compareAndSet(instance, before, after)
    }
  }
}

private class RefNonTxnAccessor[T](val instance: Ref[T]) extends STM.NonTxnAccessor[T] with Ref.Accessor[T]

private class RefTxnAccessor[T](val txn: Txn, val instance: Ref[T]) extends STM.TxnAccessor[T] with Ref.Accessor[T]

/** Holds a single element of type <i>T</i>, providing optimistic concurrency
 *  control using software transactional memory.  Reads and writes performed
 *  by a successful <code>Txn</code> return the same values as if they were
 *  executed atomically at the transaction's commit (linearization) point.
 *  Reads and writes performed in a non-transactional manner have sequential
 *  consistency with regard to all accesses (transactional and
 *  non-transactional) for STM data types.
 *  <p>
 *  If an implicit <code>Txn</code> instance is available, transactional reads
 *  and writes can be performed as if for a property named <code>elem</code>.
 *  A <code>Ref.Bound</code> view on the <code>Ref</code> may also be created
 *  that does not require the <code>Txn</code> to be available, and that
 *  reduces overheads if multiple accesses are performed to a <code>Ref</code>
 *  instance in the same transaction.
 *  <p>
 *  <code>Ref</code> allows subclassing to allow it to be opportunistically
 *  embedded in the internal node objects of collection classes, to reduce
 *  storage and indirection overheads.
 *  @see edu.stanford.ppl.ccstm.Txn
 *
 *  @author Nathan Bronson
 */
class Ref[T](initialValue: T) extends STM.MetadataHolder with Ref.Source[T] with Ref.Sink[T] {

  @volatile private[ccstm] var _data: STM.Data[T] = STM.initialData(initialValue)

  private[ccstm] def newDataUpdater = {
    // this must be a member of Ref, because all Scala variables are private
    // under the covers
    AtomicReferenceFieldUpdater.newUpdater(classOf[Ref[_]], classOf[STM.Data[_]], "_data")
  }

  // implicit access to readForWrite is omitted to discourage its use 

  /** Equivalent to <code>bind(txn).compareAndSet(before, after)</code>.
   *  @see edu.stanford.ppl.ccstm.Ref#bind
   *  @see edu.stanford.ppl.ccstm.Ref.Bound#compareAndSet
   */
  def compareAndSet(before: T, after: T)(implicit txn: Txn) = bind(txn).compareAndSet(before, after)

  /** Equivalent to <code>bind(txn).transform(f)</code>.
   *  @see edu.stanford.ppl.ccstm.Ref#bind
   *  @see edu.stanford.ppl.ccstm.Ref.Bound#transform
   */
  def transform(f: T => T)(implicit txn: Txn) { bind(txn).transform(f) }

  /** Equivalent to <code>bind(txn).transformIfDefined(pf)</code>.
   *  @see edu.stanford.ppl.ccstm.Ref#bind
   *  @see edu.stanford.ppl.ccstm.Ref.Bound#testAndTransform
   */
  def transformIfDefined(pf: PartialFunction[T,T])(implicit txn: Txn) = bind(txn).transformIfDefined(pf)

  /** Returns a view on this <code>Ref.Source</code> that can be used to read
   *  or write the cell's value as part of the transaction <code>txn</code>.  A
   *  transaction may be bound regardless of its state, but reads and writes
   *  are only allowed while a transaction is active.
   *  @param txn the transaction to be bound.
   *  @return a view onto the value of a <code>Ref</code> under the context of
   *      <code>txn</code>.
   */
  def bind(implicit txn: Txn): Ref.Bound[T] = new RefTxnAccessor(txn, this)

  /** Returns a view of this <code>Ref.Sink</code> that can be used to perform
   *  individual (non-transactional) reads and writes of the cell's value. The
   *  returned view acts as if each operation is performed in its own
   *  transaction. The returned instance is valid for the lifetime of the
   *  program.
   *  @return a view into the value of a <code>Ref</code>, that will perform
   *      each operation as if in its own transaction.
   */
  def nonTxn: Ref.Bound[T] = new RefNonTxnAccessor(this)

  override def toString = {
    "Ref@" + Integer.toHexString(hashCode)
  }
}

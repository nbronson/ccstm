/* $Id$
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

package ppl.stm


import java.lang.reflect.Field
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater

/** An object that provides a factory method for <code>TVar</code> instances.
 *  @see ppl.stm.TVar
 */
object TVar {
  
  /** Returns a new <code>TVar</code> instance with the specified initial
   *  value.
   */
  def apply[T](initialValue: T) = new TVar(initialValue)

  
  /** <code>Source</code> defines the covariant read-only interface of a
   *  <code>TVar</code> instance.
   */
  trait Source[+T] {
    /** Equivalent to <code>bind(txn).elem</code>.
     *  @see ppl.stm.TVar.Source#bind
     *  @see ppl.stm.TVar.BoundSource#elem
     */
    def elem(implicit txn: Txn): T = bind(txn).elem

    /** Equivalent to <code>bind(txn).elemMap(f)</code>.
     *  @see ppl.stm.TVar.Source#bind
     *  @see ppl.stm.TVar.BoundSource#elemMap
     */
    def elemMap[Z](f: T => Z)(implicit txn: Txn) = bind(txn).elemMap(f)

    /** Returns a view on this <code>TVar.Source</code> that can be used to read
     *  the cell's value as part of a transaction <code>txn</code>.  A transaction
     *  may be bound regardless of its state, but reads (and writes) are only
     *  allowed while a transaction is active.
     *  @param txn a transaction to be bound to the view.
     *  @return a read-only view onto the value of a <code>TVar</code> under the
     *      context of <code>txn</code>.
     */
    def bind(implicit txn: Txn): BoundSource[T]

    /** Returns a view of this <code>TVar.Source</code> that can be used to perform
     *  individual (non-transactional) reads of the cell's value.  The returned
     *  view acts as if each operation is performed in its own transaction.
     *  The returned view is valid for the lifetime of the program.
     *  @return a read-only view into the value of a <code>TVar</code> that will
     *      perform each read operation as if in its own transaction.
     */
    def nonTxn: BoundSource[T]

    // implicit access to unrecordedRead is omitted to discourage its use
  }

  /** <code>Sink</code> defines the contravariant write-only interface of a
   *  <code>TVar</code> instance.
   */
  trait Sink[-T] {
    /** Equivalent to <code>bind(txn).elem_=(v)</code>.
     *  <p>
     *  Note that Scala allows this method to be called without the underscore
     *  (as a property setter) only if the corresponding property getter is
     *  available from <code>TVar.Source</code>.
     *  @see ppl.stm.TVar.Sink#bind
     *  @see ppl.stm.TVar.BoundSink#elem_=
     */
    def elem_=(v: T)(implicit txn: Txn) { bind(txn).elem_=(v) }

    /** Returns a view on this <code>TVar.Source</code> that can be used to write
     *  the cell's value as part of a transaction <code>txn</code>.  A transaction
     *  may be bound regardless of its state, but writes (and reads) are only
     *  allowed while a transaction is active.
     *  @param txn a transaction to be bound to the view.
     *  @return a write-only view onto the value of a <code>TVar</code> under the
     *      context of <code>txn</code>.
     */
    def bind(implicit txn: Txn): BoundSink[T]

    /** Returns a view of this <code>TVar.Sink</code> that can be used to perform
     *  individual (non-transactional) writes of the cell's value.  The
     *  returned view acts as if each operation is performed in its own
     *  transaction. The returned view is valid for the lifetime of the
     *  program.
     *  @return a write-only view into the value of a <code>TVar</code>, that will
     *      perform each update operation as if in its own transaction.
     */
    def nonTxn: BoundSink[T]
  }

  /** <code>BoundSource</code> defines the covariant read-only view of a
   *  <code>TVar</code> bound to a particular context.  The context may be
   *  either a <code>Txn</code>, which guarantees that all reads will observe
   *  the same values as if they were executed at the transaction's commit
   *  (linearization) point, or the non-transactional context, which guarantees
   *  that each read will be linearized with all writes to the cell.
   *  @see ppl.stm.TVar
   */
  trait BoundSource[+T] {
    /** Returns <code>Some(txn)</code> if this view is bound to a transaction
     *  <code>txn</code>, <code>None</code> if it is bound to the non-transactional context.
     */
    def context: Option[Txn]

    /** Reads the element held by the bound <code>TVar</code>.  If this view
     *  was created by <code>TVar.bind(txn)</code> (or
     *  <code>TVar.Source.bind(txn)</code>) then this method will include the
     *  results of previous modifications of the <code>TVar</code> in
     *  <code>txn</code>, and will validate <code>txn</code> prior to
     *  returning.  If this view was created by <code>TVar.nonTxn</code> (or
     *  <code>TVar.Source.nonTxn</code>) the read will be strongly atomic and
     *  isolated with respect to all transactions.  This means that if a
     *  non-txn read observes a value stored by transaction <i>A</i>, any
     *  subsequent non-txn read on the same thread is guaranteed to observe all
     *  changes committed by <i>A</i>.
     *  @return the current value of the bound <code>TVar</code> as observed by
     *      the binding context.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     *  @see ppl.stm.TVar#bind
     *  @see ppl.stm.TVar#nonTxn
     */
    def elem: T

    /** Returns <code>f(elem)</code>, possibly reevaluating <code>f</code> to
     *  avoid rollback if a conflicting change is made to <code>elem</code> but
     *  both the old and new values are equal after application of
     *  <code>f</code>.  Requires that <code>f(x) == f(x)</code>.
     *  @param f an idempotent function.
     *  @return the result of applying <code>f</code> to the value read by this
     *      view.
     *  @see ppl.stm.TVar.BoundSource#elem
     */
    def elemMap[Z](f: T => Z): Z = {
      context match {
        case None => {
          // non-transactional, no semantic conflict detection
          f(elem)
        }
        case Some(txn) => {
          val u = unrecordedRead
          val result = f(u.value)
          if (!u.recorded) {
            val callback = new Txn.ReadSetCallback {
              var _latestRead = u

              def validate(txn: Txn) = {
                if (!_latestRead.stillValid) {
                  // reread, and see if that changes the result
                  _latestRead = unrecordedRead
                  val reapply = f(_latestRead.value)
                  if (result != reapply) {
                    // result changed
                    txn.markRollbackOnly
                  }
                }
              }
            }
            txn.addReadResource(callback)

            // protect against changes to the read that occurred between the call
            // to unrecordedRead and addReadOnlyResource()
            callback.validate(txn)
          }

          result
        }
      }
    }

    /** Returns an <code>UnrecordedRead</code> instance that wraps the value as
     *  that would be returned from <code>elem</code>, but if this source is
     *  bound to a transactional context does not add the <code>TVar</code> to
     *  the transaction's read set.  The caller is responsible for guaranteeing
     *  that the transaction's behavior is correct even if the
     *  <code>TVar</code> is changed before commit.  This method may be useful
     *  for heuristic decisions that can tolerate inconsistent or stale data,
     *  or for methods that register transaction handlers to perform validation
     *  at a semantic level.  When called from a non-transactional context the
     *  wrapping <code>UnrecordedRead</code> instance can be used to determine
     *  if any changes have been made to a <code>TVar</code>, which may be
     *  useful to avoid ABA problems.  Some STM implementations may spuriously
     *  indicate that a read is no longer valid.
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
     *  @see ppl.stm.TVar.BoundSource#elem
     */
    def unrecordedRead: UnrecordedRead[T]
  }

  /** <code>BoundSink</code> defines the contravariant write-only view of a
   *  <code>TVar</code> bound to a particular context. The context may be
   *  either a <code>Txn</code>, which guarantees that all writes will appear
   *  to other contexts as if they were executed atomically at the
   *  transaction's commit (linearization) point, or the non-transactional
   *  context, which guarantees that each write will be linearized (and a
   *  happens-before relationship established) with all other reads and writes
   *  to the cell.
   *  @see ppl.stm.TVar
   */
  trait BoundSink[-T] {
    /** Returns <code>Some(txn)</code> if this view is bound to a transaction
     *  <code>txn</code>, <code>None</code> if it is bound to the
     *  non-transactional context.
     */
    def context: Option[Txn]

    /** Updates the element held by the <code>TVar</code>.  If this view was
     *  created by <code>TVar.bind(txn)</code> (or
     *  <code>TVar.Sink.bind(txn)</code>) then the new value will not be
     *  visible to other contexts until (and unless) <code>txn</code>
     *  successfully commits.  If this view was created by
     *  <code>TVar.nonTxn</code> (or <code>TVar.Sink.nonTxn</code>) the value
     *  will be made available immediately, and a happens-before relationship
     *  will be established between this thread and any thread that reads the
     *  <code>v</code> stored by this method.
     *  <p>
     *  Note that Scala allows this method to be called without the underscore
     *  (as a property setter) only if the corresponding property getter is
     *  available from <code>TVar.Source</code>.
     *  @param v a value to store in the <code>TVar</code>.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     *  @see ppl.stm.TVar#bind
     *  @see ppl.stm.TVar#nonTxn
     */
    def elem_=(v: T)

    /** Updates the element held by the bound <code>TVar</code> without
     *  blocking and returns true, or does nothing and returns false.  For STM
     *  implementations that perform eager acquisition of write locks this
     *  method will return false if the bound <code>TVar</code> is already
     *  locked by a different (non-parent) transaction.  For STMs that perform
     *  lazy detection of write-write conflicts this method may operate in an
     *  identical fashion to <code>elem_=(v)</code>.  Regardless of the return
     *  value from this method the bound transaction (if any) will be
     *  validated.
     *  @param v a value to store in the <code>TVar</code>.
     *  @return true if the value was stored, false if nothing was done.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     *  @see ppl.stm.TVar#elem_=
     */
    def tryWrite(v: T): Boolean
  }

  /** A <code>TVar</code> view that supports reads and writes.  Reads and writes
   *  are performed from the perspective of the bound context, which is either
   *  a <code>Txn</code> or the non-transactional context.
   */
  trait Bound[T] extends BoundSource[T] with BoundSink[T] {
    /** Returns the same value as <code>elem</code>, but adds the
     *  <code>TVar</code> to the write set of the bound transaction context,
     *  if any.  Equivalent to <code>elem</code> when called from a
     *  non-transactional context.
     *  @return the current value of the bound <code>TVar</code> as observed by
     *      the binding context.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def readForWrite: T = {
      // a more efficient implementation may be possible
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
     *  @param before a value to compare against the current <code>TVar</code>
     *      contents.
     *  @param after a value to store in the <code>TVar</code> if <code>before</code>
     *      was equal to the previous cell contents.
     *  @return <code>true</code> if <code>before</code> was equal to the
     *      previous value of the bound <code>TVar</code>, <code>false</code>
     *      otherwise.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def compareAndSet(before: T, after: T): Boolean = {
      // a more efficient implementation may be possible
      transformIfDefined(new PartialFunction[T,T] {
        def isDefinedAt(v: T): Boolean = before == v
        def apply(v: T): T = after
      })
    }

    /** Works like <code>compareAndSet</code>, but allows spurious failures.  A
     *  false return from this method does not necessarily mean that the
     *  previous value of the <code>TVar</code> is not equal to
     *  <code>before</code>.
     *  @see ppl.stm.TVar.Bound#compareAndSet
     */
    def weakCompareAndSet(before: T, after: T): Boolean = {
      // a more efficient implementation may be possible
      compareAndSet(before, after)
    }

    /** Atomically replaces the value <i>v</i> stored in the <code>TVar</code> with
     *  <code>f</code>(<i>v</i>), possibly deferring execution of <code>f</code> or
     *  calling <code>f</code> multiple times.
     *  @param f a function that is safe to call multiple times, and safe to
     *      call later during the bound transaction (if any).
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def transform(f: T => T) {
      // a more efficient implementation may be possible
      transformIfDefined(new PartialFunction[T,T] {
        def isDefinedAt(v: T): Boolean = true
        def apply(v: T): T = f(v)
      })
    }

    /** Atomically replaces the value <i>v</i> stored in the <code>TVar</code>
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
     *      element held by this <code>TVar</code> on entry.
     *  @throws IllegalStateException if this view is bound to a transaction
     *      that is not active.
     */
    def transformIfDefined(pf: PartialFunction[T,T]): Boolean
  }

  private[TVar] val _dataUpdater = (new TVar[Any](null)).newDataUpdater 

  private trait Accessor[T] {
    val instance: TVar[T]
    def fieldIndex = 0
    def metadata = instance._metadata
    def metadata_=(v: STM.Metadata) { instance._metadata = v }
    def metadataCAS(before: STM.Metadata, after: STM.Metadata) = instance._metadataCAS(before, after)
    def data = instance._data
    def data_=(v: Any) { instance._data = v }
    def dataCAS(before: Any, after: Any) = TVar._dataUpdater.compareAndSet(instance, before, after)
  }
}

private class TVarNonTxnAccessor[T](val instance: TVar[T]) extends STM.NonTxnAccessor[T] with TVar.Accessor[T]

private class TVarTxnAccessor[T](val txn: Txn, val instance: TVar[T]) extends STM.TxnAccessor[T] with TVar.Accessor[T]

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
 *  A <code>TVar.Bound</code> view on the <code>TVar</code> may also be created
 *  that does not require the <code>Txn</code> to be available, and that
 *  reduces overheads if multiple accesses are performed to a <code>TVar</code>
 *  instance in the same transaction.
 *  <p>
 *  <code>TVar</code> allows subclassing to allow it to be opportunistically
 *  embedded in the internal node objects of collection classes, to reduce
 *  storage and indirection overheads.
 *  @see ppl.stm.Txn
 */
class TVar[T](initialValue: T) extends STM.MetadataHolder with TVar.Source[T] with TVar.Sink[T] {

  @volatile private[stm] var _data: Any = initialValue

  private[TVar] def newDataUpdater = {
    AtomicReferenceFieldUpdater.newUpdater(classOf[TVar], classOf[Object], "_data")
  }

  // implicit access to readForWrite is omitted to discourage its use 

  /** Equivalent to <code>bind(txn).compareAndSet(before, after)</code>.
   *  @see ppl.stm.TVar#bind
   *  @see ppl.stm.TVar.Bound#compareAndSet
   */
  def compareAndSet(before: T, after: T)(implicit txn: Txn) = bind(txn).compareAndSet(before, after)

  /** Equivalent to <code>bind(txn).transform(f)</code>.
   *  @see ppl.stm.TVar#bind
   *  @see ppl.stm.TVar.Bound#transform
   */
  def transform(f: T => T)(implicit txn: Txn) { bind(txn).transform(f) }

  /** Equivalent to <code>bind(txn).transformIfDefined(pf)</code>.
   *  @see ppl.stm.TVar#bind
   *  @see ppl.stm.TVar.Bound#testAndTransform
   */
  def transformIfDefined(pf: PartialFunction[T,T])(implicit txn: Txn) = bind(txn).transformIfDefined(pf)

  /** Returns a view on this <code>TVar.Source</code> that can be used to read
   *  or write the cell's value as part of the transaction <code>txn</code>.  A
   *  transaction may be bound regardless of its state, but reads and writes
   *  are only allowed while a transaction is active.
   *  @param txn the transaction to be bound.
   *  @return a view onto the value of a <code>TVar</code> under the context of
   *      <code>txn</code>.
   */
  def bind(implicit txn: Txn): TVar.Bound[T] = new TVarTxnAccessor(txn, this)

  /** Returns a view of this <code>TVar.Sink</code> that can be used to perform
   *  individual (non-transactional) reads and writes of the cell's value. The
   *  returned view acts as if each operation is performed in its own
   *  transaction. The returned instance is valid for the lifetime of the
   *  program.
   *  @return a view into the value of a <code>TVar</code>, that will perform
   *      each operation as if in its own transaction.
   */
  def nonTxn: TVar.Bound[T] = new TVarNonTxnAccessor(this)
}

/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// Txn.scala

package edu.stanford.ppl.ccstm


private[ccstm] abstract class AbstractTxn extends impl.StatusHolder {
  import Txn._

  //////////////// Functions to be implemented in an STM-specific manner

  private[ccstm] def requestRollbackImpl(cause: RollbackCause): Boolean
  private[ccstm] def retryImpl(): Nothing
  private[ccstm] def commitImpl(): Status
  private[ccstm] def explicitlyValidateReadsImpl()
  private[ccstm] def addReferenceImpl(ptr: AnyRef)

  //////////////// Functions implemented in Txn, but available to the STM-specific base class

  /** Returns the transaction's current status.  The status may change due to
   *  the actions of a concurrent thread.
   */
  def status: Status

  /** Causes this transaction to fail with the specified cause.  If the
   *  transaction is already doomed (`status.mustRollBack`) then
   *  this method does nothing.  Throws an exception if it cannot be arranged
   *  that this transaction roll back.
   *  <p>
   *  This method does not throw `RollbackError`, but in most places
   *  where it would be called it is probably correct to immediately do this.
   *  This method may only be called from the thread executing the transaction
   *  (probably not checked); use `requestRollback` if you wish to
   *  doom a transaction running on another thread.
   *  @throws IllegalStateException if `status.mustCommit`.
   *  @see edu.stanford.ppl.ccstm.Txn#requestRollback
   */
  def forceRollback(cause: RollbackCause)

  /** Attempts to doom the transaction, returning true if the transaction will
   *  definitely roll back, false if the transaction will definitely commit.
   *  This method may return true if rollback occurs for a reason other than
   *  `cause`.  Unlike `forceRollback(cause)`, this
   *  method may be called from any thread, and never throws an exception.
   *  @see edu.stanford.ppl.ccstm.Txn#forceRollback
   */
  def requestRollback(cause: RollbackCause): Boolean

  /** Rolls the transaction back, indicating that it should be retried after
   *  one or more of the values read during the transaction have changed.
   *  @throws IllegalStateException if the transaction is not active.
   *  @see edu.stanford.ppl.ccstm.Atomic#orElse
   */
  def retry()

  /** Completes the transaction, committing if possible.  Returns the final
   *  status.
   */
  def commit(): Status

  /** Validates that the transaction is consistent with all other committed
   *  transactions and completed non-transactional accesses, immediately
   *  rolling the transaction back if that is not the case (by throwing an
   *  `InvalidReadError`).
   *  <p>
   *  CCSTM guarantees that all reads will be consistent with those from a
   *  point-in-time snapshot, even without any manual validation, so use of
   *  this method by user code should be rare.  CCSTM always provides the
   *  consistency guarantee referred to as "opacity".
   */
  def explicitlyValidateReads()

  /** Creates a strong reference between this `Txn` and
   *  `ref` until the transaction has completed.
   */
  def addReference(ptr: AnyRef)

  /** Arranges for `callback` to be executed as late as possible
   *  while the transaction is still active.  If the transaction rolls back
   *  then the callback may not be invoked.  If the callback throws an
   *  exception then the transaction will be rolled back and no subsequent
   *  before-completion callbacks will be invoked.  If two callbacks have
   *  different priorities then the one with the smaller priority will be
   *  invoked first, otherwise the one enqueued earlier will be invoked
   *  first.
   *  @throws IllegalStateException if this transaction is not active.
   */
  def beforeCommit(callback: Txn => Unit, prio: Int)

  /** Enqueues a before-commit callback with the default priority of 0. */
  def beforeCommit(callback: Txn => Unit)

  /** Adds a read resource to the transaction, which will participate in
   *  validation, and then immediately calls
   *  `readResource.valid`.  This register/validate pair
   *  corresponds to the most common use case for read resources.  If the
   *  caller takes responsibility for validating the read resource then they
   *  may use the three-argument form of this method.  If the validation fails
   *  then the transaction will be immediately rolled back and this method will
   *  not return.  If two read resources have different priorities then the one
   *  with the smaller priority will be validated first, otherwise the one
   *  enqueued earlier will be validated first.
   *  @throws IllegalStateException if this transaction is not active.
   */
  def addReadResource(readResource: ReadResource, prio: Int)

  /** Adds a read resource with the default priority of 0. */
  def addReadResource(readResource: ReadResource)

  /** Adds a read resource to the transaction like the two-argument form of
   *  this method, but skips the subsequent call to
   *  `readResource.valid` if `checkAfterRegister` is
   *  false.
   *  @throws IllegalStateException if this transaction is not active.
   */
  def addReadResource(readResource: ReadResource, prio: Int, checkAfterRegister: Boolean)

  /** Adds a write resource to the transaction, which will participate in the
   *  two-phase commit protocol.  If two write resources have different
   *  priorities then the one with the smaller priority will be invoked first,
   *  otherwise the one enqueued earlier will be invoked first.
   *  @throws IllegalStateException if this transaction is not active.
   */
  def addWriteResource(writeResource: WriteResource, prio: Int)

  /** Adds a write resource with the default priority of 100. */
  def addWriteResource(writeResource: WriteResource)

  /** Arranges for `callback` to be executed after transaction
   *  completion, if this transaction commits.  An exception thrown from an
   *  after-commit callback will be rethrown after the rest of the after-commit
   *  callbacks are invoked.  If two callbacks have different priorities then
   *  the one with the smaller priority will be invoked first, otherwise the
   *  one enqueued earlier will be invoked first.
   *  @throws IllegalStateException if the transaction is already completed.
   */
  def afterCommit(callback: Txn => Unit, prio: Int)

  /** Enqueues an after-commit callback with the default priority of 0. */
  def afterCommit(callback: Txn => Unit)

  /** Arranges for `callback` to be executed after transaction
   *  completion, if this transaction rolls back.  An exception thrown from an
   *  after-commit callback will be rethrown after the rest of the after-commit
   *  callbacks are invoked.  If two callbacks have different priorities then
   *  the one with the smaller priority will be invoked first, otherwise the
   *  one enqueued earlier will be invoked first.
   *  @throws IllegalStateException if the transaction is already completed.
   */
  def afterRollback(callback: Txn => Unit, prio: Int)

  /** Enqueues an after-rollback callback with the default priority of 0. */
  def afterRollback(callback: Txn => Unit)

  /** Arranges for `callback` to be executed after transaction
   *  completion, regardless of whether the transaction rolls back or commits.
   *  Equivalent to passing `callback` to both
   *  `afterCommit` and `afterRollback`.
   *  @throws IllegalStateException if the transaction is already completed.
   */
  def afterCompletion(callback: Txn => Unit, prio: Int)

  /** Enqueues an after-completionk callback with the default priority of 0. */
  def afterCompletion(callback: Txn => Unit)

  /** Detaches this transaction from the current thread.  When combined with
   *  `attach()` this can be used to migrate a `Txn` from one thread to another
   *  while it is active.  A transaction may not be used from multiple threads
   *  at the same time.  `attach()` must be called before the transaction is
   *  used on the other thread, although this may or may not be checked. There
   *  is no need to attach or detach at the normal beginning and end of a
   *  transaction, this pair of functions is used only for advanced scenarios
   *  in which transaction processing needs to be migrated from one thread to
   *  another.  With extreme care this may be used to suspend a transaction on
   *  the current thread to run another one, but if there are any conflicts
   *  between the transactions this can easily lead to deadlock.
   */
  def detach();

  /** Attaches this transaction to the current thread after a called to
   *  `detach()`.                                                                         
   */
  def attach();

  //////////////// internal Txn lifecycle stuff

  /** Calls `commit`, then throws the exception that caused rollback
   *  if the cause was either `UserExceptionCause` or
   *  `CallbackExceptionCause`.
   */
  private[ccstm] def commitAndRethrow()


  private[ccstm] var _callbacks: impl.Callbacks

  /** Calls `ReadResource.valid(this)` for all read resources,
   *  unless rollback is required.  Returns true if commit is still possible.
   *  Captures and handles exceptions.
   */
  private[ccstm] def readResourcesValidate(): Boolean

  /** Returns true if there are actual write resources (as opposed to
   *  write-like resources), false otherwise.
   */
  private[ccstm] def writeResourcesPresent: Boolean

  /** Calls `WriteResource.prepare(this)` for all write resources,
   *  unless rollback is required.  Returns true if commit is still possible.
   *  Captures and handles exceptions.  This also has the effect of invoking
   *  all of the callbacks registered with `beforeCommit`.
   */
  private[ccstm] def writeLikeResourcesPrepare(): Boolean

  /** Calls `WriteResource.performCommit(this)`, handling
   *  exceptions.
   */
  private[ccstm] def writeResourcesPerformCommit()

  /** Calls `WriteResource.performRollback(this)`, handling
   *  exceptions.
   */
  private[ccstm] def writeResourcesPerformRollback()

  /** Calls the handlers registered with either `afterCommit` or
   *  `afterRollback`, as appropriate, handling any exceptions.
   *  Also adjusts the commit and rollback counters.
   */
  private[ccstm] def callAfter()

  private[ccstm] def attach(ctx: impl.ThreadContext)
  private[ccstm] def detach(ctx: impl.ThreadContext)
}

/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// Txn.scala

package edu.stanford.ppl.ccstm


private[ccstm] abstract class AbstractTxn extends impl.StatusHolder {
  import Txn._

  //////////////// Functions to be implemented in an STM-specific manner

  private[ccstm] def forceRollbackImpl(invalidNestingLevel: Int, cause: RollbackCause): Nothing
  private[ccstm] def requestRollbackImpl(cause: RollbackCause): Status
  private[ccstm] def topLevelComplete(): Status
  private[ccstm] def takeRetrySet(): impl.ReadSet
  private[ccstm] def nestedBegin()
  private[ccstm] def nestedComplete(): RollbackCause
  private[ccstm] def explicitlyValidateReadsImpl()
  private[ccstm] def addReferenceImpl(ptr: AnyRef)

  //////////////// Functions implemented in Txn, but available to the STM-specific base class

  /** Returns the transaction's current status.  The status may change due to
   *  the actions of a concurrent thread.
   */
  def status: Status

  /** Returns the number of nested atomic blocks in this `Txn`.  A value of 0
   *  indicates a top-level atomic block.
   */
  def nestingLevel: Int

  /** Causes the specified `invalidNestingLevel` to be rolled back due to the
   *  specified `cause`.  If `invalidNestingLevel` is 0 then the entire
   *  transaction is guaranteed to be rolled back, otherwise either a a
   *  partial or complete rollback may occur.  After a partial rollback
   *  `nestingLevel` will be less than `invalidNestingLevel`.
   *
   *  To roll back just the current nesting level of `txn`, use {{{
   *    txn.forceRollback(txn.nestingLevel, cause)
   *  }}}
   *  To roll back the entire transaction `txn`, use {{{
   *    txn.forceRollback(0, cause)
   *  }}}
   *
   *  If the invalid nesting level is already doomed then this method does not
   *  change the cause.  Throws an `IllegalStateException` if the transaction
   *  is already committed.  This method may only be called by the thread
   *  executing the transaction; use `requestRollback` if you wish to doom a
   *  transaction running on another thread.
   *  @throws IllegalStateException if `status` is `Committed` or if called
   *      from a thread that is not attached to the transaction.
   *  @throws IllegalArgumentException if `invalidNestinglevel` is less than
   *      zero.
   */
  def forceRollback(invalidNestingLevel: Int, cause: RollbackCause): Nothing

  /** If the transaction is either `Active` or `Preparing`, marks it for
   *  rollback, otherwise it does not affect the transaction.  Returns the
   *  transaction status after the attempt.  The returned status will be one
   *  of `Prepared`, `Committed`, or `RolledBack`.  Regardless of the status,
   *  this method does not throw an exception.
   *
   *  Unlike `forceRollback`, this method may be called from any thread.  Note
   *  that there is no facility for remotely triggering a partial rollback.
   */
  def requestRollback(cause: RollbackCause): Status

  /** Rolls the transaction back, indicating that it should be retried after
   *  one or more of the values read during the transaction have changed.
   *  @throws IllegalStateException if the transaction is not active.
   *  @see edu.stanford.ppl.ccstm.atomic#oneOf
   */
  def retry()

  /** Validates that the transaction is consistent with all other committed
   *  transactions and completed non-transactional accesses, immediately
   *  rolling the transaction back if that is not the case (by throwing an
   *  `InvalidReadError`).
   *
   *  CCSTM always guarantees that all reads are consistent with those from a
   *  single snapshot, even without any calls to this method.  This method
   *  provides the additional guarantee that the snapshot can be advanced to
   *  the point in time at which this method was called.  CCSTM always
   *  provides the consistency guarantee referred to as "opacity".
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
   *  before-commit callbacks will be invoked.  If two callbacks have
   *  different priorities then the one with the smaller priority will be
   *  invoked first, otherwise the one enqueued earlier will be invoked
   *  first.
   *
   *  It is okay to call `beforeCommit` from inside a before-commit handler.
   *  If the reentrantly-added handler has a lower priority than the current
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
   *  this method, but skips the subsequent call to `readResource.valid` if
   * `checkAfterRegister` is false.
   *  @throws IllegalStateException if this transaction is not active.
   */
  def addReadResource(readResource: ReadResource, prio: Int, checkAfterRegister: Boolean)

  /** Adds a write resource to the transaction, which will participate in the
   *  two-phase commit protocol.  If two write resources have different
   *  priorities then the one with the smaller priority will be invoked first,
   *  otherwise the one enqueued earlier will be invoked first.
   *  @throws IllegalStateException if this transaction is not active or
   *      validating.
   */
  def addWriteResource(writeResource: WriteResource, prio: Int)

  /** Adds a write resource with the default priority of 0. */
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
   *
   *  If a detached transaction has performed a write that conflicts with
   *  another transaction of higher priority, it will be marked for rollback
   *  despite being detached.
   * 
   *  @throws IllegalStateException if the transaction is not active.
   */
  def detach()

  /** Attaches this transaction to the current thread after a called to
   *  `detach()`.                                                                         
   */
  def attach()

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

  /** Calls the handlers registered with `beforeCommit`.  Returns true if
   *  commit is still possible.
   */
  private[ccstm] def callBefore(): Boolean

  /** Returns true if there are actual write resources (as opposed to
   *  write-like resources), false otherwise.
   */
  private[ccstm] def writeResourcesPresent: Boolean

  /** Calls `WriteResource.prepare(this)` for all write resources,
   *  unless rollback is required.  Returns true if commit is still possible.
   *  Captures and handles exceptions.
   */
  private[ccstm] def writeResourcesPrepare(): Boolean

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
  private[ccstm] def callAfter(readSetSize: Int, writeBufferSize: Int)

  private[ccstm] def attach(ctx: impl.ThreadContext)
  private[ccstm] def detach(ctx: impl.ThreadContext)
}

/* CCSTM - (c) 2009 Stanford University - PPL */

// Txn.scala

package edu.stanford.ppl.ccstm


private[ccstm] abstract class AbstractTxn extends impl.StatusHolder {
  import Txn._

  //////////////// Functions to be implemented in an STM-specific manner

  private[ccstm] def requestRollbackImpl(cause: RollbackCause): Boolean
  private[ccstm] def retryImpl()
  private[ccstm] def commitImpl(): Status
  private[ccstm] def explicitlyValidateReadsImpl()

  //////////////// Functions implemented in Txn, but available to the STM-specific base class

  /** Returns the transaction's current status.  The status may change at any
   *  time.
   */
  def status: Status

  /** Causes this transaction to fail with the specified cause.  If the
   *  transaction is already doomed (<code>status.mustRollBack</code>) then
   *  this method does nothing.  Throws an exception if it cannot be arranged
   *  that this transaction roll back.
   *  <p>
   *  This method does not throw <code>RollbackError</code>, but in most places
   *  where it would be called it is probably correct to immediately do this.
   *  This method may only be called from the thread executing the transaction
   *  (probably not checked); use <code>requestRollback</code> if you wish to
   *  doom a transaction running on another thread.
   *  @throws IllegalStateException if <code>status.mustCommit</code>.
   *  @see edu.stanford.ppl.ccstm.Txn#requestRollback
   */
  def forceRollback(cause: RollbackCause)

  /** Attempts to doom the transaction, returning true if the transaction will
   *  definitely roll back, false if the transaction will definitely commit.
   *  This method may return true if rollback occurs for a reason other than
   *  <code>cause</code>.  Unlike <code>forceRollback(cause)</code>, this
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

  /** Calls <code>commit</code>, then throws the exception that caused rollback
   *  if the cause was either <code>UserExceptionCause</code> or
   *  <code>CallbackExceptionCause</code>.
   */
  private[ccstm] def commitAndRethrow()

  /** Validates that the transaction is consistent with all other committed
   *  transactions and completed non-transactional accesses, immediately
   *  rolling the transaction back if that is not the case (by throwing an
   *  <code>InvalidReadError</code>).
   *  <p>
   *  CCSTM guarantees that all reads will be consistent with those from a
   *  point-in-time snapshot, even without any manual validation, so use of
   *  this method by user code should be rare.  The provided consistency
   *  guarantee is referred to as "opacity".
   */
  def explicitlyValidateReads()

  /** Arranges for <code>callback</code> to be executed as late as possible
   *  while the transaction is still active.  If the transaction rolls back
   *  then the callback may not be invoked.  If the callback throws an
   *  exception then the transaction will be rolled back and no subsequent
   *  before-completion callbacks will be invoked.  If two callbacks have
   *  different priorities then the one with the smaller priority will be
   *  executed first.
   *  @throws IllegalStateException if this transaction is not active.
   */
  def beforeCommit(callback: Txn => Unit, prio: Int)

  /** Enqueues a before-commit callback with the default priority of 0. */
  def beforeCommit(callback: Txn => Unit)

  /** Adds a read resource to the transaction, which will participate in
   *  validation, and then immediately calls
   *  <code>readResource.valid</code>.  This register/validate pair
   *  corresponds to the most common use case for read resources.  If the
   *  caller takes responsibility for validating the read resource then they
   *  may use the two-argument form of this method.  If the validation fails
   *  then the transaction will be immediately rolled back and this method will
   *  not return.  If two read resources have different priorities then the one
   *  with the smaller priority will be validated first.
   *  @throws IllegalStateException if this transaction is not active.
   */
  def addReadResource(readResource: ReadResource, prio: Int)

  /** Adds a read resource with the default priority of 0. */
  def addReadResource(readResource: ReadResource)

  /** Adds a read resource to the transaction like the two-argument form of
   *  this method, but skips the subsequent call to
   *  <code>readResource.valid</code> if <code>checkAfterRegister</code> is
   *  false.
   *  @throws IllegalStateException if this transaction is not active.
   */
  def addReadResource(readResource: ReadResource, prio: Int, checkAfterRegister: Boolean)

  /** Calls <code>ReadResource.valid(this)</code> for all read resources,
   *  unless rollback is required.  Returns true if commit is still possible.
   *  Captures and handles exceptions.
   */
  private[ccstm] def readResourcesValidate(): Boolean

  /** Adds a write resource to the transaction, which will participate in the
   *  two-phase commit protocol.  If two write resources have different
   *  priorities then the one with the smaller priority will be validated first.
   *  @throws IllegalStateException if this transaction is not active.
   */
  def addWriteResource(writeResource: WriteResource, prio: Int)

  /** Adds a write resource with the default priority of <em>100</em>. */
  def addWriteResource(writeResource: WriteResource)

  /** Returns true if there are actual write resources (as opposed to
   *  write-like resources), false otherwise.
   */
  private[ccstm] def writeResourcesPresent: Boolean

  /** Calls <code>WriteResource.prepare(this)</code> for all write resources,
   *  unless rollback is required.  Returns true if commit is still possible.
   *  Captures and handles exceptions.  This also has the effect of invoking
   *  all of the callbacks registered with <code>beforeCommit</code>.
   */
  private[ccstm] def writeLikeResourcesPrepare(): Boolean

  /** Calls <code>WriteResource.performCommit(this)</code>, handling
   *  exceptions.
   */
  private[ccstm] def writeResourcesPerformCommit()

  /** Calls <code>WriteResource.performRollback(this)</code>, handling
   *  exceptions.
   */
  private[ccstm] def writeResourcesPerformRollback()

  /** Arranges for <code>callback</code> to be executed after transaction
   *  completion, if this transaction commits.  An exception thrown from an
   *  after-commit callback will be rethrown after the rest of the after-commit
   *  callbacks are invoked.  If two callbacks have different priorities then
   *  the one with the smaller priority will be executed first.
   *  @throws IllegalStateException if the transaction is already completed.
   */
  def afterCommit(callback: Txn => Unit, prio: Int)

  /** Enqueues an after-commit callback with the default priority of 0. */
  def afterCommit(callback: Txn => Unit)

  /** Arranges for <code>callback</code> to be executed after transaction
   *  completion, if this transaction rolls back.  An exception thrown from an
   *  after-commit callback will be rethrown after the rest of the after-commit
   *  callbacks are invoked.  If two callbacks have different priorities then
   *  the one with the smaller priority will be executed first.
   *  @throws IllegalStateException if the transaction is already completed.
   */
  def afterRollback(callback: Txn => Unit, prio: Int)

  /** Enqueues an after-rollback callback with the default priority of 0. */
  def afterRollback(callback: Txn => Unit)

  /** Arranges for <code>callback</code> to be executed after transaction
   *  completion, regardless of whether the transaction rolls back or commits.
   *  Equivalent to passing <code>callback</code> to both
   *  <code>afterCommit</code> and <code>afterRollback</code>.
   *  @throws IllegalStateException if the transaction is already completed.
   */
  def afterCompletion(callback: Txn => Unit, prio: Int)

  /** Enqueues an after-completionk callback with the default priority of 0. */
  def afterCompletion(callback: Txn => Unit)

  /** Calls the handlers registered with either <code>afterCommit</code> or
   *  <code>afterRollback</code>, as appropriate, handling any exceptions.
   *  Also adjusts the commit and rollback counters.
   */
  private[ccstm] def callAfter()
}

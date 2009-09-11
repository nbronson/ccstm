/* CCSTM - (c) 2009 Stanford University - PPL */

// Txn.scala

package edu.stanford.ppl.ccstm


object Txn {

  //////////////// Status

  /** Represents the current status of a <code>Txn</code>. */
  sealed abstract class Status {
    /** True if a transaction with this status might eventually commit, false
     *  if rollback is inevitable.
     */
    def mightCommit: Boolean

    /** True if a transaction with this status has decided on commit, false if
     *  rollback may still occur.
     */
    def mustCommit: Boolean

    /** True if a transaction with this status might roll back, false if commit
     *  has already been decided.  Equivalent to <code>!willCommit</code>.
     */
    def mightRollBack = !mustCommit
    
    /** True if a transaction with this status definitely will not commit,
     *  false if commit is possible.  Equivalent to <code>!mightCommit</code>.
     */
    def mustRollBack = !mightCommit

    /** True if for a transaction with this status, all user code and callbacks
     *  have been executed.
     */
    def completed: Boolean
  }

  /** The <code>Status</code> for a <code>Txn</code> that may commit, but for
   *  which completion has not yet been requested.
   */
  case object Active extends Status {
    def mightCommit = true
    def mustCommit = false
    def completed = false
  }

  /** The <code>Status</code> for a <code>Txn</code> that will not commit, but
   *  for which completion has not yet been requested.
   *  @see edu.stanford.ppl.ccstm.AbstractTxn#failure
   */
  case object MarkedRollback extends Status {
    def mightCommit = false
    def mustCommit = false
    def completed = false
  }

  /** The <code>Status</code> for a <code>Txn</code> that may commit and whose
   *  completion has been requested, but that has not yet acquired the
   *  resources required to guarantee that commit is possible.
   */
  case object Preparing extends Status {
    def mightCommit = true
    def mustCommit = false
    def completed = false
  }

  /** The <code>Status</code> for a <code>Txn</code> that is guaranteed to
   *  commit, but that has not yet released all of its resources.
   */
  case object Committing extends Status {
    def mightCommit = true
    def mustCommit = true
    def completed = false
  }

  /** The <code>Status</code> for a <code>Txn</code> that has successfully
   *  committed, applying all of its changes.
   */
  case object Committed extends Status {
    def mightCommit = true
    def mustCommit = true
    def completed = true
  }

  /** The <code>Status</code> for a <code>Txn</code> that will definitely roll
   *  back, but that has not yet released all of its resources.
   *  @see edu.stanford.ppl.ccstm.AbstractTxn#failure
   */
  case object RollingBack extends Status {
    def mightCommit = false
    def mustCommit = false
    def completed = false
  }

  /** The <code>Status</code> for a <code>Txn</code> that has been completely
   *  rolled back.
   *  @see edu.stanford.ppl.ccstm.AbstractTxn#failure
   */
  case object Rolledback extends Status {
    def mightCommit = false
    def mustCommit = false
    def completed = true
  }


  /** A reusable (stackless) exception used to initiate rollback when the user
   *  has performed an explicit <code>Txn.retry</code>.  Rather than
   *  propagating this instance, user code should either rethrow or avoid
   *  catching all instances of <code>RollbackError</code>.
   */
  object ExplicitRetryError extends RollbackError with Stackless

  /** The base class of all exceptions that indicate that a transaction was
   *  rolled back because of a failure of optimistic concurrency control.
   */
  abstract class OptimisticFailureError(invalidObj0: AnyRef, extraInfo0: Any) extends RollbackError {
    /** Returns the object for which the value read is no longer current. */
    val invalidObject = invalidObj0

    /** Returns additional information about what portion of <code>txnalObj</code> */
    val extraInfo = extraInfo0
  }

  /** An exception that indicates that a transaction was rolled back because a
   *  previous read is no longer valid.
   */
  class InvalidReadError(invalidObj0: AnyRef, extraInfo0: Any) extends OptimisticFailureError(invalidObj0, extraInfo0)

  /** An exception that indicates that a transaction was rolled back because it
   *  needed write access to an object also being written by another
   *  transaction.  The transaction that is rolled back with this exception may
   *  have been the first or second to request write access; the contention
   *  management policy may choose which transaction to roll back.
   */
  class WriteConflictError(invalidObj0: AnyRef, extraInfo0: Any) extends OptimisticFailureError(invalidObj0, extraInfo0)

  //////////////// Resources participate in a two-phase commit

  /** <code>ReadResource</code>s are checked each time that the virtual
   *  snapshot from which the transaction has performed its reads must be moved
   *  into the future.  Each transaction is associated with a read version,
   *  which defines a "virtual snapshot".  When a value is encountered that may
   *  have been changed since the virtual snapshot was taken (since the read
   *  version was assigned), the read version is advanced and
   *  <code>ReadResource.validate</code> is invoked for all read resources.
   *  <p>
   *  Both read-only and updating transactions may be able to commit without
   *  advancing their virtual snapshot, in which case they won't invoke
   *  <code>validate</code>.  To help avoid race conditions, the most
   *  straightforward mechanism for adding a read resource will automatically
   *  invoke <code>validate</code>.
   *  @see edu.stanford.ppl.ccstm.AbstractTxn#addReadResource
   */
  trait ReadResource {
    /** Called during the <code>Active</code> and/or <code>Prepared</code>
     *  states, returns true iff <code>txn</code> is still valid.  Validation
     *  during the <code>Preparing</code> state may be skipped if no other
     *  transactions have committed since the last validation, or if no
     *  <code>WriteResource</code>s have been registered.
     *  @return true if <code>txn</code> is still valid, false if
     *      <code>txn</code> should be rolled back immediately.
     */
    def validate(txn: Txn): Boolean    
  }

  /** <code>WriteResource</code>s participate in a two-phase commit.  Each
   *  write resource is given the opportunity to veto commit, and each write
   *  resource will be informed of the decision.  Unlike read resources, write
   *  resources are not consulted during advancement of the read version (and
   *  the associated virtual snapshot).  If an update resource needs to be
   *  revalidated when the read version is advanced it should also register a
   *  <code>ReadResource</code>.
   *  @see edu.stanford.ppl.ccstm.AbstractTxn#addWriteResource
   */
  trait WriteResource {
    /** Called during the <code>Preparing</code> state, returns true if this
     *  resource agrees to commit.  All locks or other resources required to
     *  complete the commit must be acquired during this callback, or else
     *  this method must return false.  The consensus decision will be
     *  delivered via a subsequent call to <code>performCommit</code> or
     *  <code>performRollback</code>.
     *  @return true if this resource can definitely succeed, false if
     *      <code>txn</code> must be rolled back. 
     */
    def prepare(txn: Txn): Boolean

    /** Called during the <code>Committing</code> state. */
    def performCommit(txn: Txn)

    /** Called during the <code>RollingBack</code> state. */
    def performRollback(txn: Txn)
  }

  private[ccstm] val EmptyPrioCallbackMap = Map.empty[Int,List[Txn => Unit]]
}

/** @see edu.stanford.ppl.ccstm.AbstractTxn
 */
sealed class Txn(failureHistory0: List[Throwable]) extends STM.TxnImpl(failureHistory0)

abstract class AbstractTxn(private[ccstm] val failureHistory: List[Throwable]) {
  this: Txn =>

  import Txn._

  private var _beforeCommit = EmptyPrioCallbackMap
  private var _readResources: List[ReadResource] = Nil
  private var _writeResources: List[WriteResource] = Nil
  private var _afterCommit = EmptyPrioCallbackMap
  private var _afterRollback = EmptyPrioCallbackMap

  /** Returns the transaction's current status, as of the most recent
   *  operation.
   */
  def status: Status

  /** If <code>status.mustRollBack</code> is true, then this returns the
   *  exception that is responsible for the rollback.
   *  @see edu.stanford.ppl.ccstm.RollbackError
   *  @see edu.stanford.ppl.ccstm.AbstractSTM.ExplicitRetryError
   *  @see edu.stanford.ppl.ccstm.AbstractSTM.InvalidReadError
   *  @see edu.stanford.ppl.ccstm.AbstractSTM.WriteConflictError
   */
  def failure: Throwable

  // TODO: should this be public?
  /** Forces this transaction to roll back, with the specified cause. */
  private[ccstm] def failure_=(v: Throwable)

  /** Completes the transaction, committing if possible.  Returns the final
   *  status.
   */
  private[ccstm] def commit(): Status

  /** Calls <code>commit</code>, then rethrows <code>failure</code> if the
   *  transaction rolled back and should not be retried.
   */
  private[ccstm] def commitAndRethrow() {
    if (commit() == Rolledback && !failure.isInstanceOf[RollbackError]) throw failure
  }

  /** Throws the <code>failure</code> exception if the transaction is marked
   *  for rollback, otherwise throws an <code>IllegalStateException</code> if
   *  the transaction is not active.
   *  @see edu.stanford.ppl.ccstm.Txn.Active
   *  @see edu.stanford.ppl.ccstm.Txn.MarkedRollback
   */
  private[ccstm] def requireActive {
    val s = status
    if (s != Active) {
      if (s == MarkedRollback) {
        throw failure
      } else {
        throw new IllegalStateException("txn.status is " + s)
      }
    }
  }

  /** A convenience function, equivalent to <code>status.completed</code>.
   *  @see edu.stanford.ppl.ccstm.Txn.Status#completed 
   */
  def completed: Boolean = status.completed

  /** A convenience function, equivalent to <code>status.mustCommit</code>.
   *  @see edu.stanford.ppl.ccstm.Txn.Status#mustCommit
   */
  def mustCommit: Boolean = status.mustCommit

  /** A convenience function, equivalent to <code>status == Committed</code>.
   */
  def committed: Boolean = status == Committed

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
  def beforeCommit(callback: Txn => Unit, prio: Int) {
    requireActive
    _beforeCommit = _beforeCommit.update(prio, callback :: _beforeCommit.getOrElse(prio, Nil))
  }

  /** Calls all callbacks registered via <code>beforeCommit</code>. */
  private[ccstm] def callBeforeCommit {
    if (!_beforeCommit.isEmpty) {
      // no need to catch exceptions, because if there is an exception we want
      // to break out of the loop anyway
      for ((_,cbs) <- _beforeCommit; cb <- cbs) cb(this)
    }
  }

  /** Enqueues a before-commit callback with the default priority of 0.
   *  @see edu.stanford.ppl.ccstm.AbstractTxn#beforeCommit(Function1[Txn,Unit],Int)
   */
  def beforeCommit(callback: Txn => Unit) { beforeCommit(callback, 0) }

  /** Adds a read resource to the transaction, which will participate in
   *  validation, and then immediately calls
   *  <code>readResource.validate</code>.  This register/validate pair
   *  corresponds to the most common use case for read resources.  If the
   *  caller takes responsibility for validating the read resource then they
   *  may use the two-argument form of this method.  If the validation fails
   *  then the transaction will be immediately rolled back and this method will
   *  not return.
   *  @throws IllegalStateException if this transaction is not active.
   */
  def addReadResource(readResource: ReadResource) {
    addReadResource(readResource, true)
  }

  /** Adds a read resource to the transaction like the single argument form of
   *  this method, but skips the subsequent call to
   *  <code>readResource.validate</code> if <code>checkAfterRegister</code> is
   *  false.
   *  @throws IllegalStateException if this transaction is not active.
   *  @see edu.stanford.ppl.ccstm.AbstractTxn#addReadResource(edu.stanford.ppl.ccstm.Txn.ReadResource)
   */
  def addReadResource(readResource: ReadResource, checkAfterRegister: Boolean) {
    requireActive
    _readResources = readResource :: _readResources
    if (checkAfterRegister) {
      try {
        if (!readResource.validate(this)) {

        }
      } catch {
        case x =>
      }
    }
  }

  /** Calls <code>ReadResource.validate(this)</code> until a resource that
   *  fails validation is found, returning false, or returns true if all read
   *  resources are valid.  No attempt is made to capture exceptions.
   */
  private[ccstm] def resourceValidate: Boolean = {
    _readResources.isEmpty || !_readResources.exists(r => !r.validate(this))
  }

  /** Adds a write resource to the transaction, which will participate in the
   *  two-phase commit protocol.
   *  @throws IllegalStateException if this transaction is not active.
   */
  def addWriteResource(writeResource: WriteResource) {
    requireActive
    _writeResources = writeResource :: _writeResources
  }

  /** Calls <code>WriteResource.prepare(this)</code> until a resource that is
   *  not ready to commit is found, returning false, or returns true if all
   *  write resources are prepared.  No exceptions capture is performed.
   */
  private[ccstm] def resourcePrepare: Boolean = {
    _writeResources.isEmpty || !_writeResources.exists(r => !r.prepare(this))
  }

  /** Calls <code>WriteResource.performCommit(this)</code>, returning the last
   *  exception thrown, or null if no exception occurred.
   */
  private[ccstm] def resourcePerformCommit: Throwable = {
    if (_writeResources.isEmpty) {
      null
    } else {
      var failure: Throwable = null
      for (r <- _writeResources) {
        try {
          r.performCommit(this)
        }
        catch {
          case x => failure = x
        }
      }
      failure
    }
  }

  /** Arranges for <code>callback</code> to be executed after transaction
   *  completion, if this transaction commits.  An exception thrown from an
   *  after-commit callback will be rethrown after the rest of the after-commit
   *  callbacks are invoked.  If two callbacks have different priorities then
   *  the one with the smaller priority will be executed first.
   */
  def afterCommit(callback: Txn => Unit, prio: Int) {
    // TODO: requireActive?
    // TODO: fix the definition of completion
    _afterCommit = _afterCommit.update(prio, callback :: _afterCommit.getOrElse(prio, Nil))
  }

  /** Enqueues an after-commit callback with the default priority of 0. */
  def afterCommit(callback: Txn => Unit) { afterCommit(callback, 0) }

  /** Arranges for <code>callback</code> to be executed after transaction
   *  completion, if this transaction rolls back.  An exception thrown from an
   *  after-commit callback will be rethrown after the rest of the after-commit
   *  callbacks are invoked.  If two callbacks have different priorities then
   *  the one with the smaller priority will be executed first.
   */
  def afterRollback(callback: Txn => Unit, prio: Int) {
    // TODO: what status is okay here?
    // TODO: homogenize with definition of status.completed
    _afterRollback = _afterRollback.update(prio, callback :: _afterRollback.getOrElse(prio, Nil))    
  }

  /** Enqueues an after-rollback callback with the default priority of 0. */
  def afterRollback(callback: Txn => Unit) { afterRollback(callback, 0) }

  /** Calls the handlers registered with either <code>afterCommit</code> or
   *  <code>afterRollback</code>, as appropriate, returning the last caught
   *  exception or returning null if no callbacks threw an exception.
   */
  private[ccstm] def callAfter: Exception = {
    val callbacks = if (status == Rolledback) _afterRollback else _afterCommit
    if (callbacks.isEmpty) {
      null
    }
    else {
      var failure: Exception = null
      for ((_,cbs) <- callbacks; cb <- cbs) {
        try {
          cb(this)
        }
        catch {
          case x: Exception => failure = x
        }
      }
      failure
    }
  }
}

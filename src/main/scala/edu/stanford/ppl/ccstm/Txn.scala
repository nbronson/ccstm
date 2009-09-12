/* CCSTM - (c) 2009 Stanford University - PPL */

// Txn.scala

package edu.stanford.ppl.ccstm


import java.util.concurrent.atomic.AtomicReferenceFieldUpdater

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

    /** True if, for a transaction with this status, the transaction can no
     *  longer intefere with the execution of other transactions.  All locks
     *  will have been released, but after-commit or after-rollback callbacks
     *  may still be in progress.
     */
    def completed: Boolean

    /** If <code>mustRollBack</code>, then the returned value describes the
     *  reason why rollback is inevitable, otherwise the returned value will be
     *  null.
     */
    def rollbackCause: RollbackCause
  }

  /** The <code>Status</code> for a <code>Txn</code> that may commit, but for
   *  which completion has not yet been requested.
   */
  case object Active extends Status {
    def mightCommit = true
    def mustCommit = false
    def completed = false
    def rollbackCause = null
  }

  /** The <code>Status</code> for a <code>Txn</code> that will not commit, but
   *  for which completion has not yet been requested.
   */
  case class MarkedRollback(val rollbackCause: RollbackCause) extends Status {
    { if (rollbackCause == null) throw new NullPointerException }

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
    def rollbackCause = null
  }

  /** The <code>Status</code> for a <code>Txn</code> that is guaranteed to
   *  commit, but that has not yet released all of its resources.
   */
  case object Committing extends Status {
    def mightCommit = true
    def mustCommit = true
    def completed = false
    def rollbackCause = null
  }

  /** The <code>Status</code> for a <code>Txn</code> that has successfully
   *  committed, applying all of its changes.
   */
  case object Committed extends Status {
    def mightCommit = true
    def mustCommit = true
    def completed = true
    def rollbackCause = null
  }

  /** The <code>Status</code> for a <code>Txn</code> that will definitely roll
   *  back, but that has not yet released all of its resources.
   */
  case class RollingBack(val rollbackCause: RollbackCause) extends Status {
    { if (rollbackCause == null) throw new NullPointerException }

    def mightCommit = false
    def mustCommit = false
    def completed = false
  }

  /** The <code>Status</code> for a <code>Txn</code> that has been completely
   *  rolled back.
   */
  case class Rolledback(val rollbackCause: RollbackCause) extends Status {
    { if (rollbackCause == null) throw new NullPointerException }

    def mightCommit = false
    def mustCommit = false
    def completed = true
  }


  //////////////// Rollback cause

  /** Instances of <code>RollbackCause</code> encode the reason for a
   *  particular <code>Txn</code>'s rollback.
   */
  sealed abstract class RollbackCause

  /** The base class of all <code>RollbackCause</code>s that indicate that a
   *  transaction was rolled back due to optimistic concurrency control, and
   *  hence should be automatically retried.
   */
  sealed abstract class OptimisticFailureCause extends RollbackCause {
    /** Returns the object for which the value read is no longer current. */
    val invalidObject: AnyRef

    /** Returns additional information about what portion of
     *  <code>invalidObject</code> caused the optimistic failure.
     */
    val extraInfo: Any
  }

  /** The <code>RollbackCause</code> recorded for an explicit
   *  <code>Txn.retry</code>.  If an API that performs automatic retry is in
   *  use (like <code>Atomic.run</code>) the atomic block will be retried under
   *  a new <code>Txn</code> after it is likely that it can succeed.
   */
  case object ExplicitRetryCause extends RollbackCause

  /** The <code>RollbackCause</code> recorded for an atomic block that threw an
   *  exception and was rolled back to provide failure atomicity.  The atomic
   *  block will not be automatically retried.
   */
  case class UserExceptionCause(cause: Throwable) extends RollbackCause

  /** The <code>RollbackCause</code> recorded for a rollback that occurred
   *  because a callback or resource threw an exception.  The atomic block will
   *  not be automatically retried.
   */
  case class CallbackExceptionCause(callback: AnyRef, cause: Throwable) extends RollbackCause

  /** An exception that indicates that a transaction was rolled back because a
   *  previous read is no longer valid.
   */
  case class InvalidReadCause(invalidObject: AnyRef, extraInfo: Any) extends OptimisticFailureCause

  /** An exception that indicates that a transaction was rolled back because a
   *  <code>ReadResource</code> was not valid.
   */
  case class InvalidReadResourceCause(resource: ReadResource) extends OptimisticFailureCause {
    val invalidObject = resource
    val extraInfo = null
  }

  /** An exception that indicates that a transaction was rolled back because it
   *  needed write access to an object also being written by another
   *  transaction.  The transaction that is rolled back with this exception may
   *  have been the first or second to request write access; the contention
   *  management policy may choose either transaction to roll back.
   */
  case class WriteConflictCause(invalidObject: AnyRef, extraInfo: Any) extends OptimisticFailureCause

  /** An exception that indicates that a transaction was rolled back because a
   *  <code>WriteResource</code> voted to roll back.
   */
  case class VetoingWriteResourceCause(resource: WriteResource) extends OptimisticFailureCause {
    val invalidObject = resource
    val extraInfo = null
  }
  

  //////////////// Resources participate in a two-phase commit

  /** <code>ReadResource</code>s are checked each time that the virtual
   *  snapshot from which the transaction has performed its reads must be moved
   *  into the future.  Each transaction is associated with a read version,
   *  which defines a "virtual snapshot".  When a value is encountered that may
   *  have been changed since the virtual snapshot was taken (since the read
   *  version was assigned), the read version is advanced and
   *  <code>ReadResource.valid</code> is invoked for all read resources.
   *  <p>
   *  Both read-only and updating transactions may be able to commit without
   *  advancing their virtual snapshot, in which case they won't invoke
   *  <code>valid</code>.  To help avoid race conditions, the most
   *  straightforward mechanism for adding a read resource will automatically
   *  invoke <code>valid</code>.
   *  @see edu.stanford.ppl.ccstm.AbstractTxn#addReadResource
   */
  trait ReadResource {
    /** Called during the <code>Active</code> and/or <code>Prepared</code>
     *  states, returns true iff <code>txn</code> is still valid.  Validation
     *  during the <code>Preparing</code> state may be skipped if no other
     *  transactions have committed since the last validation, or if no
     *  <code>WriteResource</code>s have been registered.
     *  <p>
     *  The read resource may call <code>txn.forceRollback</code> instead of
     *  returning false, if that is more convenient.
     *  <p>
     *  If this method throws an exception, the transaction will be rolled back
     *  with a <code>CallbackExceptionCause</code>, which will not result in
     *  automatic retry, and which will cause the exception to be rethrown
     *  after rollback is complete.
     *  @return true if <code>txn</code> is still valid, false if
     *      <code>txn</code> should be rolled back immediately.
     */
    def valid(txn: Txn): Boolean
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
     *  <p>
     *  The read resource may call <code>txn.forceRollback</code> instead of
     *  returning false, if that is more convenient.
     *  <p>
     *  If this method throws an exception, the transaction will be rolled back
     *  with a <code>CallbackExceptionCause</code>, which will not result in
     *  automatic retry, and which will cause the exception to be rethrown
     *  after rollback is complete.
     *  @return true if this resource can definitely succeed, false if
     *      <code>txn</code> must be rolled back. 
     */
    def prepare(txn: Txn): Boolean

    /** Called during the <code>Committing</code> state. */
    def performCommit(txn: Txn)

    /** Called during the <code>RollingBack</code> state. */
    def performRollback(txn: Txn)
  }

  /** This function will be invoked with any exceptions that are thrown from
   *  <code>WriteResource.performCommit</code>, from
   *  <code>WriteResource.performRollback</code>, or from a callback function
   *  registered via <code>Txn.afterCommit</code> or
   *  <code>Txn.afterRollback</code>.  The default implementation prints the
   *  transaction status and exception stack trace to <code>Console.err</code>.
   */
  @volatile var handlePostDecisionException = (txn: Txn, callback: AnyRef, exc: Throwable) => {
    Console.err.println("discarding exception from txn callback, status is " + txn.status)
    exc.printStackTrace(Console.err)
  }

  private[ccstm] val EmptyPrioCallbackMap = Map.empty[Int,List[Txn => Unit]]
}

/** An instance representing a single execution attempt for a single atomic
 *  block.  Most users will use the <code>Atomic</code> class to code atomic
 *  blocks, benefiting from automatic retry and a concise syntax.
 *  <p>
 *  The underlying STM will use the <code>failureHistory</code> in an
 *  implementation specific way to avoid starvation and livelock.
 *  @param failureHistory a list of the <code>Status.rollbackCause</code>s from
 *      earlier executions of the atomic block that this transaction will be
 *      used to execute, with the most recent first. 
 *  @see edu.stanford.ppl.ccstm.Atomic
 *
 *  @author Nathan Bronson
 */
sealed class Txn(failureHistory: List[Txn.RollbackCause]) extends STM.TxnImpl(failureHistory) {
  import Txn._

  /** Constructs a <code>Txn</code> with an empty failure history. */
  def this() = this(Nil)

  // This inheritence hierarchy is a bit strange.  Method signatures and the
  // scaladoc are in AbstractTxn, but all of the code is in Txn.  The
  // STM-specific transaction implementation is a subclass of AbstractTxn and
  // a *superclass* of Txn.  This allows the end-user to create Txn instances
  // directly without requiring Txn to be a type alias, which would prevent it
  // from appearing properly in the scaladoc.  The STM-specific transaction
  // class can make use of all of the Txn methods (via abstract methods in
  // AbstractTxn), but there are no overloads required.  The last bit of
  // indirection is that the methods actually implemented in an STM-specific
  // manner are xxxImpl-s, forwarded to from the real methods so that none of
  // the useful Txn functionality is not directly visible in the Txn class.
  
  private var _beforeCommit = EmptyPrioCallbackMap
  private var _readResources: List[ReadResource] = Nil
  private var _writeResources: List[WriteResource] = Nil
  private var _afterCommit = EmptyPrioCallbackMap
  private var _afterRollback = EmptyPrioCallbackMap

  def status: Status = _status

  def rollbackCause: RollbackCause = status.rollbackCause

  def forceRollback(cause: RollbackCause) {
    if (!requestRollback(cause)) throw new IllegalStateException
  }

  def requestRollback(cause: RollbackCause) = requestRollbackImpl(cause)

  def commit(): Status = commitImpl()

  private[ccstm] def commitAndRethrow() {
    val s = commit()
    s.rollbackCause match {
      case UserExceptionCause(x) => throw x
      case CallbackExceptionCause(_, x) => throw x
      case _ =>
    }
  }

  private[ccstm] def requireActive {
    val s = status
    if (s != Active) {
      if (s == MarkedRollback) {
        throw RollbackError
      } else {
        throw new IllegalStateException("txn.status is " + s)
      }
    }
  }

  private[ccstm] def requireNotCompleted {
    val s = status
    if (!s.completed) {
      throw new IllegalStateException("txn.status is " + s)
    }
  }

  def completed: Boolean = status.completed

  def mustCommit: Boolean = status.mustCommit

  def committed: Boolean = status == Committed

  def explicitlyValidateReads() { explicitlyValidateReadsImpl() }


  def beforeCommit(callback: Txn => Unit, prio: Int) {
    requireActive
    _beforeCommit = _beforeCommit.update(prio, callback :: _beforeCommit.getOrElse(prio, Nil))
  }

  def beforeCommit(callback: Txn => Unit) { beforeCommit(callback, 0) }

  private[ccstm] def callBeforeCommit(): Boolean = {
    if (!_beforeCommit.isEmpty) {
      for ((_,cbs) <- _beforeCommit; cb <- cbs) {
        try {
          cb(this)
        } catch {
          case x => {
            forceRollback(CallbackExceptionCause(cb, x))
          }
        }
        if (status != Active) return false
      }
    }
    return true
  }

  def addReadResource(readResource: ReadResource) {
    addReadResource(readResource, true)
  }

  def addReadResource(readResource: ReadResource, checkAfterRegister: Boolean) {
    requireActive
    _readResources = readResource :: _readResources
    if (checkAfterRegister) {
      validateNoThrow(readResource)
      if (status != Active) throw RollbackError
    }
  }

  private def validateNoThrow(res: ReadResource) {
    try {
      if (!res.valid(this)) {
        forceRollback(InvalidReadResourceCause(res))
      }
    } catch {
      case x => {
        forceRollback(CallbackExceptionCause(res, x))
      }
    }
  }

  private[ccstm] def readResourcesValidate(): Boolean = {
    if (!_readResources.isEmpty) {
      for (res <- _readResources) {
        validateNoThrow(res)
        if (status != Active) return false
      }
    }
    return true
  }

  def addWriteResource(writeResource: WriteResource) {
    requireActive
    _writeResources = writeResource :: _writeResources
  }

  private[ccstm] def writeResourcesPrepare(): Boolean = {
    if (!_writeResources.isEmpty) {
      for (res <- _writeResources) {
        try {
          if (!res.prepare(this)) {
            forceRollback(VetoingWriteResourceCause(res))
          }
        } catch {
          case x => {
            forceRollback(CallbackExceptionCause(res, x))
          }
        }
        if (status != Active) return false
      }
    }
    return true
  }

  private[ccstm] def writeResourcesPerformCommit() {
    if (!_writeResources.isEmpty) {
      for (res <- _writeResources) {
        try {
          res.performCommit(this)
        }
        catch {
          case x => handlePostDecisionException(this, res, x)
        }
      }
    }
  }

  private[ccstm] def writeResourcesPerformRollback() {
    if (!_writeResources.isEmpty) {
      for (res <- _writeResources) {
        try {
          res.performRollback(this)
        }
        catch {
          case x => handlePostDecisionException(this, res, x)
        }
      }
    }
  }

  def afterCommit(callback: Txn => Unit, prio: Int) {
    requireNotCompleted
    _afterCommit = _afterCommit.update(prio, callback :: _afterCommit.getOrElse(prio, Nil))
  }

  def afterCommit(callback: Txn => Unit) { afterCommit(callback, 0) }

  def afterRollback(callback: Txn => Unit, prio: Int) {
    requireNotCompleted
    _afterRollback = _afterRollback.update(prio, callback :: _afterRollback.getOrElse(prio, Nil))
  }

  def afterRollback(callback: Txn => Unit) { afterRollback(callback, 0) }

  private[ccstm] def callAfter() {
    val callbacks = if (status == Rolledback) _afterRollback else _afterCommit
    if (!callbacks.isEmpty) {
      for ((_,cbs) <- callbacks; cb <- cbs) {
        try {
          cb(this)
        }
        catch {
          case x => handlePostDecisionException(this, cb, x)
        }
      }
    }
  }
}

private[ccstm] object StatusHolder {
  val statusUpdater = (new StatusHolder).newStatusUpdater
}

private[ccstm] class StatusHolder {

  @volatile private[ccstm] var _status: Txn.Status = Txn.Active

  private[ccstm] def _statusCAS(before: Txn.Status, after: Txn.Status): Boolean = {
    StatusHolder.statusUpdater.compareAndSet(this, before, after)
  }

  private[ccstm] def newStatusUpdater = {
    // we have to create the updater from a method of StatusHolder, because
    // all Scala variables are private under the covers
    AtomicReferenceFieldUpdater.newUpdater(classOf[StatusHolder], classOf[Object], "_status")
  }
}

private[ccstm] abstract class AbstractTxn extends StatusHolder {
  import Txn._

  //////////////// Functions to be implemented in an STM-specific manner

  private[ccstm] def requestRollbackImpl(cause: RollbackCause): Boolean
  private[ccstm] def commitImpl(): Status
  private[ccstm] def explicitlyValidateReadsImpl()

  //////////////// Functions implemented in Txn, but available to the STM-specific base class

  /** Returns the transaction's current status.  The status may change at any
   *  time.
   */
  def status: Status

  /** If <code>status.mustRollBack</code> is true, then this returns the
   *  exception that is responsible for the rollback, otherwise it returns
   *  null.  This is a convenience method that is equivalent to
   *  <code>status.rollbackCause</code>.
   *  @see edu.stanford.ppl.ccstm.Status#rollbackCause
   */
  def rollbackCause: RollbackCause
  
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
   */
  def forceRollback(cause: RollbackCause)

  /** Attempts to doom the transaction, returning true if the transaction will
   *  definitely roll back, false if the transaction will definitely commit.
   *  This method may return true if rollback occurs for a reason other than
   *  <code>cause</code>.  Unlike <code>forceRollback(cause)</code>, this
   *  method may be called from any thread, and never throws an exception.
   */
  def requestRollback(cause: RollbackCause): Boolean

  /** Completes the transaction, committing if possible.  Returns the final
   *  status.
   */
  def commit(): Status

  /** Calls <code>commit</code>, then throws the exception that caused rollback
   *  if the cause was either <code>UserExceptionCause</code> or
   *  <code>CallbackExceptionCause</code>.
   */
  private[ccstm] def commitAndRethrow()

  /** Throws <code>RollbackError</code> if the transaction is marked for
   *  rollback, otherwise throws an <code>IllegalStateException</code> if the
   *  transaction is not active.
   *  @see edu.stanford.ppl.ccstm.Txn.Active
   *  @see edu.stanford.ppl.ccstm.Txn.MarkedRollback
   */
  private[ccstm] def requireActive

  /** Throws an <code>IllegalStateException</code> if the transaction is
   *  completed.
   */
  private[ccstm] def requireNotCompleted

  /** A convenience function, equivalent to <code>status.completed</code>.
   *  @see edu.stanford.ppl.ccstm.Txn.Status#completed
   */
  def completed: Boolean

  /** A convenience function, equivalent to <code>status.mustCommit</code>.
   *  @see edu.stanford.ppl.ccstm.Txn.Status#mustCommit
   */
  def mustCommit: Boolean

  /** A convenience function, equivalent to <code>status == Committed</code>.
   */
  def committed: Boolean

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

  /** Calls all callbacks registered via <code>beforeCommit</code>,
   *  unless rollback is required.  Returns true if commit is still possible.
   *  Captures and handles exceptions.
   */
  private[ccstm] def callBeforeCommit(): Boolean

  /** Adds a read resource to the transaction, which will participate in
   *  validation, and then immediately calls
   *  <code>readResource.valid</code>.  This register/validate pair
   *  corresponds to the most common use case for read resources.  If the
   *  caller takes responsibility for validating the read resource then they
   *  may use the two-argument form of this method.  If the validation fails
   *  then the transaction will be immediately rolled back and this method will
   *  not return.
   *  @throws IllegalStateException if this transaction is not active.
   */
  def addReadResource(readResource: ReadResource)

  /** Adds a read resource to the transaction like the single argument form of
   *  this method, but skips the subsequent call to
   *  <code>readResource.valid</code> if <code>checkAfterRegister</code> is
   *  false.
   *  @throws IllegalStateException if this transaction is not active.
   */
  def addReadResource(readResource: ReadResource, checkAfterRegister: Boolean)

  /** Calls <code>ReadResource.valid(this)</code> for all read resources,
   *  unless rollback is required.  Returns true if commit is still possible.
   *  Captures and handles exceptions.
   */
  private[ccstm] def readResourcesValidate(): Boolean

  /** Adds a write resource to the transaction, which will participate in the
   *  two-phase commit protocol.
   *  @throws IllegalStateException if this transaction is not active.
   */
  def addWriteResource(writeResource: WriteResource)

  /** Calls <code>WriteResource.prepare(this)</code> for all write resources,
   *  unless rollback is required.  Returns true if commit is still possible.
   *  Captures and handles exceptions.
   */
  private[ccstm] def writeResourcesPrepare(): Boolean

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

  /** Calls the handlers registered with either <code>afterCommit</code> or
   *  <code>afterRollback</code>, as appropriate, handling any exceptions.
   */
  private[ccstm] def callAfter()
}

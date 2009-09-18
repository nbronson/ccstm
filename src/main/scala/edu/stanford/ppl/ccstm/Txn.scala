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

    /** True if, for a transaction with this status, the outcome is certain.
     *  Equal to <code>(mustCommit || mustRollback)</code>.
     */
    def decided = mustCommit || mustRollBack

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

  /** The <code>Status</code> for a <code>Txn</code> for which reads and writes
   *  may still be performed and callbacks may still be registered.
   */
  case object Active extends Status {
    def mightCommit = true
    def mustCommit = false
    def completed = false
    def rollbackCause = null
  }

  /** The <code>Status</code> for a <code>Txn</code> that is undergoing final
   *  validation.  Validating transactions may commit or roll back.  No more
   *  reads or writes may be performed, and only after-commit and
   *  after-rollback callbacks may be registered.
   */
  case object Validating extends Status {
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
   *  committed, applying all of its changes and releasing all of the resources
   *  it had acquired.
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
   *  rolled back, releasing all of the resources it had acquired.
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
   *  a new <code>Txn</code> after it is likely that it can succeed.  The
   *  <code>readSet</code> is an opaque object used to block until the retry
   *  may succeed.
   */
  case class ExplicitRetryCause(readSet: AnyRef) extends RollbackCause

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
    /** Should return true iff <code>txn</code> is still valid.  May be invoked
     *  during the <code>Active</code> and/or the <code>Validating</code>
     *  states.  Validation during the <code>Validating</code> state may be
     *  skipped by the STM if no other transactions have committed since the
     *  last validation, or if no <code>WriteResource</code>s have been
     *  registered.
     *  <p>
     *  The read resource may call <code>txn.forceRollback</code> instead of
     *  returning false, if that is more convenient.
     *  <p>
     *  If this method throws an exception and the transaction has not
     *  previously been marked for rollback, the transaction will be rolled
     *  back with a <code>CallbackExceptionCause</code>, which will not result
     *  in automatic retry, and which will cause the exception to be rethrown
     *  after rollback is complete.
     *  @return true if <code>txn</code> is still valid, false if
     *      <code>txn</code> should be rolled back as soon as possible.
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
    /** Called during the <code>Validating</code> state, returns true if this
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


  /** Blocks the current thread until some value contained in one of the read
   *  sets of one of the elements of <code>explicitRetries</code> might have
   *  changed.
   */
  def awaitRetry(explicitRetries: ExplicitRetryCause*) = STMImpl.awaitRetry(explicitRetries:_*)
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
sealed class Txn(failureHistory: List[Txn.RollbackCause]) extends STMImpl.TxnImpl(failureHistory) {
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
  
  private var _readResources: CallbackList[ReadResource] = null

  /** Includes WriteResource-s and beforeCommit callbacks. */
  private var _writeLikeResources: CallbackList[WriteResource] = null
  private var _writeResourcesPresent = false
  private var _afterCommit: CallbackList[Txn => Unit] = null
  private var _afterRollback: CallbackList[Txn => Unit] = null

  def status: Status = _status

  def forceRollback(cause: RollbackCause) {
    if (!requestRollback(cause)) throw new IllegalStateException
  }

  def requestRollback(cause: RollbackCause) = requestRollbackImpl(cause)

  def retry() { retryImpl() }

  def commit(): Status = commitImpl()

  private[ccstm] def commitAndRethrow() {
    val s = commit()
    s.rollbackCause match {
      case UserExceptionCause(x) => throw x
      case CallbackExceptionCause(_, x) => throw x
      case _ =>
    }
  }

  def explicitlyValidateReads() { explicitlyValidateReadsImpl() }


  def beforeCommit(callback: Txn => Unit, prio: Int) {
    requireActive
    if (_writeLikeResources == null) _writeLikeResources = new CallbackList[WriteResource]
    _writeLikeResources.add(new WriteResource {
      def prepare(txn: Txn): Boolean = { callback(txn); true }
      def performCommit(txn: Txn) {}
      def performRollback(txn: Txn) {}
    }, prio)
  }

  def beforeCommit(callback: Txn => Unit) { beforeCommit(callback, 0) }

  def addReadResource(readResource: ReadResource) {
    addReadResource(readResource, 0)
  }

  def addReadResource(readResource: ReadResource, prio: Int) {
    addReadResource(readResource, prio, true)
  }

  def addReadResource(readResource: ReadResource, prio: Int, checkAfterRegister: Boolean) {
    requireActive
    if (_readResources == null) _readResources = new CallbackList[ReadResource]
    _readResources.add(readResource, prio)
    if (checkAfterRegister) {
      validateNoThrow(readResource)
      requireActive
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
    if (_readResources != null) {
      for (res <- _readResources) {
        validateNoThrow(res)
        if (!status.mightCommit) return false
      }
    }
    return true
  }

  def addWriteResource(writeResource: WriteResource) {
    addWriteResource(writeResource, 100)
  }

  def addWriteResource(writeResource: WriteResource, prio: Int) {
    requireActive
    if (_writeLikeResources == null) _writeLikeResources = new CallbackList[WriteResource]
    _writeResourcesPresent = true
    _writeLikeResources.add(writeResource, prio)
  }

  private[ccstm] def writeResourcesPresent = _writeResourcesPresent

  private[ccstm] def writeLikeResourcesPrepare(): Boolean = {
    if (_writeLikeResources != null) {
      for (res <- _writeLikeResources) {
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
    if (_writeLikeResources != null) {
      for (res <- _writeLikeResources) {
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
    assert(status.isInstanceOf[RollingBack])
    if (_writeLikeResources != null) {
      for (res <- _writeLikeResources) {
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
    if (_afterCommit == null) _afterCommit = new CallbackList[Txn => Unit]
    _afterCommit.add(callback, prio)
  }

  def afterCommit(callback: Txn => Unit) { afterCommit(callback, 0) }

  def afterRollback(callback: Txn => Unit, prio: Int) {
    requireNotCompleted
    if (_afterRollback == null) _afterRollback = new CallbackList[Txn => Unit]
    _afterRollback.add(callback, prio)
  }

  def afterRollback(callback: Txn => Unit) { afterRollback(callback, 0) }

  def afterCompletion(callback: Txn => Unit) { afterCompletion(callback, 0) }

  def afterCompletion(callback: Txn => Unit, prio: Int) {
    afterCommit(callback, prio)
    afterRollback(callback, prio)
  }

  private[ccstm] def callAfter() {
    val callbacks = if (status == Committed) _afterCommit else _afterRollback
    if (callbacks != null) {
      for (cb <- callbacks) {
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

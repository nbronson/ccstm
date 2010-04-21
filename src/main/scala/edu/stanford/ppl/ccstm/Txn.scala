/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// Txn.scala

package edu.stanford.ppl.ccstm

import impl.ThreadContext

/** One of `Single`, `Escaped`, or an instance of `Txn`
 *  @see edu.stanford.ppl.ccstm.Ref.View#mode
 */
sealed trait BindingMode {
  //def dynContext: Option[Txn]
}

object Single extends BindingMode {
  //def dynContext: Option[Txn] = Txn.current
  override def toString = "Single"
}

object Escaped extends BindingMode {
  //def dynContext: Option[Txn] = None
  override def toString = "Escaped"
}

object Txn {

  val EnableCounters = "1tTyY" contains (System.getProperty("ccstm.counters", "") + "0").charAt(0)

  //////////////// Status

  /** Returns `Some(t)` if `t` is statically or dynamically attached to the
   *  current thread, or `None` if no transaction is attached.  If an implicit
   *  `Txn` is available, that is used, otherwise a dynamic lookup is
   *  performed.
   */
  def current(implicit mt: MaybeTxn): Option[Txn] = {
    val t = currentOrNull
    if (null != t) Some(t) else None
  }

  /** Equivalent to `current getOrElse null`. */
  def currentOrNull(implicit mt: MaybeTxn): Txn = {
    mt match {
      case t: Txn => t
      case TxnUnknown => dynCurrentOrNull 
    }
  }

  /** Performs a dynamic lookup of the currently active transaction, returning
   *  an active `Txn` or null.
   */
  private[ccstm] def dynCurrentOrNull: Txn = {
    ThreadContext.get.txn.asInstanceOf[Txn]
  }

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
    { if (null == rollbackCause) throw new NullPointerException }

    def mightCommit = false
    def mustCommit = false
    def completed = false
  }

  /** The <code>Status</code> for a <code>Txn</code> that has been completely
   *  rolled back, releasing all of the resources it had acquired.
   */
  case class Rolledback(val rollbackCause: RollbackCause) extends Status {
    { if (null == rollbackCause) throw new NullPointerException }

    def mightCommit = false
    def mustCommit = false
    def completed = true
  }


  //////////////// Rollback cause

  /** Instances of <code>RollbackCause</code> encode the reason for a
   *  particular <code>Txn</code>'s rollback.
   */
  sealed abstract class RollbackCause {
    private[ccstm] def counter: Counter
  }

  /** The base class of all <code>RollbackCause</code>s that indicate that a
   *  transaction was rolled back due to optimistic concurrency control, and
   *  hence should be automatically retried.
   */
  sealed abstract class OptimisticFailureCause extends RollbackCause {
    /** Identifies the object that triggered rollback.  The actual object is
     *  implementation dependent, but will provide at least a reasonable
     *  attempt at a useful <code>.toString()</code> method.
     */
    val trigger: Any
  }

  /** The <code>RollbackCause</code> recorded for an explicit
   *  <code>Txn.retry</code>.  If an API that performs automatic retry is in
   *  use (like <code>Atomic.run</code>) the atomic block will be retried under
   *  a new <code>Txn</code> after it is likely that it can succeed.  The
   *  <code>readSet</code> is an opaque object used to block until the retry
   *  may succeed.
   */
  case class ExplicitRetryCause(readSet: AnyRef) extends RollbackCause {
    private[ccstm] def counter = explicitRetryCounter
  }

  /** The <code>RollbackCause</code> recorded for an atomic block that threw an
   *  exception and was rolled back to provide failure atomicity.  The atomic
   *  block will not be automatically retried.
   */
  case class UserExceptionCause(cause: Throwable) extends RollbackCause {
    private[ccstm] def counter = userExceptionCounter
  }

  /** The <code>RollbackCause</code> recorded for a rollback that occurred
   *  because a callback or resource threw an exception.  The atomic block will
   *  not be automatically retried.
   */
  case class CallbackExceptionCause(callback: AnyRef, cause: Throwable) extends RollbackCause {
    private[ccstm] def counter = callbackExceptionCounter
  }

  /** An exception that indicates that a transaction was rolled back because a
   *  previous read is no longer valid.
   */
  case class InvalidReadCause(trigger: AnyRef, extraInfo: String) extends OptimisticFailureCause {
    private[ccstm] def counter = invalidReadCounter
  }

  /** An exception that indicates that a transaction was rolled back because a
   *  <code>ReadResource</code> was not valid.
   */
  case class InvalidReadResourceCause(trigger: ReadResource) extends OptimisticFailureCause {
    private[ccstm] def counter = invalidReadResourceCounter
  }

  /** An exception that indicates that a transaction was rolled back because it
   *  needed write access to an object also being written by another
   *  transaction.  The transaction that is rolled back with this exception may
   *  have been the first or second to request write access; the contention
   *  management policy may choose either transaction to roll back.  The
   *  contention manager may record extra information about its choice in
   *  <code>extraInfo</code>.
   */
  case class WriteConflictCause(trigger: Any, extraInfo: String) extends OptimisticFailureCause {
    private[ccstm] def counter = writeConflictCounter
  }

  /** An exception that indicates that a transaction was rolled back because a
   *  <code>WriteResource</code> voted to roll back.
   */
  case class VetoingWriteResourceCause(trigger: WriteResource) extends OptimisticFailureCause {
    private[ccstm] def counter = vetoingWriteResourceCounter
  }


  //////////////// Statistics

  private[ccstm] val commitCounter = new Counter  
  private[ccstm] val explicitRetryCounter = new Counter
  private[ccstm] val userExceptionCounter = new Counter
  private[ccstm] val callbackExceptionCounter = new Counter
  private[ccstm] val invalidReadCounter = new Counter
  private[ccstm] val invalidReadResourceCounter = new Counter
  private[ccstm] val writeConflictCounter = new Counter
  private[ccstm] val vetoingWriteResourceCounter = new Counter
  private[ccstm] val bargingCommitCounter = new Counter
  private[ccstm] val bargingRollbackCounter = new Counter

  private[ccstm] def countsToStr: String = {
    val buf = new StringBuilder
    for (m <- Txn.getClass.getMethods.toList.reverse) {
      val n = m.getName
      if (n.endsWith("Count")) {
        buf ++= "\nCCSTM: " + n + (" " * (26-n.length)) + " = " + m.invoke(Txn)
      }
    }
    buf.toString
  }

//  new Thread("status updater") {
//    setDaemon(true)
//
//    override def run {
//      while (true) {
//        println(countsToStr)
//        Thread.sleep(5000)
//      }
//    }
//  }.start()

  {
    if (EnableCounters) {
      Runtime.getRuntime.addShutdownHook(new Thread("Txn shutdown hook") {
        override def run {
          println(countsToStr)
        }
      })
    }
  }

  /** Returns the number of transactions that have committed.  Only maintained
   *  if `EnableCounters` is true.
   */
  def commitCount = commitCounter.get

  /** Returns the number of transaction that rolled back for any reason,
   *  including for an explicit retry.  Only maintained if `EnableCounters` is
   *  true.
   */
  def rollbackCount = (explicitRetryCount
          + userExceptionCount
          + callbackExceptionCount
          + invalidReadCount
          + invalidReadResourceCount
          + writeConflictCount
          + vetoingWriteResourceCount)

  /** Returns the number of transactions that rolled back due to a concurrency
   *  control failure, which includes invalid reads, invalid read resources,
   *  write conflicts, and vetoing write resources.  Only maintained if
   *  `EnableCounters` is true.
   */
  def optimisticFailureCount = (invalidReadCount
          + invalidReadResourceCount
          + writeConflictCount
          + vetoingWriteResourceCount)

  /** Returns the number of transactions that rolled back with a
   *  `ExplicitRetryCause` rollback cause.  Only maintained if `EnableCounters`
   *  is true.
   */
  def explicitRetryCount = explicitRetryCounter.get

  /** Returns the number of transactions that rolled back with a
   *  `UserExceptionCause` rollback cause.  Only maintained if `EnableCounters`
   *  is true.
   */
  def userExceptionCount = userExceptionCounter.get

  /** Returns the number of transactions that rolled back with a
   *  `CallbackExceptionCause` rollback cause.  Only maintained if
   *  `EnableCounters` is true.
   */
  def callbackExceptionCount = callbackExceptionCounter.get

  /** Returns the number of transactions that rolled back with a
   *  `InvalidReadCause` rollback cause.  Only maintained if `EnableCounters`
   *  is true.
   */
  def invalidReadCount = invalidReadCounter.get

  /** Returns the number of transactions that rolled back with a
   *  `InvalidReadResourceCause` rollback cause.  Only maintained if
   *  `EnableCounters` is true.
   */
  def invalidReadResourceCount = invalidReadResourceCounter.get

  /** Returns the number of transactions that rolled back with a
   *  `WriteConflictCause` rollback cause.  Only maintained if `EnableCounters`
   *  is true.
   */
  def writeConflictCount = writeConflictCounter.get

  /** Returns the number of transactions that rolled back with a
   *  `VetoingWriteResourceCause` rollback cause.  Only maintained if
   *  `EnableCounters` is true.
   */
  def vetoingWriteResourceCount = vetoingWriteResourceCounter.get

  /** Returns the number of transactions that were attempted using the
   *  pessimistic barging mode, and that committed.  Only maintained if
   *  `EnableCounters` is true.
   */
  def bargingCommitCount = bargingCommitCounter.get

  /** Returns the number of transactions that were attempted using the
   *  pessimistic barging mode, and that rolled back.
   */
  def bargingRollbackCount = bargingRollbackCounter.get

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
   * @see edu.stanford.ppl.ccstm.AbstractTxn # addReadResource
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
   *  changed.  Each <code>ExplicitRetryCause</code> instance may only be
   *  passed once to an invocation of this method.
   */
  def awaitRetryAndDestroy(explicitRetries: ExplicitRetryCause*) = {
    impl.STMImpl.awaitRetryAndDestroy(explicitRetries:_*)
  }
}

/** An instance representing a single execution attempt for a single atomic
 *  block.  In almost all cases, `Txn` instances should be obtained by passing
 *  a block to `STM.atomic`.
 *  @see edu.stanford.ppl.ccstm.STM#atomic
 *
 *  @author Nathan Bronson
 */
final class Txn private[ccstm] (failureHistory: List[Txn.RollbackCause], ctx: ThreadContext
        ) extends impl.TxnImpl(failureHistory, ctx) with MaybeTxn with BindingMode {
  import Txn._

  /** An instance representing a single execution attempt for a single atomic
   *  block.  In almost all cases, `Txn` instances should be obtained by passing
   *  a block to `STM.atomic`.
   *  @see edu.stanford.ppl.ccstm.STM
   */
  private[ccstm] def this() = this(Nil, ThreadContext.get)

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
  // We set this up this way so that we could experiment with different
  // concrete implementations.  There are no longer separate implementations
  // (at least on the same git branch), so at some point in the future we might
  // rearrange everything to something more normal.
  
  /** Values of <code>TxnLocal</code>s for this transaction, created lazily. */
  private[ccstm] var locals: java.util.IdentityHashMap[TxnLocal[_],Any] = null


  //def dynContext: Option[Txn] = Some(this)
  
  def status: Status = _status

  def forceRollback(cause: RollbackCause) {
    if (!requestRollback(cause)) throw new IllegalStateException
  }

  def requestRollback(cause: RollbackCause) = requestRollbackImpl(cause)

  def retry(): Nothing = { retryImpl() }

  def commit(): Status = commitImpl()

  /** On return, status.rollbackCause will be either null, ExplicitRetryCause,
   *  or an OptimisticFailureCause.
   */
  private[ccstm] def commitAndRethrow() {
    val s = commit()
    s.rollbackCause match {
      case UserExceptionCause(x) => throw x
      case CallbackExceptionCause(_, x) => throw x
      case _ =>
    }
  }

  def explicitlyValidateReads() { explicitlyValidateReadsImpl() }

  def addReference(ptr: AnyRef) { addReferenceImpl(ptr) }
  

  def beforeCommit(callback: Txn => Unit, prio: Int) {
    requireActive()
    _callbacks.writeLikeResources.add(new WriteResource {
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
    requireActive()
    _callbacks.readResources.add(readResource, prio)
    if (checkAfterRegister) {
      validateNoThrow(readResource)
      requireActive()
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
    if (!_callbacks.readResources.isEmpty) {
      for (res <- _callbacks.readResources) {
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
    requireActive()
    _callbacks.writeLikeResources.add(writeResource, prio)
    _callbacks.writeResourcesPresent = true
  }

  private[ccstm] def writeResourcesPresent = _callbacks.writeResourcesPresent

  private[ccstm] def writeLikeResourcesPrepare(): Boolean = {
    if (!_callbacks.writeLikeResources.isEmpty) {
      for (res <- _callbacks.writeLikeResources) {
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
    if (_callbacks.writeLikeResources.isEmpty) {
      for (res <- _callbacks.writeLikeResources) {
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
    if (_callbacks.writeLikeResources.isEmpty) {
      for (res <- _callbacks.writeLikeResources) {
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
    requireNotCompleted()
    _callbacks.afterCommit.add(callback, prio)
  }

  def afterCommit(callback: Txn => Unit) { afterCommit(callback, 0) }

  def afterRollback(callback: Txn => Unit, prio: Int) {
    requireNotCompleted()
    _callbacks.afterRollback.add(callback, prio)
  }

  def afterRollback(callback: Txn => Unit) { afterRollback(callback, 0) }

  def afterCompletion(callback: Txn => Unit) { afterCompletion(callback, 0) }

  def afterCompletion(callback: Txn => Unit, prio: Int) {
    afterCommit(callback, prio)
    afterRollback(callback, prio)
  }

  private[ccstm] def callAfter() {
    val s = status
    val callbacks = (if (s eq Committed) {
      if (EnableCounters) {
        commitCounter += 1
        if (barging) bargingCommitCounter += 1
      }
      _callbacks.afterCommit
    } else {
      if (EnableCounters) {
        s.rollbackCause.counter += 1
        if (barging) bargingRollbackCounter += 1
      }
      _callbacks.afterRollback
    })
    if (!callbacks.isEmpty) {
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

  def detach() = detach(impl.ThreadContext.get)

  def attach() = attach(impl.ThreadContext.get)

  private[ccstm] def detach(ctx: ThreadContext): Unit = { ctx.txn = null }

  private[ccstm] def attach(ctx: ThreadContext): Unit = { ctx.txn = this }
}

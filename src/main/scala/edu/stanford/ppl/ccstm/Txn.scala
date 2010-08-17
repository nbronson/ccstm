/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// Txn.scala

package edu.stanford.ppl.ccstm

import impl.ThreadContext

/** One of `Single`, `Escaped`, or an instance of `Txn`
 *  @see edu.stanford.ppl.ccstm.Ref.View#mode
 */
sealed trait AccessMode {
  //def dynContext: Option[Txn]
}

object Single extends AccessMode {
  //def dynContext: Option[Txn] = Txn.current
  override def toString = "Single"
}

object Escaped extends AccessMode {
  //def dynContext: Option[Txn] = None
  override def toString = "Escaped"
}

object Txn {

  val EnableCounters = flag("ccstm.counters", false)

  // requires EnableCounters, defaults to on
  val EnableSizeHistos = EnableCounters && flag("ccstm.histo", true)

  private def flag(name: String, default: Boolean) = {
    "1tTyY" contains (System.getProperty(name, "") + default).charAt(0)
  }

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

  /** Represents the current status of a `Txn`. */
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
     *  has already been decided.  Equivalent to `!willCommit`.
     */
    def mightRollBack = !mustCommit
    
    /** True if a transaction with this status definitely will not commit,
     *  false if commit is possible.  Equivalent to `!mightCommit`.
     */
    def mustRollBack = !mightCommit

    /** True if, for a transaction with this status, the outcome is certain.
     *  Equal to `(mustCommit || mustRollback)`.
     */
    def decided = mustCommit || mustRollBack

    /** True if a transaction is not decided and may be canceled by a call to
     *  `Txn.requestRollback` or by a lock request by a higher-priority
     *  transaction.
     */
    def remotelyCancellable: Boolean
    
    /** True if, for a transaction with this status, the transaction can no
     *  longer intefere with the execution of other transactions.  All locks
     *  will have been released, but after-commit or after-rollback callbacks
     *  may still be in progress.
     */
    def completed: Boolean

    /** If `mustRollBack`, then the returned value describes the
     *  reason why rollback is inevitable, otherwise the returned value will be
     *  null.
     */
    def rollbackCause: RollbackCause
  }

  /** The `Status` for a `Txn` for which reads and writes
   *  may still be performed and callbacks may still be registered.
   */
  case object Active extends Status {
    def mightCommit = true
    def mustCommit = false
    def remotelyCancellable = true
    def completed = false
    def rollbackCause = null
  }

  /** The `Status` for a `Txn` that is undergoing final validation.  Validating
   *  transactions may commit or roll back.  No more reads or writes may be
   *  performed, and only write resources, after-commit, and after-rollback
   *  callbacks may be registered.
   */
  case object Validating extends Status {
    def mightCommit = true
    def mustCommit = false
    def remotelyCancellable = true
    def completed = false
    def rollbackCause = null
  }

  /** The `Status` for a `Txn` that is preparing its write resources.  This is
   *  similar to `Validating`, but the transaction may no longer be rolled back
   *  by another transaction with a higher priority or by a `requestRollback`
   *  call from another thread.  No more reads or writes may be performed, and
   *  only after-commit and after-rollback callbacks may be registered.
   */
  case object Preparing extends Status {
    def mightCommit = true
    def mustCommit = false
    def remotelyCancellable = false
    def completed = false
    def rollbackCause = null
  }

  /** The `Status` for a `Txn` that is guaranteed to
   *  commit, but that has not yet released all of its resources.
   */
  case object Committing extends Status {
    def mightCommit = true
    def mustCommit = true
    def remotelyCancellable = false
    def completed = false
    def rollbackCause = null
  }

  /** The `Status` for a `Txn` that has successfully
   *  committed, applying all of its changes and releasing all of the resources
   *  it had acquired.
   */
  case object Committed extends Status {
    def mightCommit = true
    def mustCommit = true
    def remotelyCancellable = false
    def completed = true
    def rollbackCause = null
  }

  /** The `Status` for a `Txn` that will definitely roll
   *  back, but that has not yet released all of its resources.
   */
  case class RollingBack(val rollbackCause: RollbackCause) extends Status {
    { if (null == rollbackCause) throw new NullPointerException }

    def mightCommit = false
    def mustCommit = false
    def remotelyCancellable = false
    def completed = false
  }

  /** The `Status` for a `Txn` that has been completely
   *  rolled back, releasing all of the resources it had acquired.
   */
  case class Rolledback(val rollbackCause: RollbackCause) extends Status {
    { if (null == rollbackCause) throw new NullPointerException }

    def mightCommit = false
    def mustCommit = false
    def remotelyCancellable = false
    def completed = true
  }


  //////////////// Rollback cause

  /** Instances of `RollbackCause` encode the reason for a
   *  particular `Txn`'s rollback.
   */
  sealed abstract class RollbackCause {
    private[ccstm] def counter: Counter
  }

  /** The base class of all `RollbackCause`s that indicate that a
   *  transaction was rolled back due to optimistic concurrency control, and
   *  hence should be automatically retried.
   */
  sealed abstract class OptimisticFailureCause extends RollbackCause {
    /** Identifies the object that triggered rollback.  The actual object is
     *  implementation dependent, but will provide at least a reasonable
     *  attempt at a useful `.toString()` method.
     */
    val trigger: Any
  }

  /** The `RollbackCause` recorded for an explicit `retry`.  The
   *  `readSet` is an opaque object used to block until the retry
   *  may succeed.
   */
  case class ExplicitRetryCause(readSet: AnyRef) extends RollbackCause {
    private[ccstm] def counter = explicitRetryCounter
  }

  /** The `RollbackCause` recorded for an atomic block that threw an
   *  exception and was rolled back to provide failure atomicity.  The atomic
   *  block will not be automatically retried.
   */
  case class UserExceptionCause(cause: Throwable) extends RollbackCause {
    private[ccstm] def counter = userExceptionCounter
  }

  /** The `RollbackCause` recorded for a rollback that occurred
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
   *  `ReadResource` was not valid.
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
   *  `extraInfo`.
   */
  case class WriteConflictCause(trigger: Any, extraInfo: String) extends OptimisticFailureCause {
    private[ccstm] def counter = writeConflictCounter
  }

  /** An exception that indicates that a transaction was rolled back because a
   *  `WriteResource` voted to roll back.
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
  private[ccstm] val commitReadSetSizeHisto = newSizeHisto
  private[ccstm] val commitWriteSetSizeHisto = newSizeHisto
  private[ccstm] val rollbackReadSetSizeHisto = newSizeHisto
  private[ccstm] val rollbackWriteSetSizeHisto = newSizeHisto

  private def newSizeHisto = if (!EnableSizeHistos) null else Array.tabulate(32) { _ => new Counter }

  private[ccstm] def countsToStr: String = {
    val buf = new StringBuilder
    for (m <- Txn.getClass.getMethods.toList.reverse) {
      val n = m.getName
      if (n.endsWith("Count")) {
        buf ++= "\nCCSTM: " + n + (" " * (26-n.length)) + " = " + m.invoke(Txn)
      }
    }
    if (EnableSizeHistos) {
      buf ++= "\nCCSTM: commitReadSet    = " + sizeHistoToStr(commitReadSetSizeHisto)
      buf ++= "\nCCSTM: commitWriteSet   = " + sizeHistoToStr(commitWriteSetSizeHisto)
      buf ++= "\nCCSTM: rollbackReadSet  = " + sizeHistoToStr(rollbackReadSetSizeHisto)
      buf ++= "\nCCSTM: rollbackWriteSet = " + sizeHistoToStr(rollbackWriteSetSizeHisto)
    }
    buf.toString
  }

  private[ccstm] def sizeHistoToStr(counts: Array[Counter]): String = {
    val data = counts map { _.get }
    val last = data lastIndexWhere { _ != 0L }
    data.take(1 + last).mkString(", ")
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

  /** `ReadResource`s are checked each time that the virtual
   *  snapshot from which the transaction has performed its reads must be moved
   *  into the future.  Each transaction is associated with a read version,
   *  which defines a "virtual snapshot".  When a value is encountered that may
   *  have been changed since the virtual snapshot was taken (since the read
   *  version was assigned), the read version is advanced and
   *  `ReadResource.valid` is checked for all read resources.
   *
   *  Both read-only and updating transactions may be able to commit without
   *  advancing their virtual snapshot, in which case they won't invoke
   *  `valid`.
   */
  trait ReadResource {
    /** Should return true iff `txn` is still valid.  May be invoked during the
     *  `Active` and/or the `Validating` states.  Validation during the
     *  `Validating` state may be skipped by the STM if no other transactions
     *  have committed since the last validation, or if no `WriteResource`s
     *  have been registered.
     *
     *  The read resource may call `txn.forceRollback` instead of returning
     *  false, if that is more convenient.
     *
     *  If this method throws an exception and the transaction has not
     *  previously been marked for rollback, the transaction will be rolled
     *  back with a `CallbackExceptionCause`, which will not result in
     *  automatic retry, and which will cause the exception to be rethrown
     *  after rollback is complete.
     *  @return true if `txn` is still valid, false if `txn` should be rolled
     *      back as soon as possible.
     */
    def valid(txn: Txn): Boolean
  }

  /** `WriteResource`s participate in a two-phase commit.  Each write resource
   *  is given the opportunity to veto commit, and each write resource will be
   *  informed of the decision.  Unlike read resources, write resources are not
   *  consulted during advancement of the read version (and the associated
   *  virtual snapshot).  If an update resource needs to be revalidated when
   *  the read version is advanced it should also register a `ReadResource`.
   */
  trait WriteResource {
    /** Called during the `Preparing` state, returns true if this resource
     *  agrees to commit.  If it has already been determined that a transaction
     *  will roll back, then this method won't be called.  All locks or other
     *  resources required to complete the commit must be acquired during this
     *  callback, or else this method must return false.  The consensus
     *  decision will be delivered via a subsequent call to `performCommit` or
     *  `performRollback`.  `performRollback` will be called on every write
     *  resource, whether or not their `prepare` method was called.
     *
     *  The write resource may call `txn.forceRollback` instead of returning
     *  false, if that is more convenient.
     *
     *  If this method throws an exception, the transaction will be rolled back
     *  with a `CallbackExceptionCause`, which will not result in automatic
     *  retry, and which will cause the exception to be rethrown after rollback
     *  is complete.
     */
    def prepare(txn: Txn): Boolean

    /** Called during the `Committing` state. */
    def performCommit(txn: Txn)

    /** Called during the `RollingBack` state. */
    def performRollback(txn: Txn)
  }

  /** This function will be invoked with any exceptions that are thrown from
   *  `WriteResource.performCommit`, from `WriteResource.performRollback`, or
   *  from a callback function registered via `Txn.afterCommit` or
   *  `Txn.afterRollback`.  The default implementation prints the
   *  transaction status and exception stack trace to `Console.err`.
   */
  @volatile var handlePostDecisionException = (txn: Txn, callback: AnyRef, exc: Throwable) => {
    Console.err.println("discarding exception from txn callback, status is " + txn.status)
    exc.printStackTrace(Console.err)
  }


  /** Blocks the current thread until some value contained in one of the read
   *  sets of one of the elements of `explicitRetries` might have
   *  changed.  Each `ExplicitRetryCause` instance may only be
   *  passed once to an invocation of this method.
   */
  def awaitRetryAndDestroy(explicitRetries: ExplicitRetryCause*) = {
    impl.STMImpl.awaitRetryAndDestroy(explicitRetries:_*)
  }
}

/** An instance representing a single execution attempt for a single atomic
 *  block.  In almost all cases, `Txn` instances should be obtained by passing
 *  a block to the `atomic` object's `apply` method.
 *  @see edu.stanford.ppl.ccstm.atomic
 *
 *  @author Nathan Bronson
 */
final class Txn private[ccstm] (failureHistory: List[Txn.RollbackCause], ctx: ThreadContext
        ) extends impl.TxnImpl(failureHistory, ctx) with MaybeTxn with AccessMode {
  import Txn._

  /** An instance representing a single execution attempt for a single atomic
   *  block.  In almost all cases, `Txn` instances should be obtained by passing
   *  a block to the `atomic` object's `apply` method.
   *  @see edu.stanford.ppl.ccstm.atomic
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
  
  /** Values of `TxnLocal`s for this transaction, created lazily. */
  private[ccstm] var locals: java.util.IdentityHashMap[TxnLocal[_],Any] = null


  //def dynContext: Option[Txn] = Some(this)
  
  def status: Status = _status

  def forceRollback(cause: RollbackCause) { forceRollbackImpl(cause) }

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
    _callbacks.beforeCommit.add(callback, prio)
  }

  def beforeCommit(callback: Txn => Unit) { beforeCommit(callback, 0) }

  private[ccstm] def callBefore(): Boolean = {
    if (!_callbacks.beforeCommit.isEmpty) {
      for (res <- _callbacks.beforeCommit) {
        try {
          res(this)
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
    addWriteResource(writeResource, 0)
  }

  def addWriteResource(writeResource: WriteResource, prio: Int) {
    requireActiveOrValidating()
    _callbacks.writeResources.add(writeResource, prio)
  }

  private[ccstm] def writeResourcesPresent = !_callbacks.writeResources.isEmpty

  private[ccstm] def writeResourcesPrepare(): Boolean = {
    if (!_callbacks.writeResources.isEmpty) {
      for (res <- _callbacks.writeResources) {
        try {
          if (!res.prepare(this)) {
            forceRollback(VetoingWriteResourceCause(res))
          }
        } catch {
          case x => {
            forceRollback(CallbackExceptionCause(res, x))
          }
        }
        if (status ne Preparing) return false
      }
    }
    return true
  }

  private[ccstm] def writeResourcesPerformCommit() {
    if (!_callbacks.writeResources.isEmpty) {
      for (res <- _callbacks.writeResources) {
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
    if (!_callbacks.writeResources.isEmpty) {
      for (res <- _callbacks.writeResources) {
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
    _callbacks.afterCompletion.add(callback, prio, true, false)
  }

  def afterCommit(callback: Txn => Unit) { afterCommit(callback, 0) }

  def afterRollback(callback: Txn => Unit, prio: Int) {
    requireNotCompleted()
    _callbacks.afterCompletion.add(callback, prio, false, true)
  }

  def afterRollback(callback: Txn => Unit) { afterRollback(callback, 0) }

  def afterCompletion(callback: Txn => Unit) { afterCompletion(callback, 0) }

  def afterCompletion(callback: Txn => Unit, prio: Int) {
    // TODO: afterCompletion(prio: Int)(callback: Txn => Unit)
    requireNotCompleted()
    _callbacks.afterCompletion.add(callback, prio, true, true)
  }

  private[ccstm] def callAfter(readSetSize: Int, writeBufferSize: Int) {
    val s = status
    val committing = s eq Committed
    if (EnableCounters) {
      if (committing) {
        commitCounter += 1
        if (barging)
          bargingCommitCounter += 1
        if (EnableSizeHistos) {
          commitReadSetSizeHisto(histoBucket(readSetSize)) += 1
          commitWriteSetSizeHisto(histoBucket(writeBufferSize)) += 1
        }
      } else {
        s.rollbackCause.counter += 1
        if (barging)
          bargingRollbackCounter += 1
        if (EnableSizeHistos) {
          rollbackReadSetSizeHisto(histoBucket(readSetSize)) += 1
          rollbackWriteSetSizeHisto(histoBucket(writeBufferSize)) += 1
        }
      }
    }
    if (!_callbacks.afterCompletion.isEmpty) {
      _callbacks.afterCompletion.foreach(committing) { cb =>
        try {
          cb(this)
        }
        catch {
          case x => handlePostDecisionException(this, cb, x)
        }
      }
    }
  }

  private def histoBucket(n: Int): Int = 32 - Integer.numberOfLeadingZeros(n)

  def detach() = detach(impl.ThreadContext.get)

  def attach() = attach(impl.ThreadContext.get)

  private[ccstm] def detach(ctx: ThreadContext): Unit = { ctx.txn = null }

  private[ccstm] def attach(ctx: ThreadContext): Unit = { ctx.txn = this }
}

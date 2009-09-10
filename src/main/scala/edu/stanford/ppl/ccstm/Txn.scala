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
  }

  /** The <code>Status</code> for a <code>Txn</code> that may commit, but for
   *  which completion has not yet been requested.
   */
  case object Active extends Status {
    def mightCommit = true
    def mustCommit = false
  }

  /** The <code>Status</code> for a <code>Txn</code> that will not commit, but
   *  for which completion has not yet been requested. 
   */
  case object MarkedRollback extends Status {
    def mightCommit = false
    def mustCommit = false
  }

  /** The <code>Status</code> for a <code>Txn</code> that may commit and whose
   *  completion has been requested, but that has not yet acquired the
   *  resources required to guarantee that commit is possible.
   */
  case object Preparing extends Status {
    def mightCommit = true
    def mustCommit = false
  }

  /** The <code>Status</code> for a <code>Txn</code> that is guaranteed to
   *  commit, but that has not yet released all of its resources.
   */
  case object Committing extends Status {
    def mightCommit = true
    def mustCommit = true
  }

  /** The <code>Status</code> for a <code>Txn</code> that has successfully
   *  committed, applying all of its changes.
   */
  case object Committed extends Status {
    def mightCommit = true
    def mustCommit = true
  }

  /** The <code>Status</code> for a <code>Txn</code> that will definitely roll
   *  back, but that has not yet released all of its resources.  A rollback is
   *  considered to be explicit if <code>Txn.requireRollback</code> was called
   *  during a lifecycle handler's invocation, explicit otherwise.
   */
  case class RollingBack(explicit: Boolean, cause: Any) extends Status {
    def mightCommit = false
    def mustCommit = false
  }

  /** The <code>Status</code> for a <code>Txn</code> that has been completely
   *  rolled back.  A rollback is considered to be implicit if
   *  <code>Txn.triggerRollback</code> was called during a lifecycle handler's
   *  invocation, explicit otherwise.
   */
  case class Rolledback(explicit: Boolean, cause: Any) extends Status {
    def mightCommit = false
    def mustCommit = false
  }


  object ExplicitRetryException extends Exception with Stackless

  abstract class OptimisticFailureException(val target: Any) extends Exception with Stackless
  class InvalidReadException(target0: Any) extends OptimisticFailureException(target0)
  class WriteConflictException(target0: Any) extends OptimisticFailureException(target0)

  //////////////// Resources participate in a two-phase commit

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
sealed class Txn extends STM.TxnImpl

abstract class AbstractTxn(protected val retryHistory: List[Txn.Rolledback]) {
  this: Txn =>

  import Txn._

  private var _beforeCommit = EmptyPrioCallbackMap
  private var _readResources: List[ReadResource] = Nil
  private var _writeResources: List[WriteResource] = Nil
  private var _afterCommit = EmptyPrioCallbackMap
  private var _afterRollback = EmptyPrioCallbackMap

  /** Returns the transaction's current status, as of the most recent
   *  operation.  Does not validate the transaction.
   */
  def status: Status

  /** Throws <code>RollbackException</code> if the transaction is marked
   *  for rollback, otherwise throws an <code>IllegalStateException</code> if
   *  the transaction is not active.
   *  @see edu.stanford.ppl.ccstm.Txn.Active
   *  @see edu.stanford.ppl.ccstm.Txn.MarkedRollback
   */
  private[ccstm] def requireActive {
    val s = status
    if (s != Active) {
      if (s == MarkedRollback) {
        throw RollbackException
      } else {
        throw new IllegalStateException("txn.status is " + s)
      }
    }
  }

  /** Returns true if this transaction's <code>status</code> is either
   *  <code>Committed</code> or <code>Rolledback</code>.
   */
  def completed: Boolean = { val s = status ; s == Committed || s == Rolledback }

  /** Returns true if this transaction's <code>status</code> is either
   *  <code>Committing</code> or <code>Committed</code>.
   */
  def decidedCommit: Boolean = { val s = status ; s == Committing || s == Committed }

  /** Validates that the transaction is consistent with all other committed
   *  transactions and completed non-transactional accesses, throwing
   *  <code>RollbackException</code> if this transaction is not consistent.
   *  @throws edu.stanford.ppl.ccstm.RollbackException if this transaction cannot commit.
   */
  def validate { if (!validatedStatus.mightCommit) throw RollbackException }

  /** Validates that the transaction is consistent with all other committed
   *  transactions and completed non-transactional accesses, setting the
   *  status to <code>RollingBack</code> if the transaction is inconsistent,
   *  then returns the current status.
   */
  def validatedStatus: Status

  /** Arranges for <code>callback</code> to be executed as late as possible
   *  while the transaction is still active.  If the transaction rolls back
   *  then the callback may not be invoked.  If the callback throws an
   *  exception then the transaction will be rolled back.  If two callbacks
   *  have different priorities then the one with the smaller priority will be
   *  executed first.
   */
  def beforeCommit(callback: Txn => Unit, prio: Int) {
    _beforeCommit = _beforeCommit.update(prio, callback :: _beforeCommit.getOrElse(prio, Nil))
  }

  /** Calls all callbacks registered via <code>beforeCommit</code>. */
  private[ccstm] def callBeforeCommit {
    if (!_beforeCommit.isEmpty) {
      for ((_,cbs) <- _beforeCommit; cb <- cbs) cb(this)
    }
  }

  /** Enqueues a before-commit callback with the default priority of 0. */
  def beforeCommit(callback: Txn => Unit) { beforeCommit(callback, 0) }

  /** Adds a read resource to the transaction, which will participate in
   *  validation.
   */
  def addReadResource(readResource: ReadResource) {
    _readResources = readResource :: _readResources
  }

  /** Calls <code>ReadResource.validate(this)</code> until a resource that
   *  fails validation is found, returning false, or returns true if all read
   *  resources are valid.
   */
  private[ccstm] def resourceValidate: Boolean = {
    _readResources.isEmpty || !_readResources.exists(r => !r.validate(this))
  }

  /** Adds a write resource to the transaction, which will participate in the
   *  two-phase commit protocol.
   */
  def addWriteResource(writeResource: WriteResource) {
    _writeResources = writeResource :: _writeResources
  }

  /** Calls <code>WriteResource.prepare(this)</code> until a resource that is
   *  not ready to commit is found, returning false, or returns true if all
   *  write resources are prepared.
   */
  private[ccstm] def resourcePrepare: Boolean = {
    _writeResources.isEmpty || !_writeResources.exists(r => !r.prepare(this))
  }

  /** Calls <code>WriteResource.performCommit(this)</code>, returning the last
   *  exception thrown, or null if no exception occurred.
   */
  private[ccstm] def resourcePerformCommit: Exception = {
    if (_writeResources.isEmpty) {
      null
    } else {
      var failure: Exception = null
      for (r <- _writeResources) {
        try {
          r.performCommit(this)
        }
        catch {
          case x: Exception => failure = x
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

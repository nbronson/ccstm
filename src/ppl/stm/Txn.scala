/* $Id$
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

package ppl.stm


import java.util.concurrent.atomic.AtomicInteger

object Txn {

  //////////////// Status

  /** Represents the current status of a <code>Txn</code>. */
  sealed abstract class Status(mayCommit0: Boolean, willCommit0: Boolean) {
    /** True if a transaction with this status may eventually commit, false if
     *  rollback is inevitable.
     */
    val mayCommit = mayCommit0

    /** True if a transaction with this status has decided on commit, false if
     *  rollback may still occur.
     */
    val willCommit = willCommit0
  }

  /** The <code>Status</code> for a <code>Txn</code> that may commit, but for
   *  which completion has not yet been requested.
   */
  case object Active extends Status(true, false)

  /** The <code>Status</code> for a <code>Txn</code> that will not commit, but
   *  for which completion has not yet been requested. 
   */
  case object MarkedRollback extends Status(false, false)

  /** The <code>Status</code> for a <code>Txn</code> that may commit and whose
   *  completion has been requested, but that has not yet acquired the
   *  resources required to guarantee that commit is possible.
   */
  case object Preparing extends Status(true, false)

  /** The <code>Status</code> for a <code>Txn</code> that is guaranteed to
   *  commit, but that has not yet released all of its resources.
   */
  case object Committing extends Status(true, true)

  /** The <code>Status</code> for a <code>Txn</code> that has successfully
   *  committed, applying all of its changes.
   */
  case object Committed extends Status(true, true)

  /** The <code>Status</code> for a <code>Txn</code> that will definitely roll
   *  back, but that has not yet released all of its resources.  A rollback is
   *  considered to be implicit if <code>Txn.requireRollback</code> was called
   *  during a lifecycle handler's invocation, explicit otherwise.
   */
  case class RollingBack(explicit: Boolean, cause: Any) extends Status(false, false)

  /** The <code>Status</code> for a <code>Txn</code> that has been completely
   *  rolled back.  A rollback is considered to be implicit if
   *  <code>Txn.triggerRollback</code> was called during a lifecycle handler's
   *  invocation, explicit otherwise.
   */
  case class Rolledback(explicit: Boolean, cause: Any) extends Status(false, false)


  //////////////// Traits to extend for callback functionality

  trait ReadSetCallback {
    /** Called during the <code>Active</code> and/or <code>Prepared</code>
     *  states.  Validation during the <code>Preparing</code> state may be
     *  skipped if no other transactions have committed since the last
     *  validation, or if no <code>WriteSetCallback</code>s have been
     *  registered.  Implementations should call
     *  <code>txn.requireRollback</code> if <code>txn</code> is no longer
     *  valid.
     */
    def validate(txn: Txn)
  }

  trait WriteSetCallback {
    /** Called during the <code>Preparing</code> state.  All locks or other
     *  resources required to complete the commit must be acquired during this
     *  callback, or else <code>txn.requireRollback</code> must be called.
     */
    def prepare(txn: Txn)

    /** Called during the <code>Committing</code> state. */
    def performCommit(txn: Txn)

    /** Called during the <code>RollingBack</code> state. */
    def performRollback(txn: Txn)
  }

  trait PreCompletionCallback {
    /** Called during the <code>Active</code> or <code>MarkedRollback</code>
     *  states, after completion has been requested.
     */
    def beforeCompletion(txn: Txn)
  }

  trait PostCommitCallback {
    /** Called during the <code>Committed</code> state. */
    def afterCommit(txn: Txn)
  }

  trait PostRollbackCallback {
    /** Called during the <code>Rolledback</code> state. */
    def afterRollback(txn: Txn)
  }

}

abstract class AbstractTxn {
  this: ppl.stm.Txn =>

  import Txn._

  private[stm] var _readSetCallbacks: Array[ReadSetCallback] = null
  private[stm] var _readSetCount = 0

  private[stm] var _writeSetCallbacks: Array[WriteSetCallback] = null
  private[stm] var _writeSetCount = 0

  private[stm] var _preCompletionCallbacks: Array[PreCompletionCallback] = null
  private[stm] var _preCompletionCount = 0

  private[stm] var _postCommitCallbacks: Array[PostCommitCallback] = null
  private[stm] var _postCommitCount = 0

  private[stm] var _postRollbackCallbacks: Array[PostRollbackCallback] = null
  private[stm] var _postRollbackCount = 0


  /** Returns the transaction's current status, as of the most recent
   *  operation.  Does not validate the transaction.
   */
  def status: Status

  /** Validates that the transaction is consistent with all other committed
   *  transactions and completed non-transactional accesses, throwing
   *  <code>RollbackException</code> if this transaction is not consistent.
   *  @throws ppl.stm.RollbackException if this transaction cannot commit.
   */
  def validate { if (!validatedStatus.mayCommit) throw RollbackException }

  /** Validates that the transaction is consistent with all other committed
   *  transactions and completed non-transactional accesses, setting the
   *  status to <code>RollingBack</code> if the transaction is inconsistent,
   *  then returns the current status.
   */
  def validatedStatus: Status

  //////////////// Callback registration

  def addCallback(cb: ReadSetCallback) { addReadSetCallback(cb) }

  def addReadSetCallback(cb: ReadSetCallback) {
    val n = _readSetCount
    _readSetCount = n + 1
    if (n == 0 || n == _readSetCallbacks.length) _readSetCallbacks = grow(_readSetCallbacks, 32)
    _readSetCallbacks(n) = cb
  }

  private[stm] def callValidate {
    var i = _readSetCount - 1
    while (i >= 0) {
      _readSetCallbacks(i).validate(this)
      i -= 1
    }
  }


  def addCallback(cb: WriteSetCallback) { addWriteSetCallback(cb) }

  def addWriteSetCallback(cb: WriteSetCallback) {
    val n = _writeSetCount
    _writeSetCount = n + 1
    if (n == 0 || n == _writeSetCallbacks.length) _writeSetCallbacks = grow(_writeSetCallbacks, 4)
    _writeSetCallbacks(n) = cb
  }

  private[stm] def callPrepare {
    var i = _writeSetCount - 1
    while (i >= 0) {
      _writeSetCallbacks(i).prepare(this)
      i -= 1
    }
  }

  private[stm] def callPerformCommit {
    var i = _writeSetCount - 1
    while (i >= 0) {
      _writeSetCallbacks(i).performCommit(this)
      i -= 1
    }
  }

  private[stm] def callPerformRollback {
    var i = _writeSetCount - 1
    while (i >= 0) {
      _writeSetCallbacks(i).performRollback(this)
      i -= 1
    }
  }


  def addCallback(cb: PreCompletionCallback) { addPreCompletionCallback(cb) }

  def addPreCompletionCallback(cb: PreCompletionCallback) {
    val n = _preCompletionCount
    _preCompletionCount = n + 1
    if (n == 0 || n == _preCompletionCallbacks.length) _preCompletionCallbacks = grow(_preCompletionCallbacks, 4)
    _preCompletionCallbacks(n) = cb
  }

  private[stm] def callBeforeCompletion {
    var i = _preCompletionCount - 1
    while (i >= 0) {
      _preCompletionCallbacks(i).beforeCompletion(this)
      i -= 1
    }
  }

  
  def addCallback(cb: PostCommitCallback) { addPostCommitCallback(cb) }

  def addPostCommitCallback(cb: PostCommitCallback) {
    val n = _postCommitCount
    _postCommitCount = n + 1
    if (n == 0 || n == _postCommitCallbacks.length) _postCommitCallbacks = grow(_postCommitCallbacks, 4)
    _postCommitCallbacks(n) = cb
  }

  private[stm] def callAfterCommit {
    var i = _postCommitCount - 1
    while (i >= 0) {
      _postCommitCallbacks(i).afterCommit(this)
      i -= 1
    }
  }


  def addCallback(cb: PostRollbackCallback) { addPostRollbackCallback(cb) }

  def addPostRollbackCallback(cb: PostRollbackCallback) {
    val n = _postRollbackCount
    _postRollbackCount = n + 1
    if (n == 0 || n == _postRollbackCallbacks.length) _postRollbackCallbacks = grow(_postRollbackCallbacks, 4)
    _postRollbackCallbacks(n) = cb
  }

  private[stm] def callAfterRollback {
    var i = _postRollbackCount - 1
    while (i >= 0) {
      _postRollbackCallbacks(i).afterRollback(this)
      i -= 1
    }
  }

  private def grow[T <: AnyRef](xs: Array[T], initialSize: Int): Array[T] = {
    if (xs == null) {
      new Array[T](initialSize)
    }
    else {
      val z = new Array[T](xs.length * 2)
      Array.copy(xs, 0, z, 0, xs.length)
      z
    }
  }
}

class Txn extends STM.TxnImpl {
  import Txn._



  def valid: Boolean = false

  def validate() { if (!valid) throw RollbackException }

  def active: Boolean = !completed && !committing
  def committing: Boolean = false
  override def committed: Boolean = false
  def completed: Boolean = false

  def rollback() {}
  def attemptCommit(): Boolean = false

  def awaitCompletion() {}

  def markRollbackOnly() {}

  def addReadResource(handler: ReadSetCallback) {}
}

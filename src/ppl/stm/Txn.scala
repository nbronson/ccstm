/* Txn
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

package ppl.stm

object Txn {

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

  trait Synchronization {
    /** Called during the <code>Active</code> or <code>MarkedRollback</code>
     *  states, after completion has been requested.
     */
    def beforeCompletion(txn: Txn)

    /** Called during the <code>Committed</code> or <code>Rolledback</code>
     *  states.
     */
    def afterCompletion(txn: Txn)
  }

  abstract class AbstractTxn {
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

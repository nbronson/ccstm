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

  
  abstract class AbstractTxn {
    this <= ppl.stm.Txn

    private var _indexedCallbacks: Array[AnyRef] = new Array[AnyRef](nextSlot)

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

    /** Returns the indexed callback for <code>index</code> registered with
     *  this transaction, creating and registering a new callback instance if
     *  necessary.
     */
    def callback[CB](key: SharedCallbackKey[CB]): CB = {
      if (key.slot >= _indexedCallbacks.length) {
        growIndexedCallbacks
      }
      val existing = _indexedCallbacks(key.slot)
      if (existing != null) {
        existing.asInstanceOf[CB]
      }
      else {
        val fresh = key.factory(this)
        _indexedCallbacks(key.slot) = fresh
        fresh
      }
    }

    private def growIndexedCallbacks {
      val before = _indexedCallbacks
      val after = new Array[AnyRef](nextSlot)
      Array.copy(before, 0, after, 0, before.length)
      _indexedCallbacks = after
    }

    def addCallback(cb: ReadSetCallback) { addReadSetCallback(cb) }
    
    def addReadSetCallback(cb: ReadSetCallback) { callback(genericReadSetCallbacks) += cb }
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

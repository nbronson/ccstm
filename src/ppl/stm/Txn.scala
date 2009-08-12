/* Txn
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

package ppl.stm

object Txn {

  sealed abstract class Status
  case object Active extends Status
  case object MarkedRollback extends Status
  case object Preparing extends Status
  case object Prepared extends Status
  case object Committing extends Status
  case object Committed extends Status
  case object RollingBack extends Status
  case object Rolledback extends Status

  trait ReadSetCallback {
    /** Called during the Active and Prepared states.  Validation during the
     *  Preparing state may be skipped if no other transactions have committed
     *  since the last validation, or if no WriteSetCallback-s have been
     *  registered.
     */
    def validate(txn: Txn)
  }

  trait WriteSetCallback extends CommitHandler with RollbackHandler {
    /** Called during the Preparing states.  All locks required to complete
     *  the commit must be acquired during this callback, or else
     *  txn.markForRollback must be called.
     */
    def prepare(txn: Txn)
  }

  trait CommitHandler {
    /** Called during the Committing state. */
    def performCommit(txn: Txn)
  }

  trait RollbackHandler {
    /** Called during the RollingBack state. */
    def performRollback(txn: Txn)
  }

  trait Synchronization {
    /** Called during the Active and MarkedRollback states, after commit has been requested. */
    def beforeCompletion(txn: Txn)

    /** Called during the Committed and Rolledback states. */
    def afterCompletion(txn: Txn)
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

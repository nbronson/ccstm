package ppl.stm

/* Txn
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

object Txn {
  private[stm] case class Changing[T](txn: Txn, before: T, after: T) {
    def elem = if (txn.committed) after else before
  }

  private[stm] type Version = Long

  private[stm] val VersionChanging: Version = 1L  

  /** The fundamental correctness assumption of the system is that if txn R
   *  observes {@link #globalVersion} to be 
   */
  @volatile private[stm] var globalVersion: Version = 0

  private[stm] def nextNonTxnWriteVersion(): Version = 0
}

object RollbackException extends Exception {
  {
    setStackTrace(Array[StackTraceElement]())
  }
}

abstract class Txn {
  import Txn._

  def valid: Boolean

  def validate() { if (!valid) throw RollbackException }

  def active: Boolean = !completed && !committing
  def committing: Boolean
  def committed: Boolean
  def completed: Boolean

  def rollback()
  def attemptCommit(): Boolean

  def awaitCompletion()

  private[stm] var readVersion: Version
  private[stm] def commitVersion: Version
}
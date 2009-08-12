package ppl.stm

/* Txn
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

object Txn {
  private[stm] trait Changing[T] {
    def txn: Txn
    def before: T
    def after: T

    def elem = if (txn.committed) after else before
  }

  /** <tt>Version</tt> holds the bottom 32-bits of a conceptually infinite
   *  precision timestamp, which should be compared using {@link #vcmp}.  When
   *  comparing version numbers we make the assumption that the actual values
   *  being compared differ by no more than 2^31-1, which allows version
   *  numbers to grow indefinitely.  This means that we get the somewhat
   *  contradictory result that 0x7fffffff &lt; 0x8000000 &lt; 0xffffffff &lt;
   *  0x00000000 &lt; 0x7fffffff. 
   */
  private[stm] type Version = Int

  /** A comparison function for version numbers that assumes that version
   *  numbers are actually no two active version numbers differ by more than
   *  2^31-1.  Returns a negative number if <tt>lhs&lt;rhs</tt>, returns zero
   *  if <tt>lhs==rhs</tt>, otherwise returns a positive number. See {@link
   *  Version}.
   */
  private[stm] def vcmp(lhs: Version, rhs: Version): Int = {
    // Unlike most .compareTo implementations, subtraction is correct here.  It
    // is what gives us the wrapping behavior described in Version's doc.
    lhs - rhs
  }

  /** The fundamental correctness assumption of the system is that if txn R
   *  observes {@link #globalVersion} to be 
   */
  @volatile private[stm] var globalVersion: Version = 0


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

  private[stm] var readVersion: Version
  private[stm] def commitVersion: Version
}
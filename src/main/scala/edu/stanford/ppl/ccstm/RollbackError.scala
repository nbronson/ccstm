/* CCSTM - (c) 2009 Stanford University - PPL */

// RollbackError.scala

package edu.stanford.ppl.ccstm


/** The base class for all exceptions thrown by CCSTM to perform the non-local
 *  control transfer needed to roll a transaction back.
 *
 *  @author Nathan Bronson
 */
abstract class RollbackError(message: String, cause: Throwable) extends Error(message, cause) {
  def this() = this(null, null)
  def this(message: String) = this(message, null)
  def this(cause: Throwable) = this(if (cause == null) null else cause.toString, cause)
}
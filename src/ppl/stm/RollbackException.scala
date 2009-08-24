/* RollbackException
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

package ppl.stm

private[stm] object RollbackException extends Exception {
  {
    setStackTrace(Array(new StackTraceElement(
      "ppl.stm.RollbackException", "reusableException", "RollbackException.scala", 0)))
  }
}
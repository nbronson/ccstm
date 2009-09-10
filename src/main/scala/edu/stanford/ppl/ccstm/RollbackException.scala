/* RollbackException
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

package edu.stanford.ppl.ccstm

private[ccstm] object RollbackException extends Exception {
  {
    setStackTrace(Array(new StackTraceElement(
      "edu.stanford.ppl.ccstm.RollbackException", "reusableException", "RollbackException.scala", 0)))
  }
}
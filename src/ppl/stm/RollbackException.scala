package ppl.stm

/* RollbackException
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

object RollbackException extends Exception {
  {
    setStackTrace(Array(new StackTraceElement(
      "ppl.stm.RollbackException", "reusableException", "RollbackException.scala", 0)))
  }
}
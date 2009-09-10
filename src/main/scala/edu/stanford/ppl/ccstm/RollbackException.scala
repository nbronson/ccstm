/* CCSTM - (c) 2009 Stanford University - PPL */

// RollbackException.scala

package edu.stanford.ppl.ccstm


private[ccstm] object RollbackException extends Exception {
  {
    setStackTrace(Array(new StackTraceElement(
      "edu.stanford.ppl.ccstm.RollbackException", "reusableException", "RollbackException.scala", 0)))
  }
}
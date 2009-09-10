/* CCSTM - (c) 2009 Stanford University - PPL */

// Stackless.scala

package edu.stanford.ppl.ccstm


private[ccstm] object Stackless {
  val EmptyStackTrace = new Array[StackTraceElement](0)
}

/** A trait that when mixed into an exception type results in the exception
 *  having no stack trace, which reduces the performance penalty of using
 *  exceptions for non-local control flow.
 *
 *  @author Nathan Bronson
 */
trait Stackless {
  this: Exception =>

  {
    setStackTrace(Stackless.EmptyStackTrace)
  }
}
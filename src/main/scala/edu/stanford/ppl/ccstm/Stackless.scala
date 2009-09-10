/* Stackless
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

package edu.stanford.ppl.ccstm

private[ccstm] object Stackless {
  val EmptyStackTrace = new Array[StackTraceElement](0)
}

/** A trait that when mixed into an exception type results in the exception
 *  having no stack trace, which reduces the performance penalty of using
 *  exceptions for non-local control flow.
 */
trait Stackless {
  this: Exception =>

  {
    setStackTrace(Stackless.EmptyStackTrace)
  }
}
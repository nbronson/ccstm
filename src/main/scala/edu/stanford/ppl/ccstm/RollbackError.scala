/* CCSTM - (c) 2009 Stanford University - PPL */

// RollbackError.scala

package edu.stanford.ppl.ccstm


/** A reusable exception instance, thrown by CCSTM when a transaction is doomed
 *  and should not continue executing.  User code should either rethrow this
 *  exception or not catch it.
 *
 *  @author Nathan Bronson
 */
object RollbackError extends Error with Stackless
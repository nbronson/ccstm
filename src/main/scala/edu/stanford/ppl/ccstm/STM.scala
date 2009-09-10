/* CCSTM - (c) 2009 Stanford University - PPL */

// STM.scala

package edu.stanford.ppl.ccstm


/** This is the master switch that controls which STM implementation is in use.
 *  Any change requires complete recompilation of everything that uses the STM.
 *
 *  @author Nathan Bronson
 */
object STM extends impls.IndirectEagerTL2
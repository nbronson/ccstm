/* CCSTM - (c) 2009 Stanford University - PPL */

// STMImpl

package edu.stanford.ppl.ccstm


/** This is the master switch that controls which STM implementation is in use.
 *  Any change requires complete recompilation of everything that uses the STM.
 *
 *  @author Nathan Bronson
 */
object STMImpl extends impl.IndirectEagerTL2
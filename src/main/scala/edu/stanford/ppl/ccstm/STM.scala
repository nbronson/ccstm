/* STM
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

package edu.stanford.ppl.ccstm

/** This is the master switch that controls which STM implementation is in use.
 *  Any change requires complete recompilation of everything that uses the STM.
 */
object STM extends impls.IndirectEagerTL2
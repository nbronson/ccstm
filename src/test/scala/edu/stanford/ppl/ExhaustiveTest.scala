/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// ExhaustiveTest

package edu.stanford.ppl

import org.scalatest.Tag


/** A ScalaTest group for tests that may be omitted when a faster test run is
 *  desired.
 */
object ExhaustiveTest extends Tag("ExhaustiveTest")

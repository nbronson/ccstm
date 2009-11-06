/* CCSTM - (c) 2009 Stanford University - PPL */

// STMFunSuite

package edu.stanford.ppl.stm

import edu.stanford.ppl.ccstm.STM
import org.scalatest.{FunSuite, Tag}


/** Extends <code>org.scalatest.FunSuite</code> with functionality to make sure
 *  that all transactions that have been assigned slots have released them.
 */
trait STMFunSuite extends FunSuite {
  override protected def test(testName: String, testTags: Tag*)(f: => Unit) = {
    super.test(testName, testTags:_*)({
      f
      STM.Debug.assertQuiescent()
    })
  }
}
/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// Callbacks

package edu.stanford.ppl.ccstm.impl

import edu.stanford.ppl.ccstm.Txn


private[ccstm] final class Callbacks {
  val readResources: CallbackList[Txn.ReadResource] = new CallbackList[Txn.ReadResource]

  /** Includes WriteResource-s and beforeCommit callbacks. */
  val writeLikeResources: CallbackList[Txn.WriteResource] = new CallbackList[Txn.WriteResource]
  var writeResourcesPresent = false
  val afterCompletion: CallbackList[Txn => Unit] = new CallbackList[Txn => Unit]

  def clear() {
    readResources.clear()
    writeLikeResources.clear()
    writeResourcesPresent = false
    afterCompletion.clear()
  }
}

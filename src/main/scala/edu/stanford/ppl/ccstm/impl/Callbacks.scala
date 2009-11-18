/* CCSTM - (c) 2009 Stanford University - PPL */

// Callbacks

package edu.stanford.ppl.ccstm.impl

import edu.stanford.ppl.ccstm.Txn


private[ccstm] class Callbacks {
  val readResources: CallbackList[Txn.ReadResource] = new CallbackList[Txn.ReadResource]

  /** Includes WriteResource-s and beforeCommit callbacks. */
  val writeLikeResources: CallbackList[Txn.WriteResource] = new CallbackList[Txn.WriteResource]
  var writeResourcesPresent = false
  val afterCommit: CallbackList[Txn => Unit] = new CallbackList[Txn => Unit]
  val afterRollback: CallbackList[Txn => Unit] = new CallbackList[Txn => Unit]

  def clear() {
    readResources.clear()
    writeLikeResources.clear()
    writeResourcesPresent = false
    afterCommit.clear()
    afterRollback.clear()
  }
}

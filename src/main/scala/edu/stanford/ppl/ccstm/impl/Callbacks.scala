/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// Callbacks

package edu.stanford.ppl.ccstm.impl

import edu.stanford.ppl.ccstm.Txn


private[ccstm] final class Callbacks {
  val readResources = new CallbackList[Txn.ReadResource]
  val beforeCommit = new CallbackList[Txn => Unit]
  val writeResources = new CallbackList[Txn.WriteResource]
  val afterCompletion: CallbackList[Txn => Unit] = new CallbackList[Txn => Unit]

  def clear() {
    if (!readResources.isEmpty) readResources.clear()
    if (!beforeCommit.isEmpty) beforeCommit.clear()
    if (!writeResources.isEmpty) writeResources.clear()
    if (!afterCompletion.isEmpty) afterCompletion.clear()
  }
}

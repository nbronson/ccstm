/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TxnBound

package edu.stanford.ppl.ccstm.impl

import edu.stanford.ppl.ccstm._


private[ccstm] class TxnBound[T](val unbind: Ref[T],
                                 protected val handle: impl.Handle[T],
                                 txn: Txn) extends Ref.Bound[T] {

  def mode: BindingMode = txn

  def get: T = txn.get(handle)
  def map[Z](f: (T) => Z): Z = txn.map(handle, f)
  def await(pred: (T) => Boolean) { if (!pred(txn.get(handle))) txn.retry }
  def unrecordedRead: UnrecordedRead[T] = txn.unrecordedRead(handle)
  def releasableRead: ReleasableRead[T] = txn.releasableRead(handle)

  def set(v: T) { txn.set(handle, v) }
  def trySet(v: T): Boolean = txn.trySet(handle, v)

  def readForWrite: T = txn.readForWrite(handle)
  def swap(v: T): T = txn.swap(handle, v)
  def compareAndSet(before: T, after: T): Boolean = txn.compareAndSet(handle, before, after)
  def compareAndSetIdentity[A <: T with AnyRef](before: A, after: T): Boolean = txn.compareAndSetIdentity(handle, before, after)
  def transform(f: T => T) {
    // this isn't as silly as it seems, because some Bound implementations
    // override getAndTransform()
    txn.getAndTransform(handle, f)
  }
  def getAndTransform(f: T => T): T = txn.getAndTransform(handle, f)
  def tryTransform(f: T => T): Boolean = txn.tryTransform(handle, f)
  def transformIfDefined(pf: PartialFunction[T,T]): Boolean = txn.transformIfDefined(handle, pf)
}

/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// NonTxnBound

package edu.stanford.ppl.ccstm.impl


import edu.stanford.ppl.ccstm._

private[ccstm] class NonTxnBound[T](val unbind: Ref[T],
                                    protected val handle: Handle[T]) extends Ref.Bound[T] {

  def context: Option[Txn] = None

  def get: T = NonTxn.get(handle)
  def map[Z](f: (T) => Z): Z = f(NonTxn.get(handle))
  def await(pred: T => Boolean) { NonTxn.await(handle, pred) }
  def unrecordedRead: UnrecordedRead[T] = NonTxn.unrecordedRead(handle)
  def releasableRead: ReleasableRead[T] = NonTxn.releasableRead(handle)

  def set(v: T) { NonTxn.set(handle, v) }
  def tryWrite(v: T): Boolean = NonTxn.tryWrite(handle, v)

  def readForWrite: T = NonTxn.get(handle)
  def getAndSet(v: T): T = NonTxn.getAndSet(handle, v)
  def compareAndSet(before: T, after: T): Boolean = NonTxn.compareAndSet(handle, before, after)
  def compareAndSetIdentity[R <: AnyRef with T](before: R, after: T): Boolean = NonTxn.compareAndSetIdentity(handle, before, after)
  def weakCompareAndSet(before: T, after: T): Boolean = NonTxn.compareAndSet(handle, before, after)
  def weakCompareAndSetIdentity[R <: AnyRef with T](before: R, after: T): Boolean = NonTxn.compareAndSetIdentity(handle, before, after)
  def transform(f: T => T) { NonTxn.getAndTransform(handle, f) }
  def getAndTransform(f: T => T): T = NonTxn.getAndTransform(handle, f)
  def tryTransform(f: T => T): Boolean = NonTxn.tryTransform(handle, f)
  def transformIfDefined(pf: PartialFunction[T,T]): Boolean = NonTxn.transformIfDefined(handle, pf)
}

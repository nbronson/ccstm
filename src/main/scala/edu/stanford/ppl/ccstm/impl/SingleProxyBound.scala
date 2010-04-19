/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// SingleBound

package edu.stanford.ppl.ccstm.impl

import edu.stanford.ppl.ccstm._


private[ccstm] class SingleProxyBound[T](unbind0: Ref[T]) extends Ref.Bound[T] {

  private def dynBound = Txn.dynCurrentOrNull match {
    case null => unbind.escaped
    case txn: Txn => unbind.bind(txn)
  }

  def unbind: Ref[T] = unbind0
  def mode: BindingMode = Single

  def get: T = dynBound.get
  def map[Z](f: (T) => Z): Z = dynBound.map(f)
  def await(pred: T => Boolean): Unit = dynBound.await(pred)
  def unrecordedRead: UnrecordedRead[T] = dynBound.unrecordedRead
  def releasableRead: ReleasableRead[T] = dynBound.releasableRead

  def set(v: T): Unit = dynBound.set(v)
  def trySet(v: T): Boolean = dynBound.trySet(v)

  def readForWrite: T = dynBound.readForWrite
  def swap(v: T): T = dynBound.swap(v)
  def compareAndSet(before: T, after: T): Boolean = dynBound.compareAndSet(before, after)
  def compareAndSetIdentity[R <: AnyRef with T](before: R, after: T): Boolean =
    dynBound.compareAndSetIdentity(before, after)
  def transform(f: T => T): Unit = dynBound.transform(f)
  def getAndTransform(f: T => T): T = dynBound.getAndTransform(f)
  def tryTransform(f: T => T): Boolean = dynBound.tryTransform(f)
  def transformIfDefined(pf: PartialFunction[T,T]): Boolean = dynBound.transformIfDefined(pf)
}
/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// SingleView

package edu.stanford.ppl.ccstm.impl

import edu.stanford.ppl.ccstm._


private[ccstm] class SingleProxyView[T](unbind0: Ref[T]) extends Ref.View[T] {

  private def dynView = Txn.dynCurrentOrNull match {
    case null => unbind.escaped
    case txn: Txn => unbind.bind(txn)
  }

  def unbind: Ref[T] = unbind0
  def mode: BindingMode = Single

  def get: T = dynView.get
  def map[Z](f: (T) => Z): Z = dynView.map(f)
  def await(pred: T => Boolean): Unit = dynView.await(pred)
  def unrecordedRead: UnrecordedRead[T] = dynView.unrecordedRead
  def releasableRead: ReleasableRead[T] = dynView.releasableRead

  def set(v: T): Unit = dynView.set(v)
  def trySet(v: T): Boolean = dynView.trySet(v)

  def readForWrite: T = dynView.readForWrite
  def swap(v: T): T = dynView.swap(v)
  def compareAndSet(before: T, after: T): Boolean = dynView.compareAndSet(before, after)
  def compareAndSetIdentity[R <: AnyRef with T](before: R, after: T): Boolean =
    dynView.compareAndSetIdentity(before, after)
  def transform(f: T => T): Unit = dynView.transform(f)
  def getAndTransform(f: T => T): T = dynView.getAndTransform(f)
  def tryTransform(f: T => T): Boolean = dynView.tryTransform(f)
  def transformIfDefined(pf: PartialFunction[T,T]): Boolean = dynView.transformIfDefined(pf)
}
/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// SingleView

package edu.stanford.ppl.ccstm.impl

import edu.stanford.ppl.ccstm._

private[ccstm] class SingleView[@specialized(Int) T](val unbind: Ref[T], handle: Handle[T]) extends Ref.View[T] {

  def mode: AccessMode = Single
  
  def get: T = Txn.dynCurrentOrNull match {
    case null => NonTxn.get(handle)
    case txn: Txn => txn.get(handle)
  }
  def getWith[@specialized(Int,Boolean) Z](f: (T) => Z): Z = Txn.dynCurrentOrNull match {
    case null => f(NonTxn.get(handle))
    case txn: Txn => txn.getWith(handle, f)
  }
  def await(pred: T => Boolean): Unit = Txn.dynCurrentOrNull match {
    case null => NonTxn.await(handle, pred)
    case txn: Txn => if (!pred(txn.get(handle))) txn.retry
  }
  def unrecordedRead: UnrecordedRead[T] = Txn.dynCurrentOrNull match {
    case null => NonTxn.unrecordedRead(handle)
    case txn: Txn => txn.unrecordedRead(handle)
  }
  def releasableRead: ReleasableRead[T] = Txn.dynCurrentOrNull match {
    case null => NonTxn.releasableRead(handle)
    case txn: Txn => txn.releasableRead(handle)
  }
  def set(v: T): Unit = Txn.dynCurrentOrNull match {
    case null => NonTxn.set(handle, v)
    case txn: Txn => txn.set(handle, v)
  }
  def trySet(v: T): Boolean = Txn.dynCurrentOrNull match {
    case null => NonTxn.trySet(handle, v)
    case txn: Txn => txn.trySet(handle, v)
  }
  def readForWrite: T = Txn.dynCurrentOrNull match {
    case null => NonTxn.get(handle)
    case txn: Txn => txn.readForWrite(handle)
  }
  def swap(v: T): T = Txn.dynCurrentOrNull match {
    case null => NonTxn.swap(handle, v)
    case txn: Txn => txn.swap(handle, v)
  }
  def compareAndSet(before: T, after: T): Boolean = Txn.dynCurrentOrNull match {
    case null => NonTxn.compareAndSet(handle, before, after)
    case txn: Txn => txn.compareAndSet(handle, before, after)
  }
  def compareAndSetIdentity[R <: AnyRef with T](before: R, after: T): Boolean = Txn.dynCurrentOrNull match {
    case null => NonTxn.compareAndSetIdentity(handle, before, after)
    case txn: Txn => txn.compareAndSetIdentity(handle, before, after)
  }
  def transform(f: T => T): Unit = Txn.dynCurrentOrNull match {
    case null => NonTxn.getAndTransform(handle, f)
    case txn: Txn => txn.getAndTransform(handle, f)
  }
  def getAndTransform(f: T => T): T = Txn.dynCurrentOrNull match {
    case null => NonTxn.getAndTransform(handle, f)
    case txn: Txn => txn.getAndTransform(handle, f)
  }
  def tryTransform(f: T => T): Boolean = Txn.dynCurrentOrNull match {
    case null => NonTxn.tryTransform(handle, f)
    case txn: Txn => txn.tryTransform(handle, f)
  }
  def transformIfDefined(pf: PartialFunction[T,T]): Boolean = Txn.dynCurrentOrNull match {
    case null => NonTxn.transformIfDefined(handle, pf)
    case txn: Txn => txn.transformIfDefined(handle, pf)
  }
}

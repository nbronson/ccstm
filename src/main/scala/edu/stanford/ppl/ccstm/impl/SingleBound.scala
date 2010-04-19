/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// SingleBound

package edu.stanford.ppl.ccstm.impl

import edu.stanford.ppl.ccstm._

private[ccstm] class SingleBound[T](val unbind: Ref[T],
                                    val nonTxnHandle: Handle[T],
                                    val txnHandle: Handle[T]) extends Ref.Bound[T] {

  def mode: BindingMode = Single
  
  def get: T = Txn.dynCurrentOrNull match {
    case null => NonTxn.get(nonTxnHandle)
    case txn: Txn => txn.get(txnHandle)
  }
  def map[Z](f: (T) => Z): Z = Txn.dynCurrentOrNull match {
    case null => f(NonTxn.get(nonTxnHandle))
    case txn: Txn => txn.map(txnHandle, f)
  }
  def await(pred: T => Boolean): Unit = Txn.dynCurrentOrNull match {
    case null => NonTxn.await(nonTxnHandle, pred)
    case txn: Txn => if (!pred(txn.get(txnHandle))) txn.retry
  }
  def unrecordedRead: UnrecordedRead[T] = Txn.dynCurrentOrNull match {
    case null => NonTxn.unrecordedRead(nonTxnHandle)
    case txn: Txn => txn.unrecordedRead(txnHandle)
  }
  def releasableRead: ReleasableRead[T] = Txn.dynCurrentOrNull match {
    case null => NonTxn.releasableRead(nonTxnHandle)
    case txn: Txn => txn.releasableRead(txnHandle)
  }

  def set(v: T): Unit = Txn.dynCurrentOrNull match {
    case null => NonTxn.set(nonTxnHandle, v)
    case txn: Txn => txn.set(txnHandle, v)
  }
  def trySet(v: T): Boolean = Txn.dynCurrentOrNull match {
    case null => NonTxn.trySet(nonTxnHandle, v)
    case txn: Txn => txn.trySet(txnHandle, v)
  }

  def readForWrite: T = Txn.dynCurrentOrNull match {
    case null => NonTxn.get(nonTxnHandle)
    case txn: Txn => txn.readForWrite(txnHandle)
  }
  def swap(v: T): T = Txn.dynCurrentOrNull match {
    case null => NonTxn.swap(nonTxnHandle, v)
    case txn: Txn => txn.swap(txnHandle, v)
  }
  def compareAndSet(before: T, after: T): Boolean = Txn.dynCurrentOrNull match {
    case null => NonTxn.compareAndSet(nonTxnHandle, before, after)
    case txn: Txn => txn.compareAndSet(txnHandle, before, after)
  }
  def compareAndSetIdentity[R <: AnyRef with T](before: R, after: T): Boolean = Txn.dynCurrentOrNull match {
    case null => NonTxn.compareAndSetIdentity(nonTxnHandle, before, after)
    case txn: Txn => txn.compareAndSetIdentity(txnHandle, before, after)
  }
  def transform(f: T => T): Unit = Txn.dynCurrentOrNull match {
    case null => NonTxn.getAndTransform(nonTxnHandle, f)
    case txn: Txn => txn.getAndTransform(txnHandle, f)
  }
  def getAndTransform(f: T => T): T = Txn.dynCurrentOrNull match {
    case null => NonTxn.getAndTransform(nonTxnHandle, f)
    case txn: Txn => txn.getAndTransform(txnHandle, f)
  }
  def tryTransform(f: T => T): Boolean = Txn.dynCurrentOrNull match {
    case null => NonTxn.tryTransform(nonTxnHandle, f)
    case txn: Txn => txn.tryTransform(txnHandle, f)
  }
  def transformIfDefined(pf: PartialFunction[T,T]): Boolean = Txn.dynCurrentOrNull match {
    case null => NonTxn.transformIfDefined(nonTxnHandle, pf)
    case txn: Txn => txn.transformIfDefined(txnHandle, pf)
  }
}

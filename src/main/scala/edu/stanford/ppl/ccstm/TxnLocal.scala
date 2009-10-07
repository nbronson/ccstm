/* CCSTM - (c) 2009 Stanford University - PPL */

// TxnLocal

package edu.stanford.ppl.ccstm


object TxnLocal {
  trait Bound[T] {
    def txn: Txn
    def get: T
    def set(v: T)
  }
}

class TxnLocal[T] {

  protected def initialValue: T = null.asInstanceOf[T]

  def get(implicit txn: Txn) = getImpl(txn)

  def set(v: T)(implicit txn: Txn) = setImpl(txn, v)

  def bind(implicit txn0: Txn) = new TxnLocal.Bound[T] {
    def txn = txn0
    def get: T = getImpl(txn0)
    def set(v: T) = setImpl(txn0, v)
  }

  private def locals(txn: Txn): java.util.IdentityHashMap[TxnLocal[_],Any] = {
    if (null == txn.locals) {
      txn.locals = new java.util.IdentityHashMap[TxnLocal[_],Any]
    }
    txn.locals
  }

  private def getImpl(txn: Txn): T = locals(txn).get(this).asInstanceOf[T]

  private def setImpl(txn: Txn, v: T) { locals(txn).put(this, v) }
}
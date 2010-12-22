/* scala-stm - (c) 2009-2010, Stanford University, PPL */

package edu.stanford.ppl.ccstm.experimental.impl

import edu.stanford.ppl.ccstm._
import edu.stanford.ppl.ccstm.experimental._

class HashTrieTMap[A, B] private (root0: Ref.Bound[TxnHashTrie.Node[A, B]]) extends TxnHashTrie[A, B](root0) with TMap[A, B] {
  def this() = this(Ref(TxnHashTrie.emptyMapNode[A,B]).nonTxn)

  def bind(implicit txn0: Txn): TMap.Bound[A, B] = new TMap.AbstractTxnBound[A,B,HashTrieTMap[A,B]](txn0, this) {
    def elements: Iterator[(A,B)] = throw new UnsupportedOperationException
  }

  def nonTxn: TMap.Bound[A,B] = new TMap.AbstractNonTxnBound[A,B,HashTrieTMap[A,B]](this) {
    def get(key: A): Option[B] = ntGet(key)
    override def put(key: A, value: B): Option[B] = ntPut(key, value)
    override def removeKey(key: A): Option[B] = ntRemove(key)
    def elements: Iterator[(A, B)] = ntMapIterator()
  }

  def size(implicit txn: Txn): Int = throw new UnsupportedOperationException

  def isEmpty(implicit txn: Txn): Boolean = throw new UnsupportedOperationException

  def get(key: A)(implicit txn: Txn): Option[B] = txGet(key)
  def put(key: A, value: B)(implicit txn: Txn): Option[B] = txPut(key, value)
  def removeKey(key: A)(implicit txn: Txn): Option[B] = txRemove(key)
}

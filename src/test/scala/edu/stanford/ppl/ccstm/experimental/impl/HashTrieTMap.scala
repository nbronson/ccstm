/* scala-stm - (c) 2009-2010, Stanford University, PPL */

package edu.stanford.ppl.ccstm.experimental.impl

import edu.stanford.ppl.ccstm._
import edu.stanford.ppl.ccstm.experimental._

class HashTrieTMap[A, B] private (private val root: Ref.Bound[TxnHashTrie.Node[A, B]]) extends TMap[A, B] {
  def this() = this(Ref(TxnHashTrie.emptyMapNode[A,B]).nonTxn)

  def bind(implicit txn0: Txn): TMap.Bound[A, B] = new TMap.AbstractTxnBound[A,B,HashTrieTMap[A,B]](txn0, this) {
    def elements: Iterator[(A,B)] = throw new UnsupportedOperationException
  }

  def nonTxn: TMap.Bound[A,B] = new TMap.AbstractNonTxnBound[A,B,HashTrieTMap[A,B]](this) {
    def get(key: A): Option[B] = TxnHashTrie.get(root, key)
    override def put(key: A, value: B): Option[B] = TxnHashTrie.put(root, key, value)
    override def removeKey(key: A): Option[B] = TxnHashTrie.remove(root, key)
    def elements: Iterator[(A, B)] = TxnHashTrie.mapIterator(root)
  }

  def size(implicit txn: Txn): Int = throw new UnsupportedOperationException

  def isEmpty(implicit txn: Txn): Boolean = throw new UnsupportedOperationException

  def get(key: A)(implicit txn: Txn): Option[B] = TxnHashTrie.get(root.unbind, key)
  def put(key: A, value: B)(implicit txn: Txn): Option[B] = TxnHashTrie.put(root.unbind, key, value)
  def removeKey(key: A)(implicit txn: Txn): Option[B] = TxnHashTrie.remove(root.unbind, key)
}

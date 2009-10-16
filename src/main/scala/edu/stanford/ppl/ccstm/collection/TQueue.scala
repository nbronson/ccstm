/* CCSTM - (c) 2009 Stanford University - PPL */

// TQueue

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._


object TQueue {
  trait Bound[T] {
    def unbind: TQueue[T]
    def context: Option[Txn]
    def isEmpty: Boolean
    def dequeue(): T
    def enqueue(v: T)
  }

  trait Refs[T] {
    def isEmpty: Ref[Boolean]
    def peek: Ref[T]
    def bind(implicit txn: Txn): BoundRefs[T]
    def nonTxn: BoundRefs[T]
  }

  trait BoundRefs[T] {
    def unbind: Refs[T]
    def context: Option[Txn]
    def isEmpty: Ref.Bound[Boolean]
    def peek: Ref.Bound[T]
  }

  private[TQueue] class Node[T] {

  }
}

/** A transactional queue with exact FIFO behavior.  Enqueues and dequeues will
 *  be linearized in transaction commit order.  Parallel enqueue is likely to
 *  work better than parallel dequeue, since enqueuing is easier to defer and
 *  commute than dequeuing.
 */
abstract class TQueue[T] {
  import TQueue._

  def isEmpty(implicit txn: Txn): Boolean
  def dequeue()(implicit txn: Txn): T
  def enqueue(v: T)(implicit txn: Txn)

  def bind(implicit txn: Txn): Bound[T]
  def nonTxn: Bound[T]

  def refs: Refs[T]

  //////////////// implementation

  private val _head = Ref[Node[T]](null)
}
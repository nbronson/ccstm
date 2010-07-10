/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// package

package edu.stanford.ppl

/** CCSTM is a library-based software transactional memory.  The four core
 *  entities are
 *
 *  - trait [[edu.stanford.ppl.ccstm.Ref]] - instances holds mutable state that
 *    will be managed by the STM.  Create these using the companion object.
 *  - class [[edu.stanford.ppl.ccstm.Txn]] - the state of a particular attempt
 *    to execute an atomic block.  Direct (non-view) access to a `Ref` requires
 *    an implicit `Txn` to be in scope.
 *  - object [[edu.stanford.ppl.ccstm.atomic]] - creates `Txn`s and runs blocks
 *    of user code atomically.
 *  - trait [[edu.stanford.ppl.ccstm.Ref.View]] - allows access to a `Ref`
 *    without a `Txn`.
 *
 *  A simple example{{{
 *    import edu.stanford.ppl.ccstm._
 *
 *    // immutable data to store inside Refs
 *    case class Point(x: Int, y: Int) {
 *      def above(p: Point) = y > p.y
 *      def rightOf(p: Point) = x > p.x
 *    }
 *    val origin = Point(0, 0)
 *
 *    // some transactionally-managed mutable state
 *    val top = Ref(origin)
 *    val right = Ref(origin)
 *    val left = Ref(origin)
 *    val bottom = Ref(origin)
 *
 *    // a simple transactional update
 *    def update(p: Point) = {
 *      atomic { implicit t =>
 *        if (p above top()) top() = p
 *        if (bottom() above p) bottom() = p
 *        if (p rightOf right()) right() = p
 *        if (left() rightOf p) left() = p
 *      }
 *    }
 *  }}}
 */
package object ccstm {

  /** Equivalent to `implicitly[Txn].retry()`. */
  def retry(implicit txn: edu.stanford.ppl.ccstm.Txn): Nothing = txn.retry()

  /** This is the first half of the machinery for implementing `orAtomic`. */
  implicit def delayAtomic[A](lhs: => A) = new edu.stanford.ppl.ccstm.atomic.Delayed(lhs)
}

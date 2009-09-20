/* CCSTM - (c) 2009 Stanford University - PPL */

// ConstantRef

package edu.stanford.ppl.ccstm.collection


import edu.stanford.ppl.ccstm.Ref.BoundSource

/** A concrete implementation of <code>Ref</code> that holds a single
 *  unchanging value.  This is useful for code where some references may be
 *  dynamically known to be constant.
 *
 *  @author Nathan Bronson
 */
class ConstantRef[T](value0: T) extends Ref[T] {

  private abstract class Bnd extends Ref.Bound[T] {
    def unbind: Ref[T] = ConstantRef.this
    def get: T = value0
    def map[Z](f: T => Z): Z = f(value0)
    def await(pred: T => Boolean) = {
      if (!pred(value0)) throw new IllegalStateException("predicate on ConstantRef will never become true")
    }

    def unrecordedRead: UnrecordedRead[T] = new UnrecordedRead[T] {
      def context: Option[Txn] = Bnd.this.context
      def value: T = value0
      def stillValid: Boolean = true
      def recorded: Boolean = true
    }

    def readForWrite: T = value0

    //////////// mutating operations

    private def failure(op: String) = new IllegalStateException(op + ": not allowed for ConstantRef")

    // TODO: should failing compare-and-set be allowed?

    def set(v: T) = throw failure("set")
    def tryWrite(v: T): Boolean = throw failure("tryWrite")
    def compareAndSet(before: T, after: T): Boolean = throw failure("compareAndSet")
    def compareAndSetIdentity[A <: T with AnyRef](before: A, after: T): Boolean = throw failure("compareAndSetIdentity")
    def weakCompareAndSet(before: T, after: T): Boolean = throw failure("weakCompareAndSet")
    def weakCompareAndSetIdentity[A <: T with AnyRef](before: A, after: T): Boolean = throw failure("weakCompareAndSetIdentity")
    def transform(f: T => T) = throw failure("transform")
    def transformIfDefined(pf: PartialFunction[T,T]): Boolean = throw failure("transformIfDefined")

    override def toString: String = "ConstantRef.Bound(" + value0 + ")"
  }

  def bind(implicit txn: Txn): Ref.Bound[T] = new Bnd { def context = Some(txn) }

  def nonTxn: Ref.Bound[T] = new Bnd { def context = None }

  override def toString: String = "ConstantRef(" + value0 + ")"
}
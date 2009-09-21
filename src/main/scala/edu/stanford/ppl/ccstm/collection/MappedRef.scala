/* CCSTM - (c) 2009 Stanford University - PPL */

// TRef

package edu.stanford.ppl.ccstm.collection


/** A proxy <code>Ref</code> implementation that allows values to be
 *  transformed when they are passed to or returned from the underlying
 *  <code>Ref</code>.
 *  
 *  @author Nathan Bronson
 */
abstract class MappedRef[A,B](val underlying: Ref[A]) extends Ref[B] {

  //////////////// abstract methods

  protected def toUnderlying(v: B): A
  protected def fromUnderlying(v: A): B

  //////////////// implementation

  private def lowerF[Z](f: B => Z) = {
    (v: A) => f(fromUnderlying(v))
  }

  private def lowerTransform(f: B => B) = {
    (v: A) => toUnderlying(f(fromUnderlying(v)))
  }

  private def lowerPartialTransform(pf: PartialFunction[B,B]) = {
    new PartialFunction[A,A] {
      def isDefinedAt(v: A) = pf.isDefinedAt(fromUnderlying(v))
      def apply(v: A) = toUnderlying(pf(fromUnderlying(v)))
    }
  }

  def bind(implicit txn: Txn): Ref.Bound[B] = new Wrapped(underlying.bind)

  def nonTxn: Ref.Bound[B] = new Wrapped(underlying.nonTxn)

  private class Wrapped(impl: Ref.Bound[A]) extends Ref.Bound[B] {

    def unbind = MappedRef.this
    def context = impl.context

    // BoundSink

    def get = fromUnderlying(impl.get)
    def map[Z](f: B => Z) = impl.map(lowerF(f))
    def await(pred: B => Boolean) { impl.await(lowerF(pred)) }
    def unrecordedRead = new UnrecordedRead[B] {
      private val uImpl = impl.unrecordedRead

      def context: Option[Txn] = uImpl.context
      def value: B = fromUnderlying(uImpl.value)
      def stillValid: Boolean = uImpl.stillValid
      def recorded: Boolean = uImpl.recorded
    }

    // BoundSource

    def set(v: B) { impl.set(toUnderlying(v)) }
    def tryWrite(v: B) = impl.tryWrite(toUnderlying(v))
    def freeze() { impl.freeze() }

    // Bound

    def readForWrite = fromUnderlying(impl.readForWrite)
    def compareAndSet(before: B, after: B) = {
      impl.compareAndSet(toUnderlying(before), toUnderlying(after))
    }
    def compareAndSetIdentity[R <: B with AnyRef](before: R, after: B): Boolean = {
      impl.compareAndSetIdentity(toUnderlying(before).asInstanceOf[A with AnyRef], toUnderlying(after))
    }
    def weakCompareAndSet(before: B, after: B) = {
      impl.weakCompareAndSet(toUnderlying(before), toUnderlying(after))
    }
    def weakCompareAndSetIdentity[R <: B with AnyRef](before: R, after: B): Boolean = {
      impl.weakCompareAndSetIdentity(toUnderlying(before).asInstanceOf[A with AnyRef], toUnderlying(after))
    }
    def transform(f: B => B) {
      impl.transform(lowerTransform(f))
    }
    def transformIfDefined(pf: PartialFunction[B, B]) = {
      impl.transformIfDefined(lowerPartialTransform(pf))
    }
  }
}
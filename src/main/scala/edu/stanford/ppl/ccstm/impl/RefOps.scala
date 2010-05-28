/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// RefOps.scala

package edu.stanford.ppl.ccstm
package impl

/** The default implementation of `Ref`'s operations. */
private[ccstm] trait RefOps[T] extends Ref[T] {

  /** Override this to provide the handle that this `RefOps` uses. */
  private[ccstm] def handle: Handle[T]

  //////////////// Source stuff

  def get(implicit txn: Txn): T = txn.get(handle)
  def getWith[Z](f: (T) => Z)(implicit txn: Txn): Z = txn.getWith(handle, f)

  //////////////// Sink stuff

  def set(v: T)(implicit txn: Txn) { txn.set(handle, v) }

  //////////////// Ref stuff

  def swap(v: T)(implicit txn: Txn): T = txn.swap(handle, v)

  def transform(f: T => T)(implicit txn: Txn) {
    // only sub-types of Ref actually perform deferral, the base implementation
    // evaluates f immediately
    txn.getAndTransform(handle, f)
  }

  def transformIfDefined(pf: PartialFunction[T,T])(implicit txn: Txn): Boolean = {
    txn.transformIfDefined(handle, pf)
  }

  def bind(implicit txn: Txn): Ref.View[T] = new TxnView(this, handle, txn)

  def single: Ref.View[T] = new SingleView(this, handle)

  def escaped: Ref.View[T] = new EscapedView(this, handle)

  private[ccstm] def embalm(identity: Int) { STMImpl.embalm(identity, handle) }

  private[ccstm] def resurrect(identity: Int) { STMImpl.resurrect(identity, handle) }

  override def hashCode: Int = {
    val h = handle
    STMImpl.hash(h.ref, h.offset)
  }

  override def equals(rhs: Any): Boolean = {
    (this eq rhs.asInstanceOf[AnyRef]) || (rhs match {
      case r: RefOps[_] => {
        val h1 = handle
        val h2 = r.handle
        (h1.ref eq h2.ref) && (h1.offset == h2.offset)
      }
      case r: Ref[_] => {
        // give the rhs the opportunity to compare itself to us
        r equals this
      }
      case _ => false
    })
  }
}

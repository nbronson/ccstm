/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// IdentityPair

package edu.stanford.ppl.ccstm.collection


/** <code>IdentityPair</code> works like <code>Tuple2</code>, except that it
 *  uses only identity equality to compare its elements, not value equality.
 */
private[ccstm] case class IdentityPair[+A, +B](_1: A, _2: B) {
  // TODO: figure out what this class should extend

  override def hashCode = {
    1 + System.identityHashCode(_1) + 37 * System.identityHashCode(_2)
  }

  override def equals(rhs: Any): Boolean = {
    if (rhs.asInstanceOf[AnyRef] eq this) {
      true
    } else {
      rhs match {
        case ip: IdentityPair[_,_] => (_1.asInstanceOf[AnyRef] eq ip._1.asInstanceOf[AnyRef]) && (_2.asInstanceOf[AnyRef] eq ip._2.asInstanceOf[AnyRef])
        case _ => false
      }
    }
  }
}

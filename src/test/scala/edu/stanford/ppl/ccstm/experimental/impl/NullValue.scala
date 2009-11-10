/* SnapTree - (c) 2009 Stanford University - PPL */

// NullValue

package edu.stanford.ppl.ccstm.experimental.impl


private[impl] object NullValue {
  def encode[B](value: B): AnyRef = {
    val v = value.asInstanceOf[AnyRef]
    if (null eq v) this else v
  }

  def decode[B](packed: AnyRef): B = {
    (if (packed eq this) null else packed).asInstanceOf[B]
  }

  def decodeOption[B](packed: AnyRef): Option[B] = {
    if (null eq packed) None else Some(decode(packed))
  }
}

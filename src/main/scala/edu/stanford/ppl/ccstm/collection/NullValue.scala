/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// NullValue

package edu.stanford.ppl.ccstm.collection


private[collection] object NullValue {
  def encode[@specialized(Int) B](value: B): AnyRef = {
    val v = value.asInstanceOf[AnyRef]
    if (null eq v) this else v
  }

  def encodeOption[@specialized(Int) B](vOpt: Option[B]): AnyRef = {
    vOpt match {
      case Some(v) => encode(v)
      case None => null
    }
  }

  def decode[@specialized(Int) B](packed: AnyRef): B = {
    (if (packed eq this) null else packed).asInstanceOf[B]
  }

  def decodeOption[@specialized(Int) B](packed: AnyRef): Option[B] = {
    if (null eq packed) None else Some(decode(packed))
  }
}

/* CCSTM - (c) 2009 Stanford University - PPL */

// DefaultValue

package edu.stanford.ppl.ccstm.impl


object DefaultValue {
  def apply[T](implicit manifest: scala.reflect.Manifest[T]): T = {
    manifest.erasure.toString match {
      case "boolean" => false.asInstanceOf[T]
      case "byte" => 0.asInstanceOf[Byte].asInstanceOf[T]
      case "short" => 0.asInstanceOf[Short].asInstanceOf[T]
      case "char" => '\0'.asInstanceOf[T]
      case "int" => 0.asInstanceOf[T]
      case "long" => 0L.asInstanceOf[T]
      case "double" => 0.0.asInstanceOf[T]
      case "float" => 0.0f.asInstanceOf[T]
      case _ => null.asInstanceOf[T]
    }
  }
}
package edu.stanford.ppl.stm.stmbench

import stmbench7.core._
import edu.stanford.ppl.ccstm._
import scala.collection.immutable.WrappedString

class DocumentImpl(id: Int, title: String, text0: String) extends Document {
  val text = Ref(text0).single
  val part = Ref(null : CompositePart).single

  def getDocumentId = id
  def getTitle = title

  def getCompositePart = part()
  def setPart(v: CompositePart) { part() = v }

  def nullOperation() {}

  def getText = text()
  def searchText(symbol: Char) = new WrappedString(text()).count(_ == symbol)
  def replaceText(from: String, to: String) = {
    val f = text transformIfDefined {
      case s if s.startsWith(from) => s.replaceFirst(from, to)
    }
    if (f) 1 else 0
  }
  def textBeginsWith(prefix: String) = text().startsWith(prefix)
}

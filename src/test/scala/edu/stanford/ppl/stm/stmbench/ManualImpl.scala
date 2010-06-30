package edu.stanford.ppl.stm.stmbench

import stmbench7.core._
import edu.stanford.ppl.ccstm._
import scala.collection.immutable.WrappedString

class ManualImpl(id: Int, title0: String, text0: String) extends Manual {
  val title = Ref(title0).single
  val text = Ref(text0).single
  val module = Ref(null : Module).single

  def getId = id
  def getTitle = title()
  def getText = text()
  def getModule = module()

  def setModule(v: Module) { module() = v }

  def countOccurences(ch: Char) = new WrappedString(text()).count(_ == ch)

  def checkFirstLastCharTheSame = {
    val t = text()
    if (t.charAt(0) == t.charAt(t.length - 1)) 1 else 0
  }

  def startsWith(ch: Char) = text().charAt(0) == ch

  def replaceChar(from: Char, to: Char) = {
    text.transform(_.replace(from, to))
    countOccurences(to)
  }
}

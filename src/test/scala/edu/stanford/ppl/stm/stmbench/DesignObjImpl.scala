package edu.stanford.ppl.stm.stmbench

import stmbench7.core._
import edu.stanford.ppl.ccstm._

class DesignObjImpl(id: Int, typ: String, buildDate0: Int) extends DesignObj {
  val bd = Ref(buildDate0).single

	def getId = id
  def getType = typ
	def getBuildDate = bd()
	def updateBuildDate() { if (bd() % 2 == 0) bd -= 1 else bd += 1 }
	def nullOperation() {}
}

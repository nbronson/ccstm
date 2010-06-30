package edu.stanford.ppl.stm.stmbench

import stmbench7.core._
import edu.stanford.ppl.ccstm._

class ModuleImpl(id: Int, typ: String, buildDate: Int, man: Manual
        ) extends DesignObjImpl(id, typ, buildDate) with Module {

  val designRoot = Ref(null : ComplexAssembly).single

  man.setModule(this)

  def setDesignRoot(v: ComplexAssembly ) { designRoot() = v }
  def getDesignRoot = designRoot()
  def getManual = man
}

package edu.stanford.ppl.stm.stmbench

import stmbench7.core._
import edu.stanford.ppl.ccstm._

abstract class AssemblyImpl(id: Int, typ: String, buildDate: Int, module0: Module, superAssembly0: ComplexAssembly
    ) extends DesignObjImpl(id, typ, buildDate) with Assembly {

  private val module = Ref(module0).single
  private val superAssembly = Ref(superAssembly0).single

  def getSuperAssembly() = superAssembly()
  def getModule() = module()
    
  def clearPointers() {
    superAssembly() = null
    module() = null
  }
}

package edu.stanford.ppl.stm.stmbench

import stmbench7.core._
import edu.stanford.ppl.ccstm._
import stmbench7.Parameters

class ComplexAssemblyImpl(id: Int, typ: String, buildDate: Int, module: Module, superAssembly: ComplexAssembly
        ) extends AssemblyImpl(id, typ, buildDate, module, superAssembly) with ComplexAssembly {

  val subAssemblies = Ref(Set.empty[Assembly]).single
  val level = if (superAssembly == null) Parameters.NumAssmLevels else superAssembly.getLevel - 1

  def addSubAssembly(assembly: Assembly) = {
    if(assembly.isInstanceOf[BaseAssembly] && level != 2)
      throw new RuntimeError("ComplexAssembly.addAssembly: BaseAssembly at wrong level!");
    	
    subAssemblies transformIfDefined {
      case x if !x.contains(assembly) => x + assembly
    }
  }

  def removeSubAssembly(assembly: Assembly) = {
    subAssemblies transformIfDefined {
      case x if x.contains(assembly) => x - assembly
    }
  }

  def getSubAssemblies = new ImmutableSetImpl[Assembly](subAssemblies())

  def getLevel = level.asInstanceOf[Short]

  override def clearPointers() {
    super.clearPointers()
    subAssemblies() = null
  }
}

package edu.stanford.ppl.stm.stmbench

import stmbench7.core._
import edu.stanford.ppl.ccstm._

class BaseAssemblyImpl(id: Int, typ: String, buildDate: Int, module: Module, superAssembly: ComplexAssembly
        ) extends AssemblyImpl(id, typ, buildDate, module, superAssembly) with BaseAssembly {

  val components = Ref(List.empty[CompositePart]).single // the original BagImpl was just an ArrayList

  def addComponent(component: CompositePart) {
    components.transform(component :: _)
    component.addAssembly(this)
  }

  def removeComponent(component: CompositePart) = {
    val f = components transformIfDefined {
      case x if x contains component => x filterNot { _ == component }
    }
    if (f) component.removeAssembly(this)
    f
  }

  def getComponents = new ImmutableSeqImpl[CompositePart](components())

  override def clearPointers() {
    super.clearPointers()
    components() = null
   }
}

/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// CCSTMInitializer

package edu.stanford.ppl.stm.stmbench

import stmbench7.backend._
import stmbench7.core._
import java.lang.String
import stmbench7.{Parameters, OperationExecutor, OperationExecutorFactory, SynchMethodInitializer}
import edu.stanford.ppl.ccstm._
import stmbench7.impl.core.ConnectionImpl
import stmbench7.impl.backend.ImmutableCollectionImpl

class CCSTMInitializer extends SynchMethodInitializer {
  def createOperationExecutorFactory = new OperationExecutorFactory {

    val timestamp = Ref(0)

    def createOperationExecutor(op: Operation) = new OperationExecutor {
      var lastTS = 0

      def execute(): Int = {
        atomic { implicit t =>
          val z = op.performOperation()
          if (Parameters.sequentialReplayEnabled) {
            timestamp += 1
            lastTS = timestamp()
          }
          z
        }
      }

      def getLastOperationTimestamp = lastTS
    }
  } 

  def createBackendFactory = new BackendFactory {

    def createLargeSet[E]: LargeSet[E] =
      new LargeSet[E] // a couple hundred, not ordered

    def createIndex[K <: IndexKey, V] =
      new Index[K,V]() // ordered

    def createIdPool(maxNumberOfIds: Int) =
      new IdPool(maxNumberOfIds)
  }

  def createDesignObjFactory = new DesignObjFactory {

    def createAtomicPart(id0: Int, typ0: String, bd0: Int, x0: Int, y0: Int) = new AtomicPart {
      val bd = Ref(bd0).single
      val x = Ref(x0)
      val y = Ref(y0)
      val partOf = Ref(null : CompositePart)
      val from = Ref(Nil : List[Connection])
      val to = Ref(Nil : List[Connection])

      // DesignObj
      def getId = id0
      def getBuildDate = bd()
      def getType = typ0
      def updateBuildDate() { if (bd() % 2 == 0) bd -= 1 else bd += 1 }
      def nullOperation() {}

      // AtomicPart
      def connectTo(dest: AtomicPart, typ: String, length: Int) {
        val c = new ConnectionImpl(this, dest, typ, length)
        to.single.transform(c :: _)
        dest.addConnectionFromOtherPart(c.getReversed)
      }
      def addConnectionFromOtherPart(c: Connection) { from.single.transform(c :: _)}
      def setCompositePart(po: CompositePart) { partOf() = po }
      def getNumToConnections = to.single.size()
      def getToConnections = new ImmutableCollectionImpl[Connection](to.single())
      def getFromConnections = new ImmutableCollectionImpl[Connection](to.single())
      def getPartOf = partOf.single()
      def swapXY {
        atomic { implicit t =>
          val tmp = x()
          x() = y()
          y() = tmp
        }
      }
      def getX = x.single()
      def getY = y.single()
      def clearPointers() {
        atomic { implicit t =>
          x() = 0
          y() = 0
          to() = null
          from() = null
          partOf() = null
        }
      }

      // Comparable[AtomicPart]
      def compareTo(rhs: AtomicPart) = getId - rhs.getId
    }

    def createConnection(from: AtomicPart, to: AtomicPart, typ: String, length: Int) =
        new ConnectionImpl(from, to, typ, length)
    
    def createBaseAssembly(id: Int, typ: String, buildDate: Int, module: Module, superAssembly: ComplexAssembly) =
        new BaseAssembly(id, typ, buildDate, module, superAssembly)

    def createComplexAssembly(id: Int, typ: String, buildDate: Int, module: Module, superAssembly: ComplexAssembly) =
        new ComplexAssembly(id, typ, buildDate, module, superAssembly)

    def createCompositePart(id: Int, typ: String, buildDate: Int, documentation: Document) =
        new CompositePart(id, typ, buildDate, documentation)

    def createDocument(id: Int, title: String, text: String) =
        new Document(id, title, text)

    def createManual(id: Int, title: String, text: String) =
        new Manual(id, title, text)
    
    def createModule(id: Int, typ: String, buildDate: Int, man: Manual) =
        new Module(id, typ, buildDate, man)
  }
}
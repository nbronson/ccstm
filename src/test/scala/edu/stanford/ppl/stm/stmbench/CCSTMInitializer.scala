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

    // a couple hundred, not ordered, except for huge numbers of discarded
    // sets with exactly 1 element
    def createLargeSet[E](): LargeSet[E] = new LargeSetImpl.PredicatedHash[E]
    //def createLargeSet[E](): LargeSet[E] = new LargeSetImpl.TwoStage[E]

    // ordered
    def createIndex[K <: IndexKey,V](): Index[K,V] = new IndexImpl.BoxedImmutable[K,V]

    def createIdPool(maxNumberOfIds: Int): IdPool = new IdPoolImpl.BoxedList(maxNumberOfIds)
  }

  def createDesignObjFactory = new DesignObjFactory {

    def createAtomicPart(id: Int, typ: String, bd: Int, x: Int, y: Int) =
        new AtomicPartImpl(id, typ, bd, x, y)

    def createConnection(from: AtomicPart, to: AtomicPart, typ: String, length: Int) =
        new ConnectionImpl(from, to, typ, length)
    
    def createBaseAssembly(id: Int, typ: String, buildDate: Int, module: Module, superAssembly: ComplexAssembly) =
        new BaseAssemblyImpl(id, typ, buildDate, module, superAssembly)

    def createComplexAssembly(id: Int, typ: String, buildDate: Int, module: Module, superAssembly: ComplexAssembly) =
        new ComplexAssemblyImpl(id, typ, buildDate, module, superAssembly)

    def createCompositePart(id: Int, typ: String, buildDate: Int, documentation: Document) =
        new CompositePartImpl(id, typ, buildDate, documentation)

    def createDocument(id: Int, title: String, text: String) =
        new DocumentImpl(id, title, text)

    def createManual(id: Int, title: String, text: String) =
        new ManualImpl(id, title, text)
    
    def createModule(id: Int, typ: String, buildDate: Int, man: Manual) =
        new ModuleImpl(id, typ, buildDate, man)
  }
}
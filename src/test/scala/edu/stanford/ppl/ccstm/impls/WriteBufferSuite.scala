/* CCSTM - (c) 2009 Stanford University - PPL */

// WriteBufferSuite

package edu.stanford.ppl.ccstm.impls


import _root_.scala.collection.jcl.IdentityHashMap
import _root_.scala.collection.mutable.HashMap
import org.scalatest.FunSuite

class WriteBufferSuite extends FunSuite {
  case class Put(ref: AnyRef, offset: Int, specValue: Any)

  private class NestedMap extends IdentityHashMap[AnyRef,HashMap[Int,Any]] {
    override def apply(ref: AnyRef) = {
      get(ref) match {
        case Some(m) => m
        case None => {
          val m = new HashMap[Int,Any]
          put(ref, m)
          m
        }
      }
    }
  }

  def runTest(steps: Iterable[Put]) {
    val wb = new WriteBuffer
    val reference = new NestedMap
    for (Put(ref, offset, specValue) <- steps) {
      val expected = reference(ref).getOrElse(offset, null)
      val actual = wb.get[Any](ref, offset, null)
      assert(expected === actual)
      reference(ref)(offset) = specValue
      wb.put(ref, offset, specValue)

      // validate the set
      val fresh = new NestedMap
      wb.visit(new WriteBuffer.Visitor {
        def visit(ref: AnyRef, offset: Int, specValue: Any): Boolean = {
          fresh(ref)(offset) = specValue
          assert(wb.get(ref, offset, null) == specValue)
          true
        }
      })
      assert(fresh.size == reference.size)
      for (ref <- fresh.keys) {
        val f = fresh.get(ref).get
        val r = fresh.get(ref).get
        assert(f.size === r.size)
        for (offset <- f.keys) {
          assert(f(offset) === r(offset))
        }
      }
    }
  }

  test("single entry") {
    runTest(List(
        Put("ref1", 0, "value"),
        Put("ref1", 0, "value2")
      ))
  }

  test("many refs") {
    runTest(for (i <- 0 until 200) yield Put("ref" + i, 0, "value" + i))
  }

  test("many offsets") {
    runTest(for (i <- 0 until 200) yield Put("ref1", i, "value" + i))
  }
}

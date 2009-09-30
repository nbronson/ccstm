/* CCSTM - (c) 2009 Stanford University - PPL */

// WriteBufferSuite

package edu.stanford.ppl.ccstm.impl


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

  def nanosPerGet(size: Int, passes: Int, numReadHits: Int, numReadMisses: Int): Double = {
    val wb = new WriteBuffer
    for (i <- 0 until size) wb.put("ref", i, "x" + i)

    var best = Math.MAX_LONG
    for (p <- 0 until passes) {
      val t0 = System.nanoTime
      var k = 0
      var i = 0
      while (i < numReadHits) {
        wb.get("ref", k, "default")
        k += 1
        i += 1
        if (k == size) k = 0
      }
      k = size
      i = 0
      while (i < numReadMisses) {
        wb.get("ref", k, "default")
        k += 1
        i += 1
      }
      val elapsed = System.nanoTime - t0
      best = best min elapsed
    }

    best * 1.0 / (numReadHits + numReadMisses)
  }

  test("read performance") {
    for (size <- 0 until 64) {
      printf("%d entries -> %3.1f nanos/hit, %3.1f nanos/miss\n",
        size, nanosPerGet(size, 1000, 10000, 0), nanosPerGet(size, 1000, 0, 10000))
    }
  }
}

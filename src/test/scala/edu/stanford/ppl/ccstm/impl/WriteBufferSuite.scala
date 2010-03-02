/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TxnWriteBufferSuite

package edu.stanford.ppl.ccstm.impl


import java.util.IdentityHashMap
import scala.collection.mutable.HashMap
import org.scalatest.FunSuite
import edu.stanford.ppl.ExhaustiveTest


class WriteBufferSuite extends FunSuite {

  type WB = WriteBuffer

  /** Only needed for identity comparisons. */
  class H[T](val ref: AnyRef, val offset: Int) extends Handle[T] {
    private[ccstm] def meta: Long = 0L
    private[ccstm] def meta_=(v: Long) {}
    private[ccstm] def metaCAS(before: Long, after: Long): Boolean = false
    private[ccstm] def metaOffset = offset
    private[ccstm] def data: T = null.asInstanceOf[T]
    private[ccstm] def data_=(v: T) {}

    override def toString = {
      super.toString + "(ref=" + ref + ", offset=" + offset + ")"
    }

    override def equals(rhs: Any) = rhs match {
      case h: H[_] => (ref eq h.ref) && offset == h.offset
      case _ => false
    }
  }

  abstract class Step
  case class Put(specValue: Any, handle: Handle[_]) extends Step
  case class GetForPut(handle: Handle[_]) extends Step

  private class NestedMap extends IdentityHashMap[AnyRef,HashMap[Int,(Any,Handle[_])]] {
    def apply(ref: AnyRef) = {
      get(ref) match {
        case null => {
          val m = new HashMap[Int,(Any,Handle[_])]
          put(ref, m)
          m
        }
        case m => m
      }
    }
  }

  def runTest(steps: Iterable[Step], incrementalValidate: Boolean) {
    val wb = new WB
    val reference = new NestedMap
    val iter = steps.iterator
    while (iter.hasNext) {
      iter.next match {
        case Put(specValue, handle) => {
          val ref = handle.ref
          val offset = handle.offset
          val expected = reference(ref).getOrElse(offset, (null, null))._1
          val actual = wb.get(handle)
          assert(expected === actual)
          reference(ref)(offset) = Tuple2.apply[Any,Handle[_]](specValue, handle)
          wb.put(handle.asInstanceOf[Handle[Any]], specValue)
        }
        case GetForPut(handle) => {
          val ref = handle.ref
          val offset = handle.offset
          val expected = reference(ref).getOrElse(offset, (handle.data, null))._1
          reference(ref)(offset) = Tuple2.apply[Any,Handle[_]](expected, handle)
          val actual = wb.allocatingGet(handle)
          assert(expected === actual)
          val reget = wb.get(handle)
          assert(expected === reget)
        }
      }

      if (incrementalValidate || !iter.hasNext) {
        // validate the set
        val fresh = new NestedMap
        wb.visit(new WriteBuffer.Visitor {
          def visit(handle: Handle[_], specValue: Any): Boolean = {
            fresh(handle.ref)(handle.offset) = (specValue, handle)
            assert(wb.get(handle) == specValue)
            true
          }
        })
        assert(fresh.size == reference.size)
        for (ref <- fresh.keySet.toArray) {
          val f = fresh(ref)
          val r = reference(ref)
          assert(f.size === r.size)
          for (offset <- f.keySet) {
            assert(f(offset) === r(offset))
          }
        }
      }
    }
  }

  test("put, single entry") {
    runTest(List(
        Put("value", new H[String]("ref1", 0)),
        Put("value2", new H[String]("ref1", 0))
      ), true)
  }

  test("put, many refs") {
    runTest(for (i <- 0 until 200) yield {
      Put("value" + i, new H[String]("ref" + i, 0))
    }, true)
  }

  test("put, many offsets") {
    runTest(for (i <- 0 until 200) yield {
      Put("value" + i, new H[String]("ref1", i))
    }, true)
  }

  test("put, many many refs") {
    runTest(for (i <- 0 until 20000) yield {
      Put("value" + i, new H[String]("ref" + i, 0))
    }, false)
  }

  test("put, many many offsets") {
    runTest(for (i <- 0 until 20000) yield {
      Put("value" + i, new H[String]("ref1", i))
    }, false)
  }

  test("getForPut, single entry") {
    runTest(List(
        GetForPut(new H[String]("ref1", 0) { override def data = "value" }),
        GetForPut(new H[String]("ref1", 0) { override def data = "value2" })
      ), true)
  }

  test("getForPut, many refs") {
    runTest(for (i <- 0 until 200) yield {
      GetForPut(new H[String]("ref" + i, 0) { override def data = "value" + i })
    }, true)
  }

  test("getForPut, many offsets") {
    runTest(for (i <- 0 until 200) yield {
      GetForPut(new H[String]("ref1", i) { override def data = "value" + i })
    }, true)
  }

  test("getForPut, many many refs") {
    runTest(for (i <- 0 until 20000) yield {
      GetForPut(new H[String]("ref" + i, 0) { override def data = "value" + i })
    }, false)
  }

  test("getForPut, many many offsets") {
    runTest(for (i <- 0 until 20000) yield {
      GetForPut(new H[String]("ref1", i) { override def data = "value" + i })
    }, false)
  }

  def nanosPerGet(size: Int, passes: Int, numReadHits: Int, numReadMisses: Int): Double = {
    val refs = new Array[AnyRef](size * 2 + 2)
    for (i <- 0 until refs.length) {
      refs(i) = "ref" + i
    }

    val wb = new WB
    for (i <- 0 until size) {
      wb.put(new H[String](refs(i), 0), "x" + i)
    }

    var best = Long.MaxValue
    for (p <- 0 until passes) {
      val t0 = System.nanoTime
      var k = 0
      var i = 0
      while (i < numReadHits + numReadMisses) {
        wb.get(new H[String](refs(k + (if (i >= numReadMisses) size else 0)), 0))
        k += 1
        i += 1
        if (k >= size) k = 0
      }
      val elapsed = System.nanoTime - t0
      best = best min elapsed
    }

    best * 1.0 / (numReadHits + numReadMisses)
  }

  test("read performance", ExhaustiveTest) {
    for (size <- (0 until 8) ++ (8 until 16 by 2) ++ (16 until 32 by 4) ++ (32 to 256 by 16)) {
      printf("%d entries -> %3.1f nanos/hit, %3.1f nanos/miss\n",
        size, nanosPerGet(size, 1000, 10000, 0), nanosPerGet(size, 1000, 0, 10000))
    }
  }
}

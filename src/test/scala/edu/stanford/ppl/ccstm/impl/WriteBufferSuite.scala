/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TxnWriteBufferSuite

package edu.stanford.ppl.ccstm.impl


import java.util.IdentityHashMap
import scala.collection.mutable.HashMap
import org.scalatest.FunSuite
import edu.stanford.ppl.ExhaustiveTest


class WriteBufferSuite extends FunSuite {

  type WB = WriteBuffer
  type WBV = WriteBuffer.Visitor

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
        wb.visit(new WBV {
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

  def nanosPerOp(size: Int,
                 passes: Int,
                 numReadHits: Int,
                 numReadMisses: Int,
                 numWriteHits: Int): Double = {
    val refs = Array.tabulate[AnyRef](size * 2 + 1)({ i => "ref" + i })
    val handles = refs.map({ r => new H[String](r, 0) })

    val wb = new WB
    for (i <- 0 until size) wb.put(handles(i), "x")

    var best = Long.MaxValue
    for (p <- 0 until passes) {
      val t0 = System.nanoTime
      var k = 0
      var i = 0
      while (i < numReadHits + numReadMisses + numWriteHits) {
        if (i < numReadHits + numReadMisses) {
          val hit = i < numReadHits
          val x = wb.get(handles(k + (if (!hit) size else 0)))
          if ((null != x) != hit) {
            fail
          }
        } else {
          wb.put(handles(k), "x")
        }
        k += 1
        i += 1
        if (k >= size) k = 0
      }
      val elapsed = System.nanoTime - t0
      best = best min elapsed
    }

    best * 1.0 / (numReadHits + numReadMisses + numWriteHits)
  }

  test("read performance", ExhaustiveTest) {
    for (size <- (0 until 8) ++ (8 until 16 by 2) ++ (16 until 32 by 4) ++ (32 to 256 by 16)) {
      printf("%d entries -> %3.1f nanos/read hit, %3.1f nanos/read miss, %3.1f nanos/write hit\n",
        size,
        if (size == 0) 0.0 else nanosPerOp(size, 1000, 10000, 0, 0),
        nanosPerOp(size, 1000, 0, 10000, 0),
        if (size == 0) 0.0 else nanosPerOp(size, 1000, 0, 0, 10000))
    }
  }

  test("nested commit") {
    val ref1 = "ref1"
    val ref2 = "ref2"
    val h10 = new H[String](ref1, 0)
    val h11 = new H[String](ref1, 1)
    val h2 = new H[String](ref2, 0)
    val wb = new WB
    wb.put(h10, "outer")
    assert(wb.get(h10) === "outer")
    wb.push()
    assert(wb.get(h10) === "outer")
    wb.put(h10, "inner10")
    wb.put(h11, "inner11")
    assert(wb.get(h10) === "inner10")
    assert(wb.get(h11) === "inner11")
    wb.popWithCommit()
    assert(wb.get(h10) === "inner10")
    assert(wb.get(h11) === "inner11")
    wb.put(h2, "outer2")
    assert(wb.get(h2) === "outer2")
  }

  test("nested rollback") {
    val ref1 = "ref1"
    val ref2 = "ref2"
    val h10 = new H[String](ref1, 0)
    val h11 = new H[String](ref1, 1)
    val h2 = new H[String](ref2, 0)
    val wb = new WB
    wb.put(h10, "outer")
    assert(wb.get(h10) === "outer")
    wb.push()
    assert(wb.get(h10) === "outer")
    wb.put(h10, "inner10")
    wb.put(h11, "inner11")
    assert(wb.get(h10) === "inner10")
    assert(wb.get(h11) === "inner11")
    val accum = new ReadSetBuilder()
    wb.accumulateLevel(accum)
    val rs = accum.result()
    assert(rs.size == 1)
    assert(rs.indexEnd == 1)
    assert(rs.handle(0) eq h11)
    wb.popWithRollback()
    assert(wb.get(h10) === "outer")
    assert(wb.get(h11) eq null)
    wb.put(h2, "outer2")
    assert(wb.get(h2) === "outer2")
  }
}
